#!/usr/bin/perl -w
# -- extract cohort information from csv with data form cohorts.sql query
# -- reads STDIN, relies on a header and a table sorted last by the measurement
#    variable (so that matched groups are in subsequent lines)
# -- needs a column called "vaccinated" and can use columns called
#    "week" and "year"
# -- also needs a column called "ili" and a column called "non_ili"
# -- every column right of the "vaccinated" column will be ignored for the
#    group matching (these columns should contain the data)

use strict;
use warnings;
use Getopt::Long;
use DBI;

sub min {
    [$_[0], $_[1]]->[$_[0] >= $_[1]];
}

my $motionchart = 0; # motion chart format
my $variable_file = "variables"; # file containing all the variables
my $measure = "vaccinated"; # measurement variable
my $timestring = "year,week"; # time variable(s)
my $controlstring = "agegroup,risk,children"; # match variable(s)
my $definition = "";
my @options = ("unvaccinated", "vaccianted");

GetOptions(
    "chart|t" => \$motionchart,
    "definition=s" => \$definition,
    "measure|m=s" => \$measure,
    "time=s" => \$timestring,
    "control|o=s" => \$controlstring,
    "variable-file|f=s" => \$variable_file
);

# extract control and measurement variables
my %control;
my @control_vars = split(/,/, $controlstring);
foreach (@control_vars) {
    $control{$_} = 1;
}
my %time;
my @time_vars = split(/,/, $timestring);
foreach (@time_vars) {
    $time{$_} = 1;
}

# compose database string

my $selectstring = "SELECT count(*) AS participants, ".
    "count(ili) AS ili, ".
    "count(non_ili) AS non_ili";
my $fromstring = "FROM (SELECT NULLIF(S.status = 'ILI', false) AS ili, ".
    "NULLIF(S.status != 'ILI', false) AS non_ili";

my %sections;
my $section = "";

# read in variables file
open (IN, "$variable_file") or
    die "Could not open variable file $variable_file\n";

while (<IN>) {
    if (/^#/) { next; }
    chomp;
    if (/\[(.+)\]/) {
	$section = $1;
	$sections{$section}{"select"} = $section;
	$sections{$section}{"db"} = "";
	$sections{$section}{"cases"} = [];
    } elsif (/select=(.+)/) {
	$sections{$section}{"select"} = $1;
    } elsif (/db=([^\s]+)/) {
	$sections{$section}{"db"} = $1;
    } elsif (/[,=]/) {
	push @{$sections{$section}{"cases"}}, $_;
    }
}

undef $section;

close(IN);

my %outcomes;

foreach my $section ((@time_vars,@control_vars,$measure)) {

    my $select = $sections{$section}{"select"};
    my $db = $sections{$section}{"db"};

    if (scalar (@{$sections{$section}{"cases"}}) == 0) {
	if ($select =~ /\(/) {
	    $selectstring .= ", $section AS $section";
	    $fromstring .= ", $select AS $section";
	} else {
	    $fromstring .= ", $db.\"$select\" AS $select";
	    $selectstring .= ", $select AS $section";
	}
    } else {
	$selectstring .= ", CASE";
	my @variable_names;
	if ($select =~ /\(/) {
	    # if section contains paranthese it's an expression
	    # so we make the section the variable name
	    $variable_names[0] = $section;
	    $fromstring .= ", $select AS $section";
	} else {
	    @variable_names = split(/,/, $select);
	    foreach (@variable_names) {
		$fromstring .= ", $db.\"$_\" AS $_";
	    }
	}
	foreach (@{$sections{$section}{"cases"}}) {
	    if (/ranges=([0-9,]+)/) {
		my @splits = split(/,/, $1);
		my $nsplits = (scalar @splits) - 1;
		for (my $i = 0; $i < $nsplits; $i++) {
		    $selectstring .= " WHEN $section >= $splits[$i] ".
			"AND $section < $splits[$i+1] THEN ".
			    "'$splits[$i]..$splits[$i+1]'";
		    $outcomes{$section}{"$splits[$i]..$splits[$i+1]"} = 1
		}
		$selectstring .= " WHEN $section >= $splits[$nsplits]".
		    " THEN '$splits[$nsplits]+'";
	    } else {
		my @line = split(/,/);
		my $logical = $line[0];
		if ($logical eq "else") {
		    $selectstring .= " ELSE $line[1]";
		} else {
		    $selectstring .= " WHEN ";
		    my @strarray = ($line[0] =~ m/./g);
		    my $current;
		    my $counter = 0;
		    while (scalar(@strarray) > 0) {
			my $char = shift(@strarray);
			if ($char =~ /[\|&]/) {
			    $selectstring .=
				"$variable_names[$counter] = $current";
			    if ($char eq "|") {
				$selectstring .= " OR ";
			    } elsif ($char eq "&") {
				$selectstring .= " AND ";
			    }
			    $counter++;
			    $current = "";
			} else {
			    $current .= "$char";
			}
		    }
		    if ($current =~ /[^\s]/) {
			$selectstring .=
			    "$variable_names[$counter] = $current";
		    }
		    $selectstring .= " THEN $line[1]";
		}
		$outcomes{$section}{$line[1]} = $line[2];
	    }
	}
	$selectstring .= " END AS $section";
    }
}

$fromstring .= " FROM epidb_results_intake AS I, epidb_health_status";
if ($definition ne "") {
    $fromstring .= "_$definition";
}
$fromstring .= " AS S, epidb_results_weekly AS W";
$fromstring .= " WHERE I.\"Q10\"<2".
    " AND S.epidb_results_weekly_id = W.id".
    " AND (W.\"Q2\" IS NULL OR W.\"Q2\" != 0)".
    " AND W.global_id = I.global_id".
    ") AS statuses";

my $sqlstring = "$selectstring $fromstring".
    " GROUP BY $timestring,$controlstring,$measure".
    " ORDER BY $timestring,$controlstring,$measure";

my $dbh = DBI->connect ( "dbi:Pg:dbname=flusurvey", "", "");
if ( !defined $dbh ) {
    die "Cannot connect to database!\n";
}

print "$sqlstring\n";

my $sth = $dbh->prepare($sqlstring);
$sth->execute();

my $count = -1;
my $sum = 0;
my $nfail = 0;

# variables to store counts

my %ili;
my %nonili;
my @matches;
my @current_controls = "init" x (scalar @time_vars + scalar @control_vars);

my $first = 0;

while (my @row = $sth->fetchrow_array() ) {
    print "@row\n";
    my $fail = 0;
    foreach (@row) {
	if (!(defined $_)) {
	    $fail = 1;
	}
    }
    if (!$fail) {
	# see if we've got a new combination of control groups
	# (which should be followed by at least one matching group)
	my $matching_group = 1;
	for (my $i = 3; $i < (scalar @time_vars + scalar @control_vars); $i++) {
	    if (!("$row[$i]" eq "$current_controls[$i]")) {
		$matching_group = 0;
	    }
	}

	if ($matching_group) {
	    my @new_measure;
	    for (my $i = 5 + (scalar @control_vars);
		 $i < 5 + (scalar @control_vars) + (scalar @time_vars);
		 $i++) {
		push @new_measure, $row[$i];
	    }
	    push @matches, \@new_measure;
	} else {
	    	my $year = $row[3];
	my $week = $row[4];

	if (!(exists $ili{$year}{$week})) {
	    $ili{$year}{$week} = {};
	}
	if (!(exists $nonili{$year}{$week})) {
	    $nonili{$year}{$week} = {};
	}

	    }


	my @vaccinated = @nextdata;
		my $vaccinated_total =
		    $vaccinated[$ili_index] + $vaccinated[$nonili_index];
		my $unvaccinated_total =
		    $unvaccinated[$ili_index] + $unvaccinated[$nonili_index];
		my $smaller_total = min($vaccinated_total,
					$unvaccinated_total);
		if ($vaccinated_total > 0 && $unvaccinated_total > 0) {
			$unvaccinated_ili{$year}{$week} +=
			    $unvaccinated[$ili_index] *
				$smaller_total / $unvaccinated_total;
			$unvaccinated_nonili{$year}{$week} +=
			    $unvaccinated[$nonili_index] *
				$smaller_total / $unvaccinated_total;
			$vaccinated_ili{$year}{$week} +=
			    $vaccinated[$ili_index] *
				$smaller_total / $vaccinated_total;
			$vaccinated_nonili{$year}{$week} +=
			    $vaccinated[$nonili_index] *
				$smaller_total / $vaccinated_total;
		    }
		}
		my $line = <STDIN>;
		if ($line) {
		    chomp $line;
		    @nextdata = split /,/, $line;
		}
	    }
	}
    }
}

    } else {
	$nfail++;
    }
}

# clean up
$dbh->disconnect();

my @categories;
my @vaccinated_data;
my @unvaccinated_data;
if ($countries) {
    foreach my $country (sort keys %vaccinated_ili) {
	foreach my $year (sort keys %{ $vaccinated_ili{$country} }) {
	    foreach my $week (sort {$a <=> $b} keys %{ $vaccinated_ili{$country}{$year} }) {
		if ($vaccinated_ili{$country}{$year}{$week} +
			$vaccinated_nonili{$country}{$year}{$week} > 0 &&
			    $unvaccinated_ili{$country}{$year}{$week} +
				$unvaccinated_nonili{$country}{$year}{$week} >
				    0) {
		    push @categories, "$country,$year,$week";
		    push @vaccinated_data,
			sprintf("%.1f", ($vaccinated_ili{$country}{$year}{$week} * 100 /
					     ($vaccinated_ili{$country}{$year}{$week} +
						  $vaccinated_nonili{$country}{$year}{$week}
						      + .0)));
		    push @unvaccinated_data,
			sprintf("%.1f", ($unvaccinated_ili{$country}{$year}{$week} * 100 /
					     ($unvaccinated_ili{$country}{$year}{$week} +
						  $unvaccinated_nonili{$country}{$year}{$week}
						      + .0)));
		}
	    }
	}
    }
} else {
    foreach my $year (sort keys %vaccinated_ili) {
	foreach my $week (sort {$a <=> $b} keys %{ $vaccinated_ili{$year} }) {
	    if ($vaccinated_ili{$year}{$week} +
		    $vaccinated_nonili{$year}{$week} > 0 &&
			$unvaccinated_ili{$year}{$week} +
			    $unvaccinated_nonili{$year}{$week} >
				0) {
		if ($motionchart) {
		    push @categories, "'$week/$year'";
		} else {
		    push @categories, "$year,$week";
		}
		push @vaccinated_data,
		sprintf("%.1f", ($vaccinated_ili{$year}{$week} * 100 /
				     ($vaccinated_ili{$year}{$week} +
					  $vaccinated_nonili{$year}{$week} + .0)));
		push @unvaccinated_data,
		    sprintf("%.1f", ($unvaccinated_ili{$year}{$week} * 100 /
					 ($unvaccinated_ili{$year}{$week} +
					      $unvaccinated_nonili{$year}{$week} + .0)));
	    }
	}
    }
}

if ($motionchart) {
    foreach (@categories) {
	$_ = "'$_'";
    }
    print "<html>\n";
    print "  <head>\n";
    print "    <script type=\"text/javascript\" ".
	"src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.6.1/".
	    "jquery.min.js\"></script>\n";
    print "    <script type=\"text/javascript\" ".
	"src=\"/media/highcharts/highcharts.js\"></script>\n";
    print "    <script type=\"text/javascript\">\n";
    print "            var chart;\n";
    print "            \$(document).ready(function() {\n";
    print "                    chart = new Highcharts.Chart({\n";
    print "                            chart: {\n";
    print "                                    renderTo: 'container',\n";
    print "                                    defaultSeriesType: 'line',\n";
    print "                                    marginRight: 0,\n";
    print "                                    marginBottom: 70\n";
    print "                            },\n";
    print "                            title: {\n";
    print "                                    text: 'Incidence in vaccinated/unvaccinated groups',\n";
    print "                                    x: -20 //center\n";
    print "                            },\n";
    print "                            xAxis: {\n";
    print "                                    categories: [".join(',', @categories)."],\n";
    print "                                    labels: {\n";
    print "                                            rotation: -45,\n";
    print "                                            align: 'right'\n";
    print "                                    }\n";
    print "                            },\n";
    print "                            yAxis: {\n";
    print "                                    title: {\n";
    print "                                            text: 'Incidence (in %)'\n";
    print "                                    },\n";
    print "                                    plotLines: [{\n";
    print "                                            value: 0,\n";
    print "                                            width: 1,\n";
    print "                                            color: '#808080'\n";
    print "                                    }]\n";
    print "                            },\n";
    print "                            tooltip: {\n";
    print "                                    formatter: function() {\n";
    print "                                    return '<b>'+ this.series.name +'</b><br/>'+\n";
    print "                                                    'Incidence in week ' + this.x +': '+ this.y + '%';\n";
    print "                                    }\n";
    print "                            },\n";
    print "                            legend: {\n";
    print "                                    layout: 'vertical',\n";
    print "                                    align: 'right',\n";
    print "                                    verticalAlign: 'top',\n";
    print "                                    x: -10,\n";
    print "                                    y: 100,\n";
    print "                                    borderWidth: 0,\n";
    print "                                    enabled: false\n";
    print "                            },\n";
    print "                            series: [{\n";
    print "                                    name: 'Vaccinated',\n";
    print "                                    data: [".join(",", @vaccinated_data)."]\n";
    print "                            }, {\n";
    print "                                    name: 'Unvaccinated',\n";
    print "                                    data: [".join(",", @unvaccinated_data)."]\n";
    print "                            }]\n";
    print "                                });\n";
    print "                        });\n";
    print "    </script>\n";
    print "  </head>\n";
    print "  <body>\n";
    print "    <div id=\"container\" style=\"width: 550px; height: 300px; margin: 0 auto\"></div>\n";
    print "  </body>\n";
    print "</html>\n";
} else {
    if ($countries) {
	print "country,";
    }
    print "year,week,variable,value\n";
    for (my $i = 0; $i < (scalar @categories); $i++) {
	print "$categories[$i],vaccinated,$vaccinated_data[$i]\n";
	print "$categories[$i],unvaccinated,$unvaccinated_data[$i]\n";
    }
}

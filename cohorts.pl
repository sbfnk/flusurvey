#!/usr/bin/perl -w
#
# cohorts.pl
#
# queries the epidb database to match cohorts according to a specified set of matching
# variables, time variables and a measurement variable
#
# options:
#
#   -b, --dbname: name of the local database to query (containing the epidb tables);
#                 default is "flusurvey"
#   -t, --chart: create a motionchart HTML file (as opposed to a CSV table);
#                default is to create a table
#   -d, --definition: the ILI definition to use; by default the script gets ILI status
#                     from epidb_health_status; if a string "str" is given here, it uses
#                     epidb_health_status_str; e.g., "-d fever" will make the script use
#                     epidb_health_status_fever for the ILI definition. The corresponding
#                     table needs to be created separately in the SQL database;
#                     default is no string, that is to use epidb_health_status
#   -m, --measure: the measurement variable, e.g. vaccinated, transport
#                  default is "vaccinated"
#   -t, --time: the time variables, e.g. "year,week" or "year" or "none" (for aggregate figures)
#               default is "year,week"
#   -o, --control: the control variables, that is what is matched in cohorts
#                  default is "agegroup,risk,children"
#   -f, --variable-file: the file defining the different measurement, time and control variables
#                        and relate them to columns in the database; see the "variables" file for
#                        an example
#                        default is "variables"

use strict;
use warnings;
use Getopt::Long;
use DBI;

# subroutine for getting the minimum of two values
sub min {
    [$_[0], $_[1]]->[$_[0] >= $_[1]];
}

my $motionchart = 0; # motion chart format
my $variable_file = "variables"; # file containing all the variables
my $measure = "vaccinated"; # measurement variable
my $timestring = "year,week"; # time variable(s)
my $controlstring = "agegroup,risk,children"; # matched control variable(s)
my $dbname = "flusurvey"; # database name
my $definition = ""; # ILI definition to use

# get command line options
GetOptions(
    "chart|t" => \$motionchart,
    "definition=s" => \$definition,
    "measure|m=s" => \$measure,
    "time=s" => \$timestring,
    "control|o=s" => \$controlstring,
    "variable-file|f=s" => \$variable_file,
    "db|b" => \$dbname
);

# extract control and measurement variables
my @control_vars = split(/,/, $controlstring);
my @time_vars;
if ($timestring ne "none") {
    @time_vars = split(/,/, $timestring);
}

# compose database string
my $selectstring = "SELECT count(ili) AS ili, ".
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

my %outcomes; # the different outcome strings from the variables file

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

	my $index = 0;
	if ($select =~ /\(/) {
	    $variable_names[0] = "$section\_var$index";
	    # if section contains paranthese it's an expression
	    # so we create a new variable name
	    $fromstring .=
		", $select AS $variable_names[0]";
	} else {
	    @variable_names = split(/,/, $select);
	    foreach my $variable (@variable_names) {
		$fromstring .=
		    ", $db.\"$variable\" AS $variable";
	    }
	}

	$index = 0;
	foreach (@{$sections{$section}{"cases"}}) {
	    if (/ranges=([0-9,]+)/) {
		my @splits = split(/,/, $1);
		my $nsplits = (scalar @splits) - 1;
		for (my $i = 0; $i < $nsplits; $i++) {
		    $selectstring .= " WHEN $variable_names[$index] >= $splits[$i] ".
			"AND $variable_names[$index] < $splits[$i+1] THEN ".
			    "'$splits[$i]..$splits[$i+1]'";
		    $outcomes{$section}{"$splits[$i]..$splits[$i+1]"} = 1
		}
		$selectstring .= " WHEN $variable_names[$index] >= $splits[$nsplits]".
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
	$index++;
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

# connect to database
my $dbh = DBI->connect ( "dbi:Pg:dbname=$dbname", "", "");
if ( !defined $dbh ) {
    die "Cannot connect to database!\n";
}

my $sth = $dbh->prepare($sqlstring);
$sth->execute();

my $count = -1;
my $sum = 0;
my $nfail = 0;

# variables to store counts
my %matched_ili;
my %matched_nonili;
my @current_time = ("init") x (scalar @time_vars);
my @current_controls = ("init") x (scalar @control_vars);
my $measure_column = scalar @time_vars + scalar @control_vars + 2;
my %measure_range;
my %measure_times; # to preserve sorting order
my $index = 0;

# go through db output and extract values
while (my @row = $sth->fetchrow_array() ) {
    my $fail = 0;
    foreach (@row) {
	if (!(defined $_)) {
	    $fail = 1;
	}
    }
    if (!$fail) {
	my $current_time = join(",", @row[2..(2 + (scalar @current_time) - 1)]);
	my $current_controls =
	    join(",",  @row[(2 + (scalar @current_time))..
				(2 + (scalar @current_time +
					  scalar @current_controls) - 1)]);
	$measure_range{$row[$measure_column]} = 1;
	$matched_ili{$current_time}{$current_controls}{$row[$measure_column]} = $row[0];
	$matched_nonili{$current_time}{$current_controls}{$row[$measure_column]} = $row[1];
	$measure_times{$current_time} = $index;
    } else {
	$nfail++;
    }
    $index++;
}

$dbh->disconnect();

my %ili;
my %nonili;
my @categories;

# match cohorts
foreach my $time (sort {$measure_times{$a} <=> $measure_times{$b}}
		   keys %matched_ili) {

    foreach my $control (keys %{ $matched_ili{$time} }) {
	my $valid = 1;
	my %total;
	my $smallest_total = -1;
	foreach (keys %measure_range) {
	    if (exists $matched_ili{$time}{$control}{$_} &&
		    exists $matched_nonili{$time}{$control}{$_}) {
		$total{$_} =
		    $matched_ili{$time}{$control}{$_} +
			$matched_nonili{$time}{$control}{$_};
		if ($smallest_total < 0) {
		    $smallest_total = $total{$_};
		} else {
		    $smallest_total = min($total{$_}, $smallest_total);
		}
	    } else {
		$valid = 0;
	    }
	}
	if ($valid == 1) {
	    foreach (keys %measure_range) {
		if (!defined $ili{$time}{$_}) {
		    $ili{$time}{$_} = 0;
		}
		$ili{$time}{$_} += $matched_ili{$time}{$control}{$_} *
		    $smallest_total / $total{$_};
		$nonili{$time}{$_} += $matched_nonili{$time}{$control}{$_} *
		    $smallest_total / $total{$_};
	    }
	}
    }
}

# find times at which we have data
foreach my $time (keys %ili) {
    if (scalar keys %{ $ili{$time} } > 0) {
	push @categories, $time;
    }
}

if ($motionchart) {
    my %data;
    foreach (keys %measure_range) {
	my @new_array;
	$data{$_} = \@new_array;
    }
    my @dates;
    foreach my $time (sort {$measure_times{$a} <=> $measure_times{$b}}
				@categories) {
	my @date = split(/,/, $time);
	my $datestr = "'" . pop(@date);
	foreach (reverse @date) {
	    $datestr .= "/$_";
	}
	$datestr .= "'";
	push @dates, $datestr;
	foreach (keys %measure_range) {
	    push @ {$data{$_}}, 
		sprintf("%.1f", ($ili{$time}{$_} * 100 /
 				     ($ili{$time}{$_} +
 					  $nonili{$time}{$_} + .0)));
	}
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
    print "                                    categories: [".join(',', @dates)."],\n";
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
    my $first = 1;
    foreach (keys %measure_range) {
	if (!($first == 1)) {
	    print "                            }, {\n";
	} else {
	    $first = 0;
	}
	print "                                    name: ".$outcomes{$measure}{$_}.",\n";
	print "                                    data: [".join(",", @{$data{$_}})."]\n";
    }
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
    print join(",", @time_vars).",variable,value\n";
    foreach my $time (sort {$measure_times{$a} <=> $measure_times{$b}}
				@categories) {
	foreach (keys %measure_range) {
	    my $fraction = $ili{$time}{$_} * 100 /
		($ili{$time}{$_} +
		     $nonili{$time}{$_} + .0);
	    if (scalar @time_vars > 0) {
		print "$time,";
	    }
	    print "$outcomes{$measure}{$_},$fraction\n";
	}
    }
}

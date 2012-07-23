#!/usr/bin/perl -w
# -- extract cohort information from csv with data form cohorts.sql query
# -- reads STDIN, relies on a header and a table sorted last by transport
#    (so that matched groups are in subsequent lines)
# -- needs a column called "transport" and can use columns called
#    "week" and "year"
# -- also needs a column called "ili" and a column called "non_ili"
# -- every column right of the "transport" column will be ignored for the
#    group matching (these columns should contain the data)

use strict;

sub min {
    [$_[0], $_[1]]->[$_[0] >= $_[1]];
}

my $motionchart = 0; # motion chart format
my $countries = 0; # separate countries (otherwise they are matched)

foreach (@ARGV) {
    if ($_ eq "-m") {
	$motionchart = 1;
    }
    if ($_ eq "-c") {
	$countries = 1;
    }
}

# read header line
my $header_line;
if (!($header_line = <STDIN>)) {
    die "No header line\n";
}
chomp($header_line);
my @header = split /,/, $header_line;
my $country_index = -1; # index of "country" column
my $transport_index = -1; # index of "transport" column
my $week_index = -1; # index of "week" column
my $year_index = -1; # index of "year" column
my $yearweek_index = -1; # index of "year-week" column
my $ili_index = -1; # index of "ili" coulmn
my $nonili_index = -1; # index of "nonili" coulmn
my $count = -1;
my $sum = 0;

# find where the relevant columns are
for (my $i = 0; $i < scalar(@header); $i++) {
    if ("$header[$i]" eq "country") {
	$country_index = $i;
    }
    if ("$header[$i]" eq "transport") {
	$transport_index = $i;
    }
    if ("$header[$i]" eq "week") {
	$week_index = $i;
    }
    if ("$header[$i]" eq "year") {
	$year_index = $i;
    }
    if ("$header[$i]" eq "year-week") {
	$yearweek_index = $i;
    }
    if ("$header[$i]" eq "ili") {
	$ili_index = $i;
    }
    if ("$header[$i]" eq "non_ili") {
	$nonili_index = $i;
    }
}

if ($countries && $country_index == -1) {
    die "Input has no \"country\" column\n";
}
if ($transport_index == -1) {
    die "Input has no \"transport\" column\n";
}
if ($ili_index == -1) {
    die "Input has no \"ili\" column\n";
}
if ($nonili_index == -1) {
    die "Input has no \"non_ili\" column\n";
}
if ((($year_index == -1) || ($week_index == -1)) &&
	$yearweek_index == -1) {
    die "Input has no year/week columns\n";
}
if ($countries && $motionchart) {
    die "Cannot split motionchart by countries\n";
}
my @data; # will store the data read in from csv
my @nextdata; # will store one readahead from csv
if (!(@nextdata = split /,/, <STDIN>)) {
    die "No data\n";
}

# variables to store counts of transport/notransport
my %transport_ili;
my %notransport_ili;
my %transport_nonili;
my %notransport_nonili;

while (<STDIN>) {
    @data = @nextdata;
    chomp;
    @nextdata = split /,/;

    my $week = 0;
    my $year = 0;

    if ($week_index >= 0 && $year_index >= 0) {
	#  we have got a "week" and "year" column
	$week = $data[$week_index];
	$year = $data[$year_index];
    } else {
	#  we have got a "year-week" column
	($year, $week) = split /-/, $data[$yearweek_index];
    }
    if ($countries) {
	my $country = $data[$country_index];
	if (!(exists $transport_ili{$country}{$year}{$week})) {
	    $transport_ili{$country}{$year}{$week} = 0;
	}
	if (!(exists $transport_nonili{$country}{$year}{$week})) {
	    $transport_nonili{$country}{$year}{$week} = 0;
	}
	if (!(exists $notransport_ili{$country}{$year}{$week})) {
	    $notransport_ili{$country}{$year}{$week} = 0;
	}
	if (!(exists $notransport_nonili{$country}{$year}{$week})) {
	    $notransport_nonili{$country}{$year}{$week} = 0;
	}
    } else {
	if (!(exists $transport_ili{$year}{$week})) {
	    $transport_ili{$year}{$week} = 0;
	}
	if (!(exists $transport_nonili{$year}{$week})) {
	    $transport_nonili{$year}{$week} = 0;
	}
	if (!(exists $notransport_ili{$year}{$week})) {
	    $notransport_ili{$year}{$week} = 0;
	}
	if (!(exists $notransport_nonili{$year}{$week})) {
	    $notransport_nonili{$year}{$week} = 0;
	}
    }

    # see if we're a nontransport group
    # (which should be followed by a matching transport group)
    if ($data[$transport_index] == 0) {
	my @notransport = @data;

	if (scalar (@nextdata) > $transport_index) {
	    my $matching_group = 0;
	    if ($nextdata[$transport_index] == 1) {
		$matching_group = 1;
		for (my $i = 0; $i < $transport_index; $i++) {
		    if (!("$nextdata[$i]" eq "$notransport[$i]")) {
			$matching_group = 0;
		    }
		}
	    }
	    if ($matching_group) {
		my @transport = @nextdata;
		my $transport_total =
		    $transport[$ili_index] + $transport[$nonili_index];
		my $notransport_total =
		    $notransport[$ili_index] + $notransport[$nonili_index];
		my $smaller_total = min($transport_total,
					$notransport_total);
		if ($transport_total > 0 && $notransport_total > 0) {
		    if ($countries) {
			my $country = $transport[$country_index];
			$notransport_ili{$country}{$year}{$week} +=
			    $notransport[$ili_index] *
				$smaller_total / $notransport_total;
			$notransport_nonili{$country}{$year}{$week} +=
			    $notransport[$nonili_index] *
				$smaller_total / $notransport_total;
			$transport_ili{$country}{$year}{$week} +=
			    $transport[$ili_index] *
				$smaller_total / $transport_total;
			$transport_nonili{$country}{$year}{$week} +=
			    $transport[$nonili_index] *
				$smaller_total / $transport_total;
		    } else {
			$notransport_ili{$year}{$week} +=
			    $notransport[$ili_index] *
				$smaller_total / $notransport_total;
			$notransport_nonili{$year}{$week} +=
			    $notransport[$nonili_index] *
				$smaller_total / $notransport_total;
			$transport_ili{$year}{$week} +=
			    $transport[$ili_index] *
				$smaller_total / $transport_total;
			$transport_nonili{$year}{$week} +=
			    $transport[$nonili_index] *
				$smaller_total / $transport_total;
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

my @categories;
my @transport_data;
my @notransport_data;
if ($countries) {
    foreach my $country (sort keys %transport_ili) {
	foreach my $year (sort keys %{ $transport_ili{$country} }) {
	    foreach my $week (sort {$a <=> $b} keys %{ $transport_ili{$country}{$year} }) {
		if ($transport_ili{$country}{$year}{$week} +
			$transport_nonili{$country}{$year}{$week} > 0 &&
			    $notransport_ili{$country}{$year}{$week} +
				$notransport_nonili{$country}{$year}{$week} >
				    0) {
		    push @categories, "$country,$year,$week";
		    push @transport_data,
			sprintf("%.1f", ($transport_ili{$country}{$year}{$week} * 100 /
					     ($transport_ili{$country}{$year}{$week} +
						  $transport_nonili{$country}{$year}{$week}
						      + .0)));
		    push @notransport_data,
			sprintf("%.1f", ($notransport_ili{$country}{$year}{$week} * 100 /
					     ($notransport_ili{$country}{$year}{$week} +
						  $notransport_nonili{$country}{$year}{$week}
						      + .0)));
		}
	    }
	}
    }
} else {
    foreach my $year (sort keys %transport_ili) {
	foreach my $week (sort {$a <=> $b} keys %{ $transport_ili{$year} }) {
	    if ($transport_ili{$year}{$week} +
		    $transport_nonili{$year}{$week} > 0 &&
			$notransport_ili{$year}{$week} +
			    $notransport_nonili{$year}{$week} >
				0) {
		if ($motionchart) {
		    push @categories, "'$week/$year'";
		} else {
		    push @categories, "$year,$week";
		}
		push @transport_data,
		sprintf("%.1f", ($transport_ili{$year}{$week} * 100 /
				     ($transport_ili{$year}{$week} +
					  $transport_nonili{$year}{$week} + .0)));
		push @notransport_data,
		    sprintf("%.1f", ($notransport_ili{$year}{$week} * 100 /
					 ($notransport_ili{$year}{$week} +
					      $notransport_nonili{$year}{$week} + .0)));
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
    print "            var chart_transport;\n";
    print "            \$(document).ready(function() {\n";
    print "                    chart_transport = new Highcharts.Chart({\n";
    print "                            chart: {\n";
    print "                                    renderTo: 'container_transport',\n";
    print "                                    defaultSeriesType: 'line',\n";
    print "                                    marginRight: 0,\n";
    print "                                    marginBottom: 70\n";
    print "                            },\n";
    print "                            title: {\n";
    print "                                    text: 'Incidence and public transport use',\n";
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
    print "                                    name: 'Not using public transport',\n";
    print "                                    data: [".join(",", @transport_data)."]\n";
    print "                            }, {\n";
    print "                                    name: 'Using public transport',\n";
    print "                                    data: [".join(",", @notransport_data)."]\n";
    print "                            }]\n";
    print "                                });\n";
    print "                        });\n";
    print "    </script>\n";
    print "  </head>\n";
    print "  <body>\n";
    print "    <div id=\"container_transport\" style=\"width: 550px; height: 300px; margin: 0 auto\"></div>\n";
    print "  </body>\n";
    print "</html>\n";
   } else {
    if ($countries) {
	print "country,";
    }
    print "year,week,variable,value\n";
    for (my $i = 0; $i < (scalar @categories); $i++) {
	print "$categories[$i],transport,$transport_data[$i]\n";
	print "$categories[$i],notransport,$notransport_data[$i]\n";
    }
}

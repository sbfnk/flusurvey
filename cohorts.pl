#!/usr/bin/perl -w
# -- extract cohort information from csv with data form cohorts.sql query
# -- reads STDIN, relies on a header and a table sorted last by vaccination
#    (so that matched groups are in subsequent lines)
# -- needs a column called "vaccinated" and can use columns called
#    "week" and "year"
# -- also needs a column called "ili" and a column called "non_ili"
# -- every column right of the "vaccinated" column will be ignored for the
#    group matching (these columns should contain the data)

use strict;

sub min {
    [$_[0], $_[1]]->[$_[0] >= $_[1]];
}

my $motionchart = 0; # motion chart format

foreach (@ARGV) {
    if ($_ eq "-m") {
	$motionchart = 1;
    }
}

# read header line
my $header_line = <STDIN>;
chomp($header_line);
my @header = split /,/, $header_line;
my $vaccinated_index = -1; # index of "vaccinated" column
my $week_index = -1; # index of "week" column
my $year_index = -1; # index of "year" column
my $ili_index = -1; # index of "ili" coulmn
my $nonili_index = -1; # index of "nonili" coulmn
my $count = -1;

# find where the relevant columns are
for (my $i = 0; $i < scalar(@header); $i++) {
    if ("$header[$i]" eq "vaccinated") {
	$vaccinated_index = $i;
    }
    if ("$header[$i]" eq "week") {
	$week_index = $i;
    }
    if ("$header[$i]" eq "year") {
	$year_index = $i;
    }
    if ("$header[$i]" eq "ili") {
	$ili_index = $i;
    }
    if ("$header[$i]" eq "non_ili") {
	$nonili_index = $i;
    }
}

if ($vaccinated_index == -1) {
    die "Input has no \"vaccinated\" column\n";
}
if ($ili_index == -1) {
    die "Input has no \"ili\" column\n";
}
if ($nonili_index == -1) {
    die "Input has no \"non_ili\" column\n";
}

my @data; # will store the data read in from csv

# read data
while (<STDIN>) {
    chomp;
    my @line = split /,/;
    push @data, \@line;
}

# data row
my $index = 0;
# variables to store counts of vaccinated/unvaccinated
my %vaccinated_ili;
my %vaccinated_nonili;
my %unvaccinated_ili;
my %unvaccinated_nonili;

while ($index < scalar( @data )) {
    my $week = 0;
    my $year = 0;

    # see if we have got a "week" and "year" column
    if ($week_index >= 0 && $year_index >= 0) {
	$week = $data[$index][$week_index];
	$year = $data[$index][$year_index];
	if (!(exists $vaccinated_ili{$year}{$week})) {
	    $vaccinated_ili{$year}{$week} = 0;
	}
	if (!(exists $vaccinated_nonili{$year}{$week})) {
	    $vaccinated_nonili{$year}{$week} = 0;
	}
	if (!(exists $unvaccinated_ili{$year}{$week})) {
	    $unvaccinated_ili{$year}{$week} = 0;
	}
	if (!(exists $unvaccinated_nonili{$year}{$week})) {
	    $unvaccinated_nonili{$year}{$week} = 0;
	}
    }
    # see if we're an unvaccinated group
    # (which should be followed by a matching vaccinated group)
    if ($data[$index][$vaccinated_index] == 0) {
	my @unvaccinated = @{ $data[$index] };
	$index++;
	my $matching_group = ($data[$index][$vaccinated_index] == 1);
	for (my $i = 0; $i < $vaccinated_index; $i++) {
	    $matching_group *= ("$data[$index][$i]" eq "$unvaccinated[$i]");
	}
	if ($matching_group) {
	    my @vaccinated = @{ $data[$index] };
	    my $vaccinated_total =
		$vaccinated[$ili_index] + $vaccinated[$nonili_index];
	    my $unvaccinated_total =
		$unvaccinated[$ili_index] + $unvaccinated[$nonili_index];
	    my $smaller_total = min($vaccinated_total, $unvaccinated_total);
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
	    $index++;
	}
    } else {
	$index++;
    }
}

if ($motionchart) {
    print "  <html>\n";
    print "  <head>\n";
    print "    <script type=\"text/javascript\" ".
	"src=\"https://www.google.com/jsapi\"></script>\n";
    print "    <script type=\"text/javascript\">\n";
    print "      google.load('visualization', '1', ".
	"{'packages':['motionchart']});\n";
    print "      google.setOnLoadCallback(drawChart);\n";
    print "      function drawChart() {\n";
    print "        var data = new google.visualization.DataTable();\n";
    print "        data.addColumn('string', 'Vaccination status');\n";
    print "        data.addColumn('string', 'Date');\n";
    print "        data.addColumn('number', 'Incidence');\n";
    print "        data.addRows([\n";
    foreach my $year (sort keys %vaccinated_ili) {
	foreach my $week (sort keys %{ $vaccinated_ili{$year} }) {
	    printf "          ['vaccinated','%d"."W"."%02d',%.2f],\n",
		$year, $week,
		    ($vaccinated_ili{$year}{$week} /
			 ($vaccinated_nonili{$year}{$week} + .0));
	    printf "          ['unvaccinated','%d"."W"."%02d',%.2f],\n",
		$year, $week,
		    ($unvaccinated_ili{$year}{$week} /
			 ($unvaccinated_nonili{$year}{$week} + .0));
	}
    }
    print "          ]);\n";
    print "        var chart = ".
	"new google.visualization.MotionChart(document.getElementById".
	    "('chart_div'));\n";
    print "        chart.draw(data, {width: 600, height:300});\n";
    print "      }\n";
    print "    </script>\n";
    print "  </head>\n\n";
    print "  <body>\n";
    print "    <div id=\"chart_div\" style=\"width: 600px; height: 300px;\">".
	"</div>\n";
    print "  </body>\n";
    print "</html>\n";
   } else {
    print "\"Vaccination status\",year,week,variable,value\n";
    foreach my $year (sort keys %vaccinated_ili) {
	foreach my $week (sort keys %{ $vaccinated_ili{$year} }) {
	    printf "vaccinated,ILI,%u,%.0f\n", $year, $week,
		$vaccinated_ili{$year}{$week};
	    printf "vaccinated,non-ILI,%u,%.0f\n", $year, $week,
		$vaccinated_nonili{$year}{$week};
	    printf "vaccinated,Incidence,%u,%.2f\n", $year, $week,
		($vaccinated_ili{$year}{$week} /
		     ($vaccinated_nonili{$year}{$week} + .0));
	    printf "unvaccinated,ILI,%u,%.0f\n", $year, $week,
		$unvaccinated_ili{$year}{$week};
	    printf "unvaccinated,non-ILI,%u,%.0f\n", $year, $week,
		$unvaccinated_nonili{$year}{$week};
	    printf "unvaccinated,Incidence,%u,%.2f\n", $year, $week,
		($unvaccinated_ili{$year}{$week} /
		     ($unvaccinated_nonili{$year}{$week} + .0));
	}
    }
}

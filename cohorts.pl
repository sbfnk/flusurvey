#!/usr/bin/perl -w -s
# extract cohort information from csv with data form cohorts.sql query
# reads STDIN, relies on a header and a table sorted last by vaccination
# (so that matched groups are in subsequent lines)
# needs a column called "vaccinated" and can use a column called "week"
# also needs a column called "ili" and a column called "non_ili"
# every column right of the "vaccinated" column will be ignored for the group
# matching

use strict;

sub min {
    [$_[0], $_[1]]->[$_[0] >= $_[1]];
}

# verbose switch
my $verbose = 0;
    my $switch = shift;
if ($switch && $switch eq "-v") {
    $verbose = 1;
}

# read header line
my $header_line = <STDIN>;
chomp($header_line);
my @header = split /,/, $header_line;
my $vaccinated_index = -1; # index of "vaccinated" column
my $week_index = -1; # index of "week" column
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
my $week = 0;

# variables to store counts of vaccinated/unvaccinated
my %vaccinated_ili;
my %vaccinated_nonili;
my %unvaccinated_ili;
my %unvaccinated_nonili;

while ($index < scalar( @data )) {
    # see if we have got a "week" column
    if ($week_index >= 0) {
	$week = $data[$index][$week_index];
	if (!(exists $vaccinated_ili{$week})) {
	    $vaccinated_ili{$week} = 0;
	}
	if (!(exists $vaccinated_nonili{$week})) {
	    $vaccinated_nonili{$week} = 0;
	}
	if (!(exists $unvaccinated_ili{$week})) {
	    $unvaccinated_ili{$week} = 0;
	}
	if (!(exists $unvaccinated_nonili{$week})) {
	    $unvaccinated_nonili{$week} = 0;
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
	    $unvaccinated_ili{$week} +=
		$unvaccinated[$ili_index] *
		    $smaller_total / $unvaccinated_total;
	    $unvaccinated_nonili{$week} +=
		$unvaccinated[$nonili_index] *
		    $smaller_total / $unvaccinated_total;
	    $vaccinated_ili{$week} +=
		$vaccinated[$ili_index] *
		    $smaller_total / $vaccinated_total;
	    $vaccinated_nonili{$week} +=
		$vaccinated[$nonili_index] *
		    $smaller_total / $vaccinated_total;
	    $index++;
	}
    } else {
	$index++;
    }
}

print "\"Vaccination status\",week,variable,value\n";
foreach (sort keys %vaccinated_ili) {
    printf "vaccinated,ILI,%u,%.0f\n", $_, $vaccinated_ili{$_};
    printf "vaccinated,non-ILI,%u,%.0f\n", $_, $vaccinated_nonili{$_};
    printf "vaccinated,Incidence,%u,%.2f\n", $_, ($vaccinated_ili{$_} /
				       ($vaccinated_nonili{$_} + .0));
    printf "unvaccinated,ILI,%u,%.0f\n", $_, $unvaccinated_ili{$_};
    printf "unvaccinated,non-ILI,%u,%.0f\n", $_, $unvaccinated_nonili{$_};
    printf "unvaccinated,Incidence,%u,%.2f\n", $_, ($unvaccinated_ili{$_} /
				       ($unvaccinated_nonili{$_} + .0));
}

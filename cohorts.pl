#!/usr/bin/perl -w

use strict;

my @header = split /,/, <STDIN>;
my $vaccinated_index;
my $count = -1;

do {
    $count++;
    $vaccinated_index = $count;
} while ("$header[$count]" ne "vaccinated");

my @data;

while (<STDIN>) {
    chomp;
    my @line = split /,/;
    push @data, \@line;
}

my $index = 0;

my $vaccinated_ili = 0;
my $vaccinated_nonili = 0;
my $unvaccinated_ili = 0;
my $unvaccinated_nonili = 0;

while ($index < scalar( @data )) {
    if ($data[$index][$vaccinated_index] == 0) {
	my @unvaccinated = @{ $data[$index] };
	$index++;
	my $matching_group = ($data[$index][$vaccinated_index] == 1);
	for (my $i = 0; $i < $vaccinated_index; $i++) {
	    $matching_group *= ("$data[$index][$i]" eq "$unvaccinated[$i]");
	}
	if ($matching_group) {
	    my @vaccinated = @{ $data[$index] };
	    if ($vaccinated[$vaccinated_index+1] >
		    $unvaccinated[$vaccinated_index+1]) {
		$unvaccinated_ili += $unvaccinated[$vaccinated_index+2];
		$unvaccinated_nonili += $unvaccinated[$vaccinated_index+3];
		my $added_ili_vaccinated = $vaccinated[$vaccinated_index+2] /
		     			  ($vaccinated[$vaccinated_index+1] +
					       .0) * $unvaccinated[$vaccinated_index+1];
		$vaccinated_ili += $added_ili_vaccinated;
		$vaccinated_nonili += $unvaccinated[$vaccinated_index+1] -
		    $added_ili_vaccinated;
	    } else {
		$vaccinated_ili += $vaccinated[$vaccinated_index+2];
		$vaccinated_nonili += $vaccinated[$vaccinated_index+3];
		my $added_ili_unvaccinated = $unvaccinated[$vaccinated_index+2] /
		     			  ($unvaccinated[$vaccinated_index+1] +
					       .0) * $vaccinated[$vaccinated_index+1];
		$unvaccinated_ili += $added_ili_unvaccinated;
		$unvaccinated_nonili += $vaccinated[$vaccinated_index+1] -
		    $added_ili_unvaccinated;
	    }
	    $index++;
	}
    } else {
	$index++;
    }
}

print "Vaccinated:\n";
printf "  ILI: %.0f\n", $vaccinated_ili;
printf "  non-ILI: %.0f\n", $vaccinated_nonili;
printf "  Incidence: %.2f\n", ($vaccinated_ili / ($vaccinated_nonili + .0));
print "Unvaccinated:\n";
printf "  ILI: %.0f\n", $unvaccinated_ili;
printf "  non-ILI: %.0f\n", $unvaccinated_nonili;
printf "  Incidence: %.2f\n", ($unvaccinated_ili / ($unvaccinated_nonili + .0));

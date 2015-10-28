#!/usr/bin/perl -w

use strict;
use List::Util qw(max);

my $header = <STDIN>;
chomp($header);

my @columns = split /;/, $header;
my $nColumns = scalar(@columns);
my $origColumns = $nColumns;

my @max = (0) x $nColumns;
my @idx = (0) x $nColumns;
my @data;

while (<STDIN>) {
    chomp;
    my @fields = split /;/;
    for (my $i = 0; $i < scalar (@fields); $i++) {
	if ($fields[$i] =~ /,/) {
	    if ($fields[$i] =~ /[a-zA-Z]/) {
		$fields[$i] = "\"$fields[$i]\"";
	    } else {
		my @subfields = split /,/, $fields[$i];
		$max[$i] = max($max[$i], pop(@subfields));
	    }
	}
    }
    push (@data, \@fields);
}

for (my $i = 0; $i < scalar(@max); $i++) {
    if ($max[$i] > 0) {
	$idx[$i] = $nColumns;
	for (my $j = 1; $j < $max[$i]+1; $j++) {
	    push(@columns, "$columns[$i]"."_"."$j");
	}
	$nColumns += $max[$i];
    }
}

print join ';', @columns;
print "\n";

foreach (@data) {
    my @fields = @{ $_ };
    for (my $i = $origColumns; $i < $nColumns; $i++) {
	push @fields, 0;
    }
    for (my $i = 0; $i < $origColumns; $i++) {
	if ($max[$i] > 0 && $fields[$i] =~ /[0-9]/) {
#        if ($fields[$i] =~ /,/ && $fields[$i] !~ /[a-zA-Z]/) {
	    my @subfields = split /,/, $fields[$i];
	    foreach (@subfields) {
		$fields[$idx[$i]+$_-1] = 1;
	    }
	}
    }
    print join ';', @fields;
    print "\n";
}

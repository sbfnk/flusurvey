#!/usr/bin/perl -w

use strict;

my $question;
my $multiple = 0;
my $count = 0;
my $countmode = 0;
while (my $line = <STDIN>) {
    chomp $line;
    if ($line =~ /^class (.+)\(/) {
	if ($multiple == 1) {
	    print "$question,$count\n";
	}
	$question = $1;
	$multiple = 0;
	$count = 0;
	$countmode = 0;
    } elsif ($line =~ /'options-multiple'/) {
	$multiple = 1;
    } elsif ($line =~ /'table-of-selects'/) {
	$multiple = 1;
    } elsif ($line =~ /columns = /) {
	$countmode = 1;
    } elsif ($line =~ /choices = /) {
	$countmode = 0;
    } elsif ($line =~ /options = /) {
	$countmode = 1;
    }
    if ($countmode && ($line =~ /(,|\)|\])$/)) {
	$count++;
    }
}

if ($multiple == 1) {
    print "$question,$count\n";
}

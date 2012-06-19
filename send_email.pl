#!/usr/bin/perl -w

use MIME::Entity;
use strict;

my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = gmtime(time);
my $filename=sprintf("./incidence_%04d%02d%02d.csv", $year+1900, $mon+1, $mday);

system("psql -f incidence.sql".
	   "--no-align --pset footer --field-separator ','".
	       " > $filename");

### Create the top-level, and set up the mail headers:
my $top = MIME::Entity->build(Type    =>"multipart/mixed",
			      From    => "webmaster\@flusurvey.org.uk",
			      To      => "funks\@gmx.net",
			      CC      => "sebastian.funk\@lshtm.ac.uk",
			      Subject => "Flusurvey weekly data 30/11/11");

### Part #1: message
$top->attach(Data=>["This is a text email\n\nRegards,\n  Sebastian\n"]);

### Part #2: incidence data
$top->attach(Path=>"$filename");

### Send it:
open MAIL, ">output.txt" or die "open: $!";
$top->print(\*MAIL);
close MAIL;

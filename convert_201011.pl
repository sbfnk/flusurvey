#!/usr/bin/perl -w

use strict;

my %nb_options;
if (scalar (@ARGV) > 0) {
    my $options_file = $ARGV[0];
    open IN, "<$options_file" or
            die "Could not open options file $options_file\n";
    while (<IN>) {
        chomp;
        my @line = split /,/;
        $nb_options{$line[0]} = $line[1];
    }
}

# read header line
my $header_line;
if (!($header_line = <STDIN>)) {
    die "No header line\n";
}
chomp($header_line);
my @header = split /,/, $header_line;
my $startidx = scalar(@header);

my @newheader = @header;

for (my $i = 0; $i < $startidx; $i++) {
    if (defined $nb_options{$header[$i]}) {
        for (my $j = 0; $j < $nb_options{$header[$i]}; $j++) {
            push (@newheader, "$header[$i].$j");
        }
    }
}
print join(",", @newheader)."\n";

while (<STDIN>) {
    chomp;
    my @data = split /,/, $_, -1;
    my $length = (scalar @data);
    my $idx = $startidx;
    for (my $i = 0; $i < $length; $i++) {
        if (defined $nb_options{$header[$i]}) {
            my $type = substr($data[$i], 0, 1);
            my @unpacked;
            if ($type eq "c") {
                @unpacked = split(//, $data[$i]);
                shift(@unpacked);
            } elsif ($type eq "s") {
                @unpacked = split(/\|/, $data[$i]);
                shift(@unpacked);
            }
            for (my $j = 0; $j < $nb_options{$header[$i]}; $j++) {
                if ($data[$i] eq "None") {
                    $data[$idx] = "";
                } else {
                    if (scalar (@unpacked) > 0) {
                        $data[$idx] = pop(@unpacked);
                    } else {
                        $data[$idx] = 0;
                    }
                }
                $idx++;
            }
        }
    }
    print join(",", @data)."\n";
}

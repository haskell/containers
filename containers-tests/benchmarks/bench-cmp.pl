#!/usr/bin/env perl
use warnings;
use strict;

@ARGV >= 2 or die "Usage: bench-cmp.pl csv_file_1 csv_file_2";
open (my $f1, "<", $ARGV[0]) or die "Cannot open file $ARGV[0]";
open (my $f2, "<", $ARGV[1]) or die "Cannot open file $ARGV[1]";

my $l1 = <$f1>;
my $l2 = <$f2>;
$l1 eq $l2 or die "CSV files do not correspond -- $l1 and $l2";

my $min = 1e50;
my $mult = 1.0;
my $max = 0.0;
my $count = 0;

while (defined($l1 = <$f1>)) {
  $l2 = <$f2>;

  my @parts1 = split /,/, $l1;
  my @parts2 = split /,/, $l2;

  $parts1[0] eq $parts2[0] or die "CSV files do not correspond -- $parts1[0] and $parts2[0]";

  my $factor;
  if ($parts1[1] == 0) {
      if ($parts2[1] == 0) {
          $factor = 1;
      } else {
          $factor = 'inf';
      }
  } else {
      $factor = $parts2[1] / $parts1[1];
  }
  $count = $count + 1;
  $mult = $mult * $factor;
  if ($factor > $max) {
      $max = $factor;
  }
  if ($factor < $min) {
      $min = $factor;
  }

  if ($factor == 'inf') {
    printf "%s;%.2e;%.2e\n", $parts1[0], $parts2[1], $parts1[1];
  } else {
    printf "%s;%+7.2f%%;%.2e\n", $parts1[0], 100 * $factor - 100, $parts1[1];
  }
}

printf ";\n";
printf "Minimum;%+7.2f%%\n", ($min - 1.0)*100;
printf "Average;%+7.2f%%\n", (($mult ** (1.0 / $count)) - 1.0) * 100;
printf "Maximum;%+7.2f%%\n", ($max - 1.0) * 100;

close $f2;
close $f1;

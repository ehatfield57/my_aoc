#!/usr/bin/perl
use strict;
use warnings;

sub part1 {
  my ($filename) = @_;
  my $total = 0;

  open my $fh, '<', $filename or die "Can't open file '$filename': $!";
  while (my $line = <$fh>) {
    while ($line =~ /mul\((\d{1,3}),(\d{1,3})\)/g) {
      $total += ($1 * $2);
    }
  }
  close $fh;

  return $total;
}

if (part1("test.txt") != 161) {
  print "Error: part1 with 'test.txt' file is not 161\n";
}

if (part1("input.txt") != 165225049) {
  print "Error: part1 with 'input.txt' file is not 165225049\n";
}

sub part2 {
  my ($filename) = @_;
  my ($does, $dont, $num1, $num2) = (0, 0, 0, 0);
  my ($total, $preString, $doit) = (0, "", 1);

  open my $fh, '<', $filename or die "Can't open file '$filename': $!";
  while (my $line = <$fh>) {
    chomp($line);

    while ($line =~ /(.*?)mul\((\d{1,3}),(\d{1,3})\)/g) {
      ($preString, $num1, $num2) = ($1, $2, $3);
      $does = () = $preString =~ /do\(\)/g;
      $dont = () = $preString =~ /don\'t\(\)/g;
      if ($does == 1) { $doit = 1; }
      if ($dont == 1) { $doit = 0; }
      if ($doit) { $total += ($num1 * $num2); }
    }
  }
  close $fh;

  return $total;
}

if (part2("test2.txt") != 48) {
  print "Error: part2 with 'test.txt' file is not 48\n";
}

if (part2("input.txt") != 108830766) {
  print "Error: part2 with 'input.txt' file is not 108830766\n";
}


#!/usr/bin/perl

use strict;
use CGI qw/param header/;
print header('text/plain');

my JAVACLASS = "planner";

# Read data
my $holding = param("holding");
my $world = param("world");
my $trees = param("trees");

# Remove whitespace
$holding =~ s/\s//g;
$world =~ s/\s//g;
$trees =~ s/\s//g;

# Call Java
exec "java $JAVACLASS '$holding' '$world' '$trees'";


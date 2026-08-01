#!/usr/bin/env perl

use strict;
use warnings;

use Cwd qw(abs_path);
use File::Spec;
use FindBin qw($Bin);

my $root = abs_path(File::Spec->catdir($Bin, File::Spec->updir()));

sub read_file {
    my ($path) = @_;
    open my $handle, '<', $path
        or die "Cannot open $path: $!\n";
    local $/;
    return <$handle>;
}

my $main = read_file(File::Spec->catfile($root, 'src', 'main.ml'));
my $myarg = read_file(File::Spec->catfile($root, 'src', 'myarg.ml'));
my $manual = read_file(File::Spec->catfile($root, 'doc', 'base.tex'));

$main =~ /^[ \t]*let argDescr = \[(.*?)^[ \t]*\] in[ \t]*$/ms
    or die "Cannot locate argDescr in src/main.ml\n";
my $arg_descr = $1;

my %implemented;
while ($arg_descr =~ /^[ \t]*"(--[^"\s]+)"[ \t]*,/mg) {
    $implemented{$1} = 1;
}

# Myarg supplies options from its usage function when the application does
# not override them.
$myarg =~ /^[ \t]*let usage\b(.*?)^[ \t]*;;[ \t]*$/ms
    or die "Cannot locate usage in src/myarg.ml\n";
my $usage = $1;
while ($usage =~ /"(--[A-Za-z0-9][A-Za-z0-9#-]*)/g) {
    $implemented{$1} = 1;
}

my %documented;
while ($manual =~ /\\DEFINE\{(--(?:\\[\\#_%&]|[^}])+)\}/g) {
    my $option = $1;
    $option =~ s/\\([#_%&])/$1/g;
    ++$documented{$option};
}

my @missing = grep { !exists $documented{$_} } sort keys %implemented;
my @stale = grep { !exists $implemented{$_} } sort keys %documented;
my @duplicates = grep { $documented{$_} > 1 } sort keys %documented;

if (@missing || @stale || @duplicates) {
    print STDERR "Command-line option documentation is out of sync.\n";
    print STDERR "Undocumented options:\n  ", join("\n  ", @missing), "\n"
        if @missing;
    print STDERR "Documented but unimplemented options:\n  ",
        join("\n  ", @stale), "\n"
        if @stale;
    print STDERR "Options documented more than once:\n  ",
        join("\n  ", @duplicates), "\n"
        if @duplicates;
    exit 1;
}

print scalar(keys %implemented),
    " command-line options are documented exactly once.\n";

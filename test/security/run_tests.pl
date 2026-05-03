#!/usr/bin/perl

use strict;
use warnings;
use File::Path qw(make_path remove_tree);
use File::Spec;
use Cwd qw(abs_path);

my $test_dir = abs_path(File::Spec->curdir());
my $tmp = File::Spec->catdir($test_dir, "tmp");

my $weidu = $ENV{WEIDU_BIN} || "weidu";

sub ensure_clean_tmp {
    remove_tree($tmp) if -d $tmp;
    make_path($tmp);
}

sub run_capture {
    my ($name, $env, @args) = @_;
    my $out = File::Spec->catfile($tmp, "$name.out.txt");
    my $err = File::Spec->catfile($tmp, "$name.err.txt");

    local %ENV = %ENV;
    for my $k (keys %$env) {
        $ENV{$k} = $env->{$k};
    }

    my @cmd = ($weidu, @args);
    # Keep command execution shell-based here to preserve behavior of stderr/stdout
    # redirection across host OSes for this lightweight regression runner.
    my $cmd_str = join(" ", map { /\s/ ? qq{"$_"} : $_ } @cmd);
    my $rc = system($cmd_str . " 1>\"$out\" 2>\"$err\"");
    my $exit = $rc >> 8;

    open(my $fh_out, "<", $out) or die "Cannot open $out\n";
    local $/;
    my $stdout = <$fh_out>;
    close($fh_out);

    open(my $fh_err, "<", $err) or die "Cannot open $err\n";
    my $stderr = <$fh_err>;
    close($fh_err);

    return ($exit, $stdout . "\n" . $stderr);
}

sub assert_true {
    my ($cond, $msg) = @_;
    die "TEST FAILED: $msg\n" unless $cond;
}

sub test_dry_run_copy {
    my $tp2 = File::Spec->catfile("fixtures", "dry_run_copy.tp2");
    my ($exit, $text) = run_capture(
        "dry_run_copy",
        {},
        "--nogame",
        "--no-exit-pause",
        "--dry-run",
        "--force-install", "0",
        $tp2
    );
    assert_true($exit == 0, "dry-run copy should succeed");
    assert_true($text =~ /\[DRY-RUN\] would copy/i, "dry-run copy message missing");
}

sub test_require_sha256_failure_when_tools_missing {
    my $tp2 = File::Spec->catfile("fixtures", "require_sha256.tp2");
    my ($exit, $text) = run_capture(
        "require_sha256",
        { PATH => "" },
        "--nogame",
        "--no-exit-pause",
        "--require-sha256",
        "--force-install", "0",
        $tp2
    );
    assert_true($exit != 0, "--require-sha256 should fail when SHA256 tools are unavailable");
    assert_true($text =~ /SHA-256 is required but unavailable/i, "require-sha256 error message missing");
}

sub test_strict_high_risk_block_linux {
    my $tp2 = File::Spec->catfile("fixtures", "strict_high_risk_linux.tp2");
    my ($exit, $text) = run_capture(
        "strict_high_risk_linux",
        { WEIDU_OS => "linux" },
        "--nogame",
        "--no-exit-pause",
        "--dry-run",
        "--strict-path-risk",
        "--force-install", "0",
        $tp2
    );
    assert_true($exit != 0, "--strict-path-risk should block high-risk linux path");
    assert_true($text =~ /HIGH-RISK/i, "high-risk warning missing");
    assert_true($text =~ /blocked by --strict-path-risk/i, "strict-path-risk block message missing");
}

sub test_sensitive_path_warns_but_not_blocked {
    my $tp2 = File::Spec->catfile("fixtures", "sensitive_macos.tp2");
    my ($exit, $text) = run_capture(
        "sensitive_macos",
        { WEIDU_OS => "macos" },
        "--nogame",
        "--no-exit-pause",
        "--dry-run",
        "--force-install", "0",
        $tp2
    );
    assert_true($exit == 0, "sensitive path warning should not block without strict mode");
    assert_true($text =~ /sensitive/i, "sensitive warning missing");
}

ensure_clean_tmp();
chdir($test_dir) or die "Cannot chdir to test dir\n";

test_dry_run_copy();
test_require_sha256_failure_when_tools_missing();
test_strict_high_risk_block_linux();
test_sensitive_path_warns_but_not_blocked();

print "Security tests SUCCESSFUL\n";
exit 0;

#!/usr/bin/perl

use strict;
use warnings;
use File::Path qw(make_path remove_tree);
use File::Spec;
use Cwd qw(abs_path);
use FindBin qw($Bin);

my $test_dir = abs_path($Bin);
my $tmp = File::Spec->catdir($test_dir, "tmp");

my $weidu = $ENV{WEIDU_BIN} || "weidu";

sub ensure_clean_tmp {
    remove_tree($tmp) if -d $tmp;
    make_path($tmp);
    unlink(File::Spec->catfile($test_dir, "WeiDU.log"));
    unlink(File::Spec->catfile($test_dir, "weidu-audit.log"));
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

sub assert_not_exists {
    my ($path, $msg) = @_;
    assert_true(!-e $path, $msg);
}

sub write_text {
    my ($path, $text) = @_;
    my (undef, $dir, undef) = File::Spec->splitpath($path);
    make_path($dir) if $dir && !-d $dir;
    open(my $fh, ">", $path) or die "Cannot write $path\n";
    print {$fh} $text;
    close($fh);
}

sub read_text {
    my ($path) = @_;
    open(my $fh, "<", $path) or die "Cannot read $path\n";
    local $/;
    my $text = <$fh>;
    close($fh);
    return $text;
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
    assert_true($text =~ /operations suppressed/i, "dry-run summary should report suppressed operations");
    assert_not_exists(File::Spec->catfile($tmp, "dry_copy", "security-dry-run.txt"), "dry-run copy created target file");
    assert_not_exists(File::Spec->catfile($tmp, "dry_copy", "backup"), "dry-run copy created backup directory");
    assert_not_exists(File::Spec->catfile($test_dir, "WeiDU.log"), "dry-run copy created WeiDU.log");
}

sub test_dry_run_misc_operations {
    my $base = File::Spec->catdir($tmp, "dry_misc");
    make_path(File::Spec->catdir($base, "delete-dir"));
    write_text(File::Spec->catfile($base, "append.txt"), "original\n");
    write_text(File::Spec->catfile($base, "delete-dir", "kept.txt"), "keep\n");
    write_text(File::Spec->catfile($base, "move-source.txt"), "move me\n");

    my $tp2 = File::Spec->catfile("fixtures", "dry_run_misc.tp2");
    my ($exit, $text) = run_capture(
        "dry_run_misc",
        {},
        "--nogame",
        "--no-exit-pause",
        "--dry-run",
        "--force-install", "0",
        $tp2
    );
    assert_true($exit == 0, "dry-run misc operations should succeed");
    assert_true($text =~ /would create directory/i, "dry-run mkdir message missing");
    assert_true($text =~ /would delete/i, "dry-run delete message missing");
    assert_true($text =~ /would move/i, "dry-run move message missing");
    assert_true($text =~ /would execute/i, "dry-run shell command message missing");
    assert_not_exists(File::Spec->catfile($base, "newdir"), "dry-run mkdir created directory");
    assert_not_exists(File::Spec->catfile($base, "large-copy.txt"), "dry-run COPY_LARGE created target");
    assert_true(read_text(File::Spec->catfile($base, "append.txt")) eq "original\n", "dry-run APPEND changed file");
    assert_true(-e File::Spec->catfile($base, "delete-dir", "kept.txt"), "dry-run DELETE removed directory contents");
    assert_true(-e File::Spec->catfile($base, "move-source.txt"), "dry-run MOVE removed source");
    assert_not_exists(File::Spec->catfile($base, "move-dest.txt"), "dry-run MOVE created destination");
    assert_not_exists(File::Spec->catfile($base, "at-now.txt"), "dry-run AT_NOW executed shell command");
    assert_not_exists(File::Spec->catfile($test_dir, "WeiDU.log"), "dry-run misc created WeiDU.log");
}

sub test_dry_run_uninstall_preserves_state {
    my $tp2 = File::Spec->catfile("fixtures", "dry_run_uninstall.tp2");
    my $installed = File::Spec->catfile($tmp, "dry_uninstall", "installed.txt");
    my ($install_exit, $install_text) = run_capture(
        "dry_run_uninstall_install",
        {},
        "--nogame",
        "--no-exit-pause",
        "--force-install", "0",
        $tp2
    );
    assert_true($install_exit == 0, "setup install for dry-run uninstall should succeed");
    assert_true(-e $installed, "setup install did not create target file");
    my $log_path = File::Spec->catfile($test_dir, "WeiDU.log");
    assert_true(-e $log_path, "setup install did not create WeiDU.log");
    my $before_log = read_text($log_path);

    my ($uninstall_exit, $uninstall_text) = run_capture(
        "dry_run_uninstall",
        {},
        "--nogame",
        "--no-exit-pause",
        "--dry-run",
        "--force-uninstall", "0",
        $tp2
    );
    assert_true($uninstall_exit == 0, "dry-run uninstall should succeed");
    assert_true($uninstall_text =~ /\[DRY-RUN\] would delete/i, "dry-run uninstall delete message missing");
    assert_true(-e $installed, "dry-run uninstall removed installed file");
    assert_true(read_text($log_path) eq $before_log, "dry-run uninstall changed WeiDU.log");
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
test_dry_run_misc_operations();
test_dry_run_uninstall_preserves_state();
test_require_sha256_failure_when_tools_missing();
test_strict_high_risk_block_linux();
test_sensitive_path_warns_but_not_blocked();

print "Security tests SUCCESSFUL\n";
exit 0;

#!/usr/bin/perl -w

use warnings;
use strict;
use File::Copy;
use File::Basename qw ( fileparse );
use POSIX;
use Sys::Hostname;
use Cwd qw( abs_path );
use List::Util qw(reduce);
use Data::Dumper qw(Dumper);
use Getopt::Long;
use Pod::Usage qw( pod2usage );
use File::Find;

our $fuzzbombs_per_slot = 1;

# program args  --------------------------------------------------

warn "neofuzz-ccl-launcher.pl args: @ARGV\n";
our $jobdir=shift();
our $lisp = shift();
our $optimi = shift();
our $fbs = shift();
our $localhostonly = shift();
our $usegui = shift();
our $allocationmethod = shift();
our $justdontdoit = shift();

if ((not (defined $lisp)) || $lisp eq "") {
  die "must specify lispcode\n";
}
if (!$localhostonly && (!$optimi || $optimi < 1)) {
  die "must specify positive number of Optimi\n";
}
if (!$localhostonly && ($fbs < 0)) {
  die "must specify non-negative number of Fuzzbombs\n";
}
if ($localhostonly) { warn("launching one neofuzz on local host only; numoptimi and numfbs ignored...\n"); $fbs=0;$optimi=1;}
else {warn("launching neofuzz with $optimi optimi, $fbs fuzzbombs...\n");}

if ((not (defined $allocationmethod)) || $allocationmethod eq "") {
    $allocationmethod = 'default';
}
if (not (($allocationmethod eq 'default')
         || ($allocationmethod eq 'balanced')
         || ($allocationmethod eq 'justthishost')))
	{ die "Unsupported allocationmethod: $allocationmethod"; }

#----------------------------------------------
# file locking

use Fcntl qw(:flock SEEK_END);

sub lock {
        my ($fh) = @_;
        flock($fh, LOCK_EX) or die "Cannot lock file $!\n";
        # and, in case someone appended while we were waiting...
        #seek($fh, 0, SEEK_END) or die "Cannot seek - $!\n";
    }

sub unlock {
        my ($fh) = @_;
        flock($fh, LOCK_UN) or die "Cannot unlock file- $!\n";
    }

#----------------------------------------------
# here's stuff to use a persistent /tmp file to hold next cluster identifier and other host load data
# see https://www.oreilly.com/library/view/mastering-perl/9780596527242/ch14.html

## NOTE when the tasks end, we must of course ensure that /tmp/cluster-allocation is revised to release the resources.
## this is done by release-cluster and release-my-clusters tool.

# uid for cluster being created.  Will be initialized to $nextcluster + 1 when cluster-allocation file is read.
our $cluster = 1;
our @chosen_hosts;
our (@hosts, %cores, %used, %cluster_hosts, %cluster_owners, $nextcluster);
our $totalcores=0;
our $totalused=0;
our $fh_cluster_allocation;

sub read_cluster_allocation {
  my $fn = "/tmp/cluster-allocation";
  if (! -e $fn) {
	warn "$fn doesnt exist: $!; resetting";
        system("$main::cgc/code/tools/reset-cluster-allocations");
	}
  my $data = do {
        if (open $fh_cluster_allocation, '+<', $fn) { lock($fh_cluster_allocation); local $/; <$fh_cluster_allocation> }
        else { undef }
        };
  warn "about to eval old data to get status\n";
  warn "data is:\n$data\n";
  eval $data; 	# this reloads the hashes from $fn, supporting the strict predeclared vars
  warn "finished eval of old data\n";
  warn "Hosts are:\n";
  warn "$_\n" for @hosts;

  $cluster = $nextcluster;
  $nextcluster++;

  for my $host (@hosts) {
    	$totalcores += $cores{$host};
	$totalused += $used{$host};
	}

  warn "Total cores: $totalcores, total used: $totalused\n";
}

sub choose_hosts {
  my ($needed) = @_;
  my @result;

  warn "asked to choose $needed hosts\n";

	# allocate if possible
  if ($needed > ($totalcores - $totalused)) { die "ERROR: not enough host cores available for all the specified optimi and fuzzbombs"; }

  $cluster_owners{$cluster}=$ENV{'USER'};
  while ($needed>0) {
      # sort the hosts by availability level so pick unloaded ones first
        my @sorted_hosts= sort { ($cores{$a}-$used{$a}) <=> ($cores{$b}-$used{$b}) } @hosts;
	my $curhost = pop @sorted_hosts;
	my $avail = $cores{$curhost}-$used{$curhost};
	my $give=0;
	if ($avail>0 && $needed > $avail) { $give = $avail; } else {$give=$needed;}
	$used{$curhost}+=$give;
	$needed -= $give;
	for my $i (1..$give)
  		{ push @result, $curhost;
		  $cluster_hosts{$cluster}= join(' ', @result);
		}
    }
   return @result;
}

sub choose_balanced_hosts {
  my ($needed) = @_;
  my @result;
  warn "asked to choose $needed hosts (balanced).\n";
  my $num_hosts = scalar(@hosts);
  warn "num_hosts: $num_hosts\n";
  my @spread = spread($needed, $num_hosts);
  warn "spread: @spread\n";
  # feasible?
  for (my $i=0; $i<$num_hosts; $i++) {
      my $avail = $cores{$hosts[$i]} - $used{$hosts[$i]};
      warn "avail: $avail  required: $spread[$i]\n";
      if ($avail < $spread[$i]) {die "ERROR: not enough host cores available for requested balanced spread";};
  }
  # allocate
  $cluster_owners{$cluster}=$ENV{'USER'};
  for (my $i=0; $i<$num_hosts; $i++) {
      my $curhost = $hosts[$i];
      my $give = $spread[$i];
      $used{$curhost}+=$give;
      for my $j (1..$give) {
          push @result, $curhost;
          $cluster_hosts{$cluster}= join(' ', @result);
      }
  }
  return @result;
}

sub choose_justthishost {
  my ($needed) = @_;
  my @result;
  warn "asked to start $needed agents on justthishost.\n";
  my $avail = $cores{$main::hostname}-$used{$main::hostname};
  if ($needed > $avail) { die "ERROR: not enough host cores available for all the specified optimi and fuzzbombs"; }
  $cluster_owners{$cluster}=$ENV{'USER'};
	my $give=$needed;
	$used{$main::hostname}+=$give;
	for my $i (1..$give)
  		{ push @result, $main::hostname;
		  $cluster_hosts{$cluster}= join(' ', @result);
		}
   return @result;
}


# return list of $bins numbers summing to $total.
sub spread {
    my ($total, $bins) = @_;
    my $floor = int($total / $bins);
    my $rem = $total % $bins;
    # init array values to $floor (parens are necessary for perl's WWLD type system)
    my @retval = ($floor) x $bins;
    # spread remainder
    for (my $i=0; $i < $rem; $i++) {
        $retval[$i] = $retval[$i] + 1;
    }
    return @retval;
}

sub write_cluster_allocation {
    # write the revised status back out
    seek($fh_cluster_allocation, 0, 0) or die "Cannot seek - $!\n"; # go back to start of file
    truncate $fh_cluster_allocation,0;

    my $dd = Data::Dumper->new([\@hosts, \%cores, \%used, \%cluster_hosts, \%cluster_owners, $nextcluster],[qw(*hosts *cores *used *cluster_hosts *cluster_owners *nextcluster)]);
    print $fh_cluster_allocation $dd->Dump;
    unlock($fh_cluster_allocation);
    close $fh_cluster_allocation;
}

sub make_amp {
    my ($name, $role) = @_;
    $name = uc($name);
    my $str = "(if (or (string= \"$name\" \"OPTIMUS0\")  (string= common-lisp-user::*fb-instance* \"OPTIMUS0\") (string= common-lisp-user::*fb-instance* \"$name\")) (make-amp \"$name\"";
    $str .= " :master-p T" if ($name eq "OPTIMUS0");
    $str .= " :role $role))\n";
    return $str;
}

sub write_filestring {
    my $fn = shift;
    my $str = shift;
    my $fh;
    open($fh, ">$fn");
    $fh->autoflush(1);
    print $fh $str;
    close($fh);
}

# the old start of doit

our ($program, $pdir, $psuffix) = fileparse($0, qr/\.[^.]*/);
$program = "nf";
# current working directory
our $dir = abs_path(".");
our $basename = "nf";
if (defined($ENV{CIRCA_BASENAME})) {
    $basename = $ENV{CIRCA_BASENAME};
} elsif (defined($ENV{USER})) {
    $basename = $ENV{USER};
}
our $baseport = 10000;
if (defined($ENV{CIRCA_BASEPORT})) {
    $baseport = $ENV{CIRCA_BASEPORT};
}

our $matchmakeroffset = 2;
#our $usertmp = "/tmp/$basename";
#if (! (-d $usertmp)) { mkdir($usertmp) };
#die "Couldn't make directory $usertmp" unless -d "$usertmp";
our $hostname = `hostname`;
chomp $hostname;
our $docker_tag = "latest";
if (defined($ENV{DOCKER_TAG})) {
    $docker_tag = $ENV{DOCKER_TAG};
}

#Needed for rootless Docker
our $docker_host = "";
if (defined($ENV{DOCKER_HOST})) {
    $docker_host = $ENV{DOCKER_HOST};
}
our $loopback = "";
if (defined($ENV{LOOPBACK})) {
    $loopback = $ENV{LOOPBACK};
}

STDOUT->autoflush(1);
STDERR->autoflush(1);
our $VERBOSE = 0;
our $TIMESTAMP = 1;

our $home = $ENV{HOME};
our $cgc = abs_path("$main::pdir../..");

our $log; # file handle
our $experiment = "";
our $submit = "";
our $shareddir = "$cgc/scratch";
our $shared = $ENV{DOIT_SHARED} || $shareddir;
our $owner = $ENV{DOIT_OWNER} || $basename;

# data structures shared between modules
our %fuzzbombs = ();

warn("verbose: $VERBOSE\n");
warn("program: $program\n");
warn("dir: $dir\n");
warn("hostname: $hostname\n");
warn("CIRCA_BASEPORT: $baseport\n");
warn("CIRCA_BASENAME: $basename\n");

# Generate experiment.lisp from template

$lisp = "(cl:in-package :fuzzbomb)\n" . $lisp;	# preface w/ in-package

#$lisp .= "(pushnew :all *debug-list*)\n"; # rm this when things are rocking.
$lisp .= "(dbug :top \"This instance is: ~A\" common-lisp-user::*fb-instance*)\n";
$lisp .= "(setf *connect-to-bridge* t)\n";
$lisp .= "(setf *run-acceptor* t)\n";
$lisp .= ";; required make-amp calls\n";
my $ocount = 0;
my $fcount = 0;
my @tasks = ();
my (@os, @fs);
while ($ocount < $optimi) {
  my $name = "OPTIMUS$ocount";
  push(@os, $name);
  $lisp .= make_amp($name, ":optimus");
  ++$ocount;
}
while ($fcount < $fbs) {
  my $name = "FUZZBOMB$fcount";
  push(@fs, $name);
  $lisp .= make_amp($name, ":fuzzbomb");
  ++$fcount;
}
my $oprime = $os[0];

if ($usegui) {
  my $gui = "$oprime-GUI";
  $lisp .= "(dbug :top \"GUI name is $gui\")\n";
  $lisp .= make_amp($gui, ":gui");
  $lisp .= "(setf *use-gui* t)";
}

##$lisp .= "(setf *optimus-prime* \"$oprime\")\n";
$lisp .= "(setf *optimi* '(\"" . join('" "', @os) . "\"))\n";
$lisp .= "(setf *fuzzbombs* '(\"" . join('" "', @fs) . "\"))\n";
$lisp .= "(setf *dvms* nil)\n";

$lisp .= "(dbug :top \"Finished loading experiment.lisp\")\n";
# this was in case needed to push run-amp call into here.
#$lisp .= "(dbug :top \"Finished loading experiment.lisp, about to call run-amp\")\n";
#$lisp .= "(fb::run-amp :name common-lisp-user::*fb-instance*)\n";

write_filestring("$main::cgc/code/experiment.lisp", $lisp);

#- idea for trivial node locking: have a file with a perl-formatted hash or two you lock (or wait), load, check for capacity and modify if avail,
#then write back out and unlock.  No parsing required and easily readable by human too.  Just needs hostname, totalcapacity, inuse.

if ($localhostonly) {
  @chosen_hosts= ( $hostname );
} else {
  read_cluster_allocation();
  if ($allocationmethod eq 'default') {
      @chosen_hosts= choose_hosts(int($optimi)+int($fbs));
  } elsif ($allocationmethod eq 'balanced') {
      @chosen_hosts = choose_balanced_hosts(int($optimi)+int($fbs));
  } elsif ($allocationmethod eq 'justthishost') {
      @chosen_hosts = choose_justthishost(int($optimi)+int($fbs));
  } else {
      die "Unsupported allocationmethod: $allocationmethod";
  }
  warn "chosen hosts are @chosen_hosts";
  write_cluster_allocation();
}

if ($#chosen_hosts < $#os + $#fs) { die "ERROR: not enough hosts chosen for all the specified optimi and fuzzbombs"; }

warn("creating ssh launcher file\n");
my $sub = "";
my $ampcount = 0;
my %used_hosts;
my $mm_host;

# give each optimus it's own slot
while (@os) {
  my $name = shift(@os);
  my $newhost = shift(@chosen_hosts);
  $used_hosts{$newhost}=1;
  warn "comparing $newhost and $main::hostname and $ENV{LOOPBACK}";
  if ($newhost eq $main::hostname && defined($ENV{LOOPBACK})) {
    $mm_host = $main::loopback;
  } else {
    $mm_host = $main::hostname;
  }
  my $args = "$mm_host $main::baseport $name";
  warn($args);
	# Note we prob dont have to set DOCKER_HOST anymore since tools/docker does it every time
  $sub .= "$main::cgc/code/tools/sshc $newhost CIRCA_MM_HOST=$mm_host DOCKER_HOST=$main::docker_host '$cgc/rsync/code/tools/docker exec $basename-neo-fuzz-ccl /realuser.sh bash --login -c \\\"$ENV{LACROSSE_HOME}/rsync/code/tools/run-ccl-agent $args\\\"' > $name.log 2>&1 &\n";
  ++$ampcount;
}
while (@fs) {
  my $fbs_in_slot = 0;
  while (@fs && $fbs_in_slot < $main::fuzzbombs_per_slot) {
    my $name = shift(@fs);
    my $newhost = shift(@chosen_hosts);
    $used_hosts{$newhost}=1;
    if ($newhost eq $main::hostname && defined($ENV{LOOPBACK})) {
      $mm_host = $main::loopback;
    } else {
      $mm_host = $main::hostname;
    }
    my $args = "$mm_host $main::baseport $name";
    $sub .= "$main::cgc/code/tools/sshc $newhost CIRCA_MM_HOST=$mm_host DOCKER_HOST=$main::docker_host '$cgc/rsync/code/tools/docker exec $basename-neo-fuzz-ccl /realuser.sh bash --login -c \\\"$ENV{LACROSSE_HOME}/rsync/code/tools/run-ccl-agent $args\\\"' > $name.log 2>&1 &\n";
    ++$fbs_in_slot;
    ++$ampcount;
  }
}

# retrofitting this stuff to the start of the doit.sub file string, to start docker containers once per used host
# note evil '--network host' so that container has host's hostname, for MM connections.  Port deconfliction via CIRCA_BASEPORT should be good enuf
# note that --network option now moved to start-neo-fuzz-ccl so always get it

# note that while we need to use the rsync/code at the host level, when we get inside the container
# the /neo-fuzz root is mounted to the rsync automatically, so no need to think about rsync level (and in fact if you add it, bad)
my $starts = "# ssh launcher for job $main::hostname:$jobdir\n";

# first make sure each host as rootless docker server started.... NOTE FIXME this should really be conditional on
# whether the system has rooted docker installed, but at this point SIFT has none of those so... imposs to test that.

foreach my $host (keys(%used_hosts)) {
  if (!($host eq $main::hostname)) {
    $starts .= "$main::cgc/code/tools/sshc $host $cgc/rsync/code/tools/start-docker-engine > start-docker-engine-$host.log 2>&1\n";
}}


foreach my $host (keys(%used_hosts)) {
  if ($host eq $main::hostname && defined($ENV{LOOPBACK})) {
    $mm_host = $main::loopback;
  } else {
    $mm_host = $main::hostname;
  }
  $starts .= "$main::cgc/code/tools/sshc $host CIRCA_BASENAME=$main::basename CIRCA_BASEPORT=$main::baseport CIRCA_MM_HOST=$mm_host DOCKER_HOST=$main::docker_host DOCKER_TAG=$main::docker_tag OPENAI_API_KEY=$ENV{'OPENAI_API_KEY'} POV_SERVER_IP=$ENV{'POV_SERVER_IP'} POV_SERVER_PORT=$ENV{'POV_SERVER_PORT'} WEBSWITCH_IP=$ENV{'WEBSWITCH_IP'} HOST_LOGDIR=$jobdir $cgc/rsync/code/tools/start-neo-fuzz-ccl -d > start-neo-fuzz-ccl-$host.log 2>&1\n";
}

foreach my $host (keys(%used_hosts)) {
  $starts .= "$main::cgc/code/tools/sshc $host DOCKER_HOST=$main::docker_host $cgc/rsync/code/tools/wait-for-container $basename-neo-fuzz-ccl > wait-for-neo-fuzz-ccl-$host.log 2>&1 &\n";
  #$starts .= "$main::cgc/code/tools/sshc $host DOCKER_HOST=$main::docker_host $cgc/rsync/code/tools/with-tool-path wait-for-container $basename-neo-fuzz-ccl > wait-for-neo-fuzz-ccl-$host.log 2>&1 \n";
}

$sub = $starts . "echo Finished wait-for-ctr\n" . $sub;

my $submitfile = "$jobdir/doit.sh";
write_filestring($submitfile, $sub);
warn("ssh driver file written to $submitfile\n");
chdir($jobdir);
chmod 0755, "./doit.sh";
my $cmd = "./doit.sh";
if ($justdontdoit) {
warn("NOT Running $cmd\n");
} else {
warn("Running $cmd\n");
my $results = `$cmd`;
}
#return($cluster);

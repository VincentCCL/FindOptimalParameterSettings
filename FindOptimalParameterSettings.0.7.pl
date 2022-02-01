# FindOptimalParamterSettings.pl
#------------------------------
$version='0.7'; # 01.05.2017 added possibility to replace first random initialization by predefined set of values (switch --d)
# 15.09.2016 object oriented version of the script
# 07.11.2016 better treatment of parameter effect

# created by Vincent Vandeghinste & Tom Vanallemeersch

# randomly initialize values of a command's parameters and perform smart local hill climbing according to the evaluation score assigned to the command's output
#
# usage: perl FindOptimalParameterSettings.0.7.pl [--b <maximal beam size>] [--r <number of random initializations> ] [--t <timeout in seconds> ] [--m <maximal memory used by command (in Mb)> ] [--d "<predefined set of values, space-delimited>"] "<command template>" "<evaluation template>" <output directory>
#
# the templates specify a number of parameter types and placeholders and write files to <output directory>;
# the command in <evaluation template> should print the following information to standard output: <score>[<tab><other information>]*; other output should be written to files within script
#
# examples:
#
# perl FindOptimalParameterSettings.0.4.pl \
#      "perl GetEquivNodes.pl --l [QLPAR arithmean geomean median] --i [QNPARf 0.1 1 0.1] in.xml lex.f2e lex.e2f [ALNOUT] [ALNLOG]" \
#      "EvalTreeAlnLASet.bash [ALNOUT] subtreealn [EVALOUT]" \
#      outdir
#
# -> this example <command template> uses two types of parameters, a qualitative parameter with two possible types (arithmean and median), and a quantitative
#    parameter (with f indicating that it is a float, not an integer) with range 0.1 to 1, of which we will test values 0.1, 0.2 etc. (interval 0.1)
# -> the command produces two output files, indicated above as [ALNOUT] and [ALNLOG] (arbitrary placeholder names); for instance, if the GetEquivNodes.pl command
#    is run on combination of "arithmean" and 0.5, files ALNOUT_arithmean_0.5 and ALNLOG_arithmean_0.5 are written to <output directory>, the command
#    EvalTreeAlnLASet.bash gets file <output directory>/ALNOUT_arithmean_0.5 as input, it writes files <output directory>/EVALOUT_arithmean_0.5, and prints score to standard output;
# -> each set of parameter values is printed to <output directory>/setscores with its score and the time needed for the command call; in case of a timeout, score printed is 0
#
# perl FindOptimalParameterSettings.0.4.pl \
#      "perl GetEquivNodes.pl --a [QNPAR 0 1 0.1 {L 1 1 1}] --b [QNPAR 0 1 0.1 {L 1 1 2}] --b [QNPAR 0 1 0.1 {L 1 3}] in.xml lex.f2e lex.e2f [ALNOUT] [ALNLOG]" \
#      "EvalTreeAlnLASet.bash [ALNOUT] subtreealn [EVALOUT]" \
#      outdir
#
#
# perl FindOptimalParameterSettings.0.4.pl \
#      "perl PictoToTextBatch.pl -d 5000 -p beta -l dutch -t [QNPAR 0 10 0.1] -h [QNPAR 1 40 1] -c [QNPAR 0 10 0.1 {2 <}] -r [QOPAR revlem11.db revlem21.db revlem31.db revlem36.db revlem41.db] > [PICTOUT]" \
#      "EvalTrans.bash [PICTOUT] [EVALOUT]" \
#      outdir
#      
# -> the last parameter in <command template> is a parameter with ordered values, i.e. local hill climbing follows the order of the values (in the first example, 
#    local hill climbing does not consider the order of arithmean, geomean and median); parameters are qualitative in this case, but may also be quantitive (useful if interval is not constant, e.g. exponentially growing values (0.1, 0.01, ...)
# -> the penultimate parameter is used as secondary sort criterion: if score of two configurations is equal, the first one is preferred to the second one if the parameter value is smaller
#    than that of the second one; it is also possibly to specify tertiary criterion (e.g. "... {3 >}"), etc. 
# -> the script PictoToTextBatch.pl treats a set of sentences at once (i.e. calls PictoToText.pl multiple times)
# -> EvalTrans.bash prints score and information on ngrams to standard output
# -> each set of parameter values is printed to <output directory>/<call counter>.info with its score, information on ngrams and the time needed for the command call; the command call itself is printed to <output directory>/<call counter>.cmdcall
#
# notes:
# - <maximal memory ...>: take account of fact that command may perform parallellization itself
# - do not add a space after [ and before ] in parameter description
# - if a space-delimited field in <command template> or <evaluation template> is followed by two tildes, the space following the field is removed in command call; similar for field starting with two tildes:
#   e.g. "perl SemTreeAln.pl --a \"~~ samepart [QNPAR 0.1 1 0.1] samecoarsetype [QNPAR 0.1 1 0.1] samefinetype [QNPAR 0.1 1 0.1] fallback [QNPAR 0.1 1 0.1] ~~\" ..."
# - <command template> may consist of multiple commands, s
use Getopt::Long;
use List::Util qw(min max shuffle);
use Data::Dumper;
#use strict;

$SIG{INT}=\&kill_running_pids;

my $origcall=join(' ',@ARGV); # to get the switches
my $maxbeam=50;
my $timeout=100000;
my $maxmemcmd=1024;
my $randominitnum=3;
my $paramPref=4;
my $predefvals;
#my $queuePref=5; # this is the number of parameters that gets priority treatment
my %already; # hash containing already processed param settings
GetOptions('b=i' => \$maxbeam, 
           't=i' => \$timeout, 
           'm=i' => \$maxmemcmd, # MB mem
           'r=i' => \$randominitnum,
   #        'q=i' => \$queuePref, # still used?
  #         'p=i' => \$paramPref, # nr of preferred parameters that are used (best scoring parameters) no longer used
           'd=s' => \$predefvals
           );

my ($cmdtemplate,$evaltemplate,$outdir)=@ARGV;

`rm -rf $outdir/*`;#  mkdir $outdir`;

open(LOG,">:utf8",$outdir."/logging") || die ("Can't open ".$outdir."/logging\n");

my @call_args=@ARGV;
for (0,1){ $call_args[$_]="\"".$call_args[$_]."\"" }
my $funccall="perl $0 ".substr($origcall,0,index($origcall,$ARGV[0])).join(' ',@call_args);
#print LOG "function call: ".$funccall."\n";
print LOG "start time: ".localtime()."\n";
my @params; 
my $cmdtemplate_placeholders; # [Q... ] is replaced by [PARAM<0-based counter>]
($paramNames)=&get_params($cmdtemplate);
my %conditions;
my %prefParams;
my $conditioncounter;
&search($paramNames);
print LOG "end time: ".localtime()."\n";
close(LOG);


#-----------------  FUNCTIONS
sub get_params{
    # reads in command line parameters
    my ($cmdtemplate)=@_;
    my $dbgfunc=1;
    my $inparamvalues=0;
    my @parts;
    my @withplaceholders;
    foreach my $part (split / /,$cmdtemplate){
	if ($part =~ /^\[Q[LNO]PARf?$/){
	    my $param=parameter->new;
	    if ($part=~/f$/) {
	      $param->{nrtype}='float';
	      $param->{type}= substr($part,1,-1);
	    }
	    else {
	      $param->{type} = substr($part,1);
	    }
	    if ($withplaceholders[-1]=~/^-/o) {
	      $param->{name} = substr($withplaceholders[-1],1);
	    }
	    elsif ($withplaceholders[-1]=~/=~~/) {
	      $param->{name} = substr($withplaceholders[-1],0,-3);
	    }
	    push @params,$param;
	    $inparamvalues=1;
	    @parts=();
	}
	elsif ($inparamvalues){
	    if ($part =~ /\]$/){
		$part=substr($part, 0, -1);
		$inparamvalues=0;
	    }
	    push @parts,$part;
	    unless ($inparamvalues){
		if ($params[$#params]->{'type'} =~ /^Q[LO]/){
		    $params[$#params]->{'vals'}=[ @parts ];
		}
		else {
		  $params[-1]->{'min'}=$parts[0];
		  $params[-1]->{'max'}=$parts[1];
		  $params[-1]->{'step'}=$parts[2];
# 		else {
# 		    if ($part =~ /^[<>]}$/){
# 			$params[$#params]{'sortcrit'}={ 'nth' => substr(@parts[@parts-2],1), 'order' => substr($part,0,2) };
# 			@parts=@parts[0..(@parts-2)];
# 		    }
# 		    # specify all values based on minimum, maximum and interval
# 		    my @values;
# 		    for (my $i=$parts[0];$i<=$parts[1];$i+=$parts[2]){
# 			push @{$params[$#params]{'vals'}},((($i+0.00001)/$parts[2])*$parts[2])-0.00001; # avoids floating point problems (e.g. ..., 5.8, 5.9, 5.999999, 6.099999, ...)
# 		    }
		}
		push @withplaceholders,"[PARAM".$#params."]";
	    }
	}
	else {
	    push @withplaceholders,$part;
	}
    }
    $cmdtemplate_placeholders=join(' ',@withplaceholders);
    return [@params];
}

sub search {
  my ($paramNames)=@_;
  my @q=&initializeQ($paramNames);
  while (@q>0) { 
     @q=&process([@q]);
  }
}

sub wait_procs_finished {
    my ($files)=@_;
    my $dbgfunc=0;

    my %finished;
    while ((keys %finished) < @$files){
	sleep 1;
	for my $file (@$files){
	    ($finished{$file}=1) && &rank_print_condition_files($file)."\n" if !exists($finished{$file}) && (-e $file);
	}
    }
}

 sub rank_print_condition_files{
  my ($file)=@_;
  push @conditionfiles,$file;
  my @table;
  for my $file (@conditionfiles){
    push @table,[ &getinfo($file) ];
  }
  @table=sort compare_conditions @table;
  if ($table[0][@params+1] > $bestcondition[@params+1]){
    @bestcondition=@{$table[0]};
  }
  for (my $i=0;$i<@table;$i++){
    for (my $j=0;$j<@{$table[$i]};$j++){
      $colwidths[$j]=max(($colwidths[$j] || 8),length($table[$i][$j].""));
    }
  }
#    my $toprint=$funccall."\n\n".$cmdtemplate_placeholders."\n\nFIELDS:\n\n";
#      # print some field headers
#      for (my $i=0;$i<@{$table[0]};$i++){
#  	$toprint.=sprintf("%".($colwidths[$i]+2)."s",(($i==0) ? "call" : ($i<=@params) ? "param".($i-1) : ($i==@params+1) ? "score" : ($i==@{$table[0]}-2) ? "dur" : ($i==@{$table[0]}-1) ? "sco/dur" : " "));
#      }
#      $toprint.="\n\nBEST CONDITION\n\n";
#      for (my $i=0;$i<@bestcondition;$i++){
#  	$toprint.=sprintf("%".($colwidths[$i]+2)."s",$bestcondition[$i]);
#      }
#      $toprint.="\n\nBEAM\n\n";
#      for my $row (@table){
#  	for (my $i=0;$i<@$row;$i++){
#  	    $toprint.=sprintf("%".($colwidths[$i]+2)."s",$$row[$i]);
#  	}
#  	$toprint.="\n";
#      }
#    #  print $toprint."\n";
#      print LOG $toprint."\n";
}

sub compare_conditions{
    return -1 if $$b[@params+1]<$$a[@params+1];
    return 1 if $$b[@params+1]>$$a[@params+1];
    for (my $nth=2;;$nth++){
	my $foundnth;
	for (my $i=0;$i<@params;$i++){
	    if (exists($params[$i]{'sortcrit'}) && ($params[$i]{'sortcrit'}{'nth'}==$nth)){
		if ($$a[$i+1]!=$$b[$i+1]){
		    return (($params[$i]{'sortcrit'}{'nth'} eq '<') ? (($$b[$i+1]<$$a[$i+1]) ? 1 : -1) : ($$b[$i+1]>$$a[$i+1]) ? 1 : -1);
		} else {
		    ($foundnth=1) && last;
		}
	    }
	}
	return 0 unless $foundnth
    }
}
  
sub process {
  my ($q)=@_;
  my (@newQ,@infofiles,@conditions);
  my $procnum;
  our $bestscore;
  my $maxproc=&get_maxproc;
  my @prefQ=splice(@$q,0,$maxproc);
  foreach (@prefQ) {
    my ($cond,$infofile)=$_->run;
    push(@infofiles,$infofile);
    push(@conditions,$cond);
    my $params=$_->{parameters};
    $already{join(" ",@$params)}=1;
    if (++$procnum==$maxproc){
      &wait_procs_finished([ @infofiles[ @infofiles-$procnum .. $#infofiles ] ]);
      last;
      #$procnum=0;
    }
  }
  &wait_procs_finished([ @infofiles[ @infofiles-$procnum .. $#infofiles ] ]) if $procnum;
  for (my $i=0;$i<@conditions;$i++) {
    my @info=&getinfo($infofiles[$i]);
    $conditions[$i]->{score}=$info[@params+1];
    $conditions[$i]->{'other info'}=$info[@params+2];
    $conditions[$i]->{'time'}=$info[@params+3];
    $conditions[$i]->{'scorebytime'}=$info[@params+4];
    $conditions[$i]->printToScreen;
    $conditions[$i]->printToLog(\*LOG);
  }
  @conditions=sort {$b->{'score'}  <=> $a->{'score'}} @conditions;
  my $bestflag=undef;
  foreach (@conditions) {
    if ($_->{score} > $bestscore) { 
      $bestscore=$_->{score};
      $params=$_->{parameters};
      $bestcondition=$_;
      $bestflag=1;
      print STDERR "New High Score: $bestscore in condition ".$_->{id}." with values ".join('/',@$params)."\n";
      print LOG "New High Score: $bestscore in condition ".$_->{id}." with values ".join('/',@$params)."\n";
    }
    &storePrefParams($_);
  }
  unless ($bestflag) {
    $bestscore=$bestcondition->{score};
    $bestid=$bestcondition->{id};
    $bestparam=$bestcondition->{parameters};
    print STDERR "No new high score: $bestscore still in condition ".$bestid." with values ".join('/',@$bestparam)."\n";
  }
  my ($prevscore,@restQ);
  $prevscore=$conditions[0]->{score};
  my @params=&findPrefParams;
  foreach (@conditions) {
    if ($prevscore==$_->{score} or
        @restQ<=$maxbeam) {
      push(@restQ,$_->generateVariants([@params]));
      $prevscore=$_->{score};
    }
  }
  push(@$q,@restQ);
  # sort q by score of previous
  @q=sort {$b->{prev}->{score} <=> $a->{prev}->{score}} @$q;
  if (@q>$maxbeam) { 
    my $maxbeamscore=$q[$maxbeam-1]->{prev}->{score};
    for (my $i=$maxbeam;$i<@q;$i++) {
      if ($q[$i]->{prev}->{score} < $maxbeamscore) {
        @q=@q[0..$i-1];
        last;
      }
    }
  }
  return @q;
}

sub get_maxproc {
    my $totavailmemnode=`free -m | gawk '(NR==3){ print \$4 }'`; # the "free" value of the "+/- buffers/cache" line, expressed in Mb
    my $nproc=1; # hardcoded for cluster
    return ((int($totavailmemnode/$maxmemcmd) < $nproc) ? int($totavailmemnode/$maxmemcmd) : $nproc);
}

sub storePrefParams {
  my ($cond)=@_;
  if (my $prevcond=$cond->{'prev'}) {
    my ($paramName,$dir,$score,$time,$nr)=$cond->findDifference($prevcond);
    if ($value=$prefsTracking{$paramName}) { push(@$value,[$dir,$score,$time,$nr]);}
    else { $prefsTracking{$paramName}=[[$dir,$score,$time,$nr]];}
  }
}

sub getPrefParams {
  # returns an ordered list of params
  my ($cond)=@_;
  my $newPrefs;
  if (@params) {
    print LOG "Pref Params: ";
    foreach (@params) {
      $newPrefs->{$_->[0]}=$_->[1];
      print LOG $_->[0].":".$_->[1]."\t";
    }
    print LOG "\n";
  }
  else {
    # set all params as prefParams
    $params=$cond->{paramNames};
    for (my $i=0;$i<@$params;$i++) {
      $newPrefs->{$params->[$i]->{name}}=1;
    }
  }    
  return $newPrefs;
}

sub findPrefParams {
  # takes a hash as input and calculates for every key/value pair
  # the average rise or descent in score per parameter change
  my (@queue);
  foreach (keys %prefsTracking) {
    $changes=$prefsTracking{$_};
    my ($dir,$avgscore,$avgtime,$nr)=&getAverageScoreAndTime($changes);
    if ($avgscore==0) { # This param did not change the score
      next;
    }
    if ($avgscore<0) { 
      $dir=&main::invDir($dir);
      $avgscore=-$avgscore;
      $avgtime=-$avgtime;
    }
    push(@queue,[$_,$dir,$avgscore,$avgtime,$nr]);
  }
  @queue=sort {$b->[2] <=> $a->[2]} @queue;
  if (@queue>$paramPref) {
      # cutoff 
    @queue=@queue[0..$paramPref-1];
  }
  return @queue;
}

sub getAverageScoreAndTime {
  my ($series)=@_;
  my ($dir,$diffScore,$diffTime,$totScore,$totTime);
  foreach (@$series) {
    ($dir,$diffScore,$diffTime,$i)=@$_;
    $totScore+=$diffScore;
    $totTime+=$diffTime;
  }
  return ($dir,$totScore/@$series,$totTime/@$series,$i);
}
 
sub initializeQ {
  my ($paramNames)=@_;
  my (@q);
  for (my $i=0;$i<$randominitnum;$i++) {
    my (@paramvalues);
    my $q=condition->new({'id' => $conditioncounter++});
    $conditions{$q->{id}}=$q;
    foreach (@params) {
      if ($_->{type} eq 'QLPAR') {
        push(@paramvalues,$_->{vals}->[int(rand(@{$_->{'vals'}}))]);
      }
      elsif ($_->{type} eq 'QNPAR') {
        if ($_->{min}<0) {
          $shiftedMax=$_->{max}+abs($_->{min});
          if ($_->{nrtype} eq 'float') {
            push(@paramvalues,sprintf("%.2f",rand($shiftedMax)-abs($_->{min})));
          }
          else {
            push(@paramvalues,int(rand($_->{max}+1))-$_->{min});
          }
        }
        else {
          if ($_->{nrtype} eq 'float') {
            push(@paramvalues,sprintf("%.2f",rand($_->{max}-$_->{min})+$_->{min}));
          }
          else {
            push(@paramvalues,int(rand($_->{max}-$_->{min}+1))+$_->{min});
          }
        }
      }
    }
    @paramvalues=split / /,$predefvals if ($i==0) && $predefvals;
    unless ($already{join(" ",@paramvalues)}) {
      $q->{paramNames}=$paramNames;
      $q->{parameters}=[@paramvalues];
      push(@q,$q);
    }
  }
  return @q;
}

sub getinfo {
    my ($filename)=@_;
    open(IN, '<:encoding(UTF-8)', $filename) or die "Could not open file $filename";
    my $line=<IN>;
    chomp $line;
    my @info=split /\t/,$line;
    return @info;
}

sub invDir {
  my ($dir)=@_;
  # reverses direction
  if ($dir eq '-') { return '+';}
  elsif ($dir eq '+') { return '-';}
  elsif ($dir eq '1') { return 1;}
  else {
    my ($src,$tgt)=split(/->/o,$dir);
    return $tgt.'->'.$src;
  }
}
#------------------------------------------- METHODS
package condition;

sub new {
  my ($class,$args)=@_;
  my $pkg=bless{%$args},$class;
  return $pkg;
}

sub run {
  my ($condition)=@_;
  my $cmd=$cmdtemplate_placeholders;
  my $paramvalues=$condition->{parameters};
  foreach (@$paramvalues) {
    $cmd=~s/ \[PARAM\d+\]/ $_/;
  }
  my $valuestr=join("\t",@$paramvalues);
  my $evalcmd=$evaltemplate;
  my $countindent=sprintf("%05d",$condition->{id});
  foreach ($cmd,$evalcmd){
    $_=~s/ \[([^ ]+)\]/ $outdir\/$countindent.\1/g;
    $_=~s/~~ //g;
    $_=~s/ ~~//g;
  }
  my $torun="(starttime=`date +%s`; echo '$cmd' > $outdir/$countindent.cmdcall; $cmd; endtime=`date +%s`; duration=`expr \$endtime - \$starttime`; res=`$evalcmd`; score=`echo -e \"\$res\" | sed 's/\t.*\$//'`; scorebytime=`gawk \"BEGIN { print \$score / \$duration }\"`; echo -e \"$callcounter\\t$valuestr\\t\$res\\t\$duration\\t\$scorebytime\" > $outdir/$countindent.info)& echo \$\! > $outdir/$countindent.pid";
  system($torun);
  my @info=&main::getinfo($outdir."/".$countindent.".pid");
  my $pid=$info[0];
  $torun="(if [ \"`/user/leuven/310/vsc31067/Scripts/watch_time_mem.bash $pid $timeout $maxmemcmd`\" == \"killed\" ]; then echo -e \"$callcounter\\t$valuestr\\t0\\tTIMEORMEMOUT\" > $outdir/$countindent.info; fi)&";
  #print LOG $torun."\n";
  system($torun);
  return ($condition,$outdir."/".$countindent.".info");
}

sub generateVariants {
  my ($oldCondition,$prefParams)=@_;
  my (@newConditions,%treatedParams,@unpreferred);
  # treat params in order of preference
  foreach (@$prefParams) {
    my ($name,$dir,$score,$time,$paramNr)=@$_;
    push(@newConditions,$oldCondition->variate($paramNr,$dir));
    $treatedParams{$paramNr}=$dir;
  }
  # treat rest of params and directions
  my $oldparams=$oldCondition->{parameters};
  for (my $i=0;$i<@$oldparams;$i++) {
    if (my $prevdir=$treatedParams{$i}) {
      push(@unpreferred,$oldCondition->variate($i,&main::invDir($dir)));
    }
    else {
      push(@unpreferred,$oldCondition->variate($i,1));
    }
  }
  #my @shuffled=shuffle @unpreferred; # doesnt work
  #push(@newConditions,@shuffled);
  push(@newConditions,@unpreferred);
  return @newConditions;
}
  
sub generateVariants_ {
  my ($oldcondition,$prefParams)=@_;
  #my $prefParams=&main::getPrefParams($oldcondition);
  my (@newConditions,@unpreferredNew);
  my $paramNames=$oldcondition->{paramNames};
  # find most promising feature variation directions
  my $oldparams=$oldcondition->{parameters};
  for (my $i=0;$i<@$oldparams;$i++) {
    if ($dir=$prefParams->{$paramNames->[$i]->{'name'}}) {
      if ($dir eq '1') {
        push(@unpreferredNew,$oldcondition->variate($i,1));
      }
      else {
        # generate new variant
        push(@newConditions,$oldcondition->variate($i,$dir));
        # other direction in @unpreferredNew
        push(@unpreferredNew,$oldcondition->variate($i,&main::invDir($dir)));
      }
    }
    else {
      push(@unpreferredNew,$oldcondition->variate($i,1));
    }
  }
  push(@newConditions,@unpreferredNew);
  return @newConditions;
}

sub variate {
  # generates variants on param[$paramNr]
  my ($cond,$paramNr,$dir)=@_;
  my @newConds;
  my $paramVal=$cond->{parameters}->[$paramNr];
  my $paramVals=$cond->{parameters};
  if ($cond->{paramNames}->[$paramNr]->{type} eq 'QLPAR') {
    # qualitative parameters
    $allValues=$cond->{paramNames}->[$paramNr]->{vals};
    for (my $i=0;$i<@$allValues;$i++) {
      if ($paramVal ne $allValues->[$i]) {
        if (($dir eq '1') or
            ($dir eq $paramVal.'->'.$allValues->[$i])) {
          @newVals=@$paramVals;
          splice(@newVals,$paramNr,1,$allValues->[$i]);
          if (!defined($already{join(' ',@newVals)})) {
            push(@newConds,condition->new({'id' => $conditioncounter++,
                                        'paramNames' => $cond->{paramNames},
                                        'parameters' => [@newVals],
                                        'prev' => $cond}));
            last;
          }
        }
      }
    }
  }
  # quantitative parameters
  elsif ($cond->{paramNames}->[$paramNr]->{type} eq 'QNPAR') {
    if (($dir eq '1') or ($dir eq '+')) {
      # add 1 step
      $newparamVal=$paramVal+$cond->{paramNames}->[$paramNr]->{step};
      if ($newparamVal <= $cond->{paramNames}->[$paramNr]->{max}) {
        @newVals=@$paramVals;
        splice(@newVals,$paramNr,1,$newparamVal);
        if (!defined($already{join(' ',@newVals)})) {
          push(@newConds,condition->new({'id' => $conditioncounter++,
                                        'paramNames' => $cond->{paramNames},
                                        'parameters' => [@newVals],
                                        'prev' => $cond}));
        }
      }
    }
    if (($dir eq '1') or ($dir eq '-')) {
      # subtract 1 step
      $newparamVal=$paramVal-$cond->{paramNames}->[$paramNr]->{step};
      if ($newparamVal >= $cond->{paramNames}->[$paramNr]->{min}) {
        @newVals=@$paramVals;
        splice(@newVals,$paramNr,1,$newparamVal);
        if (!defined($already{join(' ',@newVals)})) {
          push(@newConds,condition->new({'id' => $conditioncounter++,
                                        'paramNames' => $cond->{paramNames},
                                        'parameters' => [@newVals],
                                        'prev' => $cond}));
        #$already{join(' ',@newVals)}=1;                    
        }
      }
    }
  }
  return @newConds;
}
 
sub findDifference {
  my ($cond,$prevcond)=@_;
  my $condp=$cond->{parameters};
  for (my $i=0;$i<@$condp;$i++) {
    if ($cond->{parameters}->[$i] ne $prevcond->{parameters}->[$i]) {
    # difference found !!
      $nr=$i;
      $name=$params[$i]->{name};
      if ($params[$i]->{type} eq 'QNPAR') {
        $dir='+';
        if ($cond->{parameters}->[$i] > $prevcond->{parameters}->[$i]) {
          $score=$cond->{score}-$prevcond->{score};
          $time=$cond->{time}-$prevcond->{time};
        }
        else { 
          # invert the value
          $score=-($cond->{score}-$prevcond->{score});
          $time=-($cond->{time}-$prevcond->{time});
        }
      }
      else { # QLPAR STILL SOMETHING WRONG HERE?
        if ($prevcond->{parameters}->[$i] lt $cond->{parameters}->[$i]) { 
          $dir=$prevcond->{parameters}->[$i]."->".$cond->{parameters}->[$i];
          $score=$cond->{score}-$prevcond->{score};
          $time=$cond->{time}-$prevcond->{time};
        }
        else { 
          # invert the value
          $dir=$cond->{parameters}->[$i]."->".$prevcond->{parameters}->[$i];
          $score=$prevcond->{score}-$cond->{score};
          $time=$prevcond->{time}-$cond->{time};
        }
      }
      last;
    }
  }
  return ($name,$dir,$score,$time,$nr);
}

sub printToScreen {
  my ($cond)=@_;
  my $prevcond=$cond->{prev};
  if ($prevcond) { $prevcondid=$prevcond->{id};}
  my $paramVals=$cond->{parameters};
  if ($prevcond) {
    print STDERR join("\t",("Condition ".$cond->{id},"Score ".$cond->{score},"Prev ".$prevcondid,@$paramVals))."\n";
  }
  else {
    print STDERR join("\t",("Condition ".$cond->{id},"Score ".$cond->{score},@$paramVals))."\n";
  }
}

sub printToLog {
  my ($cond,$log)=@_;
  my $prevcond=$cond->{prev};
  if ($prevcond) { $prevcondid=$prevcond->{id};}
  my $paramVals=$cond->{parameters};
  if ($prevcond) {
    print $log join("\t",("Condition ".$cond->{id},"Score ".$cond->{score},"Prev ".$prevcondid,@$paramVals))."\n";
  }
  else {
    print $log join("\t",("Condition ".$cond->{id},"Score ".$cond->{score},@$paramVals))."\n";
  }
}
#-----------------------------------------------
package parameter;
sub new {
  my ($class,$args)=@_;
  my $pkg=bless{%$args},$class;
  return $pkg;
}

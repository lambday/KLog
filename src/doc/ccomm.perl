# download and install this package:
# http://search.cpan.org/~rkrimen/String-Comments-Extract-0.02/

use String::Comments::Extract;

open FILE, "$ARGV[0]" or die "Couldn't open file: $!";
my @src = <FILE>;
close FILE;
foreach $line (@src)  {
    $str = "$str" ."$line";
}

# print $str;


$comments = String::Comments::Extract::C->extract($str);
$comments =~ s/\/\/dontremoveme//g;
$comments =~ s/\/\/\//%/g;
$comments =~ s/\/\//%/g;

print $comments;


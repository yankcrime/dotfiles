#!/usr/bin/perl -w
### Rewritten a whole lot, and customized for UIUC, by Tim Skirvin 
### Tue Nov 11 14:40:50 CST 2003 
#
# mutt_ldap_query.pl 
# version 1.2

#
# Historique
#   2001/09/12 : pda : conversion utf8
#

# Copyright (C) 4/14/98 Marc de Courville <marc@courville.org>
#       but feel free to redistribute it however you like.
#
# mutt_ldap_query.pl: perl script to parse the outputs of ldapsearch
# (ldap server query tool present in ldap-3.3 distribution
# http://www.umich.edu/~rsug/ldap) in order to pass the required
# formatted data to mutt (mail client http://www.mutt.org/)
# using Brandon Long's the "External Address Query" patch
# (http://www.fiction.net/blong/programs/mutt/#query).
#
# Warren Jones <wjones@tc.fluke.com> 2-10-99
#    o Instead of just matching "sn", I try to match these fields
#      in the LDAP database: "cn", "mail", "sn" and "givenname".
#      A wildcard is used to make a prefix match.  (I borrowed
#      this query from pine.)
#
#    o Commas separating command line arguments are optional.
#      (Does mutt really start up $query_command with comma
#      separated args?)
#
#    o Streamlined the perl here and there.  In particular,
#      I used paragraph mode to read in each match in a single
#      chunk.
#
#    o Added "use strict" and made the script "-w" safe.
#
#    o Returned non-zero exit status for errors or when there
#      is no match, as specified by the mutt docs.
#
#    o Explicitly close the pipe from ldapsearch and check
#      error status.
#  

use strict;

use MIME::Base64;

# Please change the following 2 lines to match your site configuration
#
my $ldap_server = "sun-ds";
my $BASEDN = "dc=Sun,dc=Com";           
my @toget = qw( sn givenName mail telephoneNumber eduPersonPrimaryAffiliation 
	title uiucEduHomeDeptName uiucEduPhInactiveDate uiucEduCurriculum );

# Fields to search in the LDAP database:
#
my @fields = qw(cn mail sn givenname);

die "Usage: $0 <name_to_query>, [[<other_name_to_query>], ...]\n"
    if ! @ARGV;

$/ = '';	# Paragraph mode for input.
my @results;

my @queries;
foreach my $askfor ( @ARGV ) {
    $askfor =~ s/,$//;	# Remove optional trailing comma.

    my $query = join '', map { "($_=$askfor*)" } @fields;
    
    push @queries, join('', "(|", $query, ")");
}

my $query = join('', '(&', @queries, ')');

    my $command = "ldapsearch -x -L -h $ldap_server -b '$BASEDN' '$query'" .
		join(" ", '', @toget); 

    # print "$command\n";
    open( LDAPQUERY, "$command |" ) or 
		(warn "LDAP query error: $!\n" && next); 

    while ( <LDAPQUERY> ) {
	# print "[$_]\n";

	my $parag = $_ ;
	while ($parag =~ /^([a-z]+):: (.*)$/im)
	{
	    my $attr = $1 ;
	    my $val = $2 ;
	    $val = decode_base64($val) ;
	    $parag =~ s/$1:: $2/$attr: $val/im ;
	    $_ = $parag ;
	    # print "$_\n";
	}

	next if ! /^mail: *(.*)$/im;
	my $email = $1;
	my $phone = /^telephoneNumber: *(.*)$/im ? $1 : '';
	my ( @name ) = ( /^givenName: *(.*)$/im, /^sn: *(.*)$/im );
        my $type = /^eduPersonPrimaryAffiliation:\s*(.*)\s*$/im 
			? ucfirst $1 : 'Unaffiliated';
	my $title = /^title:\s*(.*)$/im ? $1 : "";
	my $dept =  /^uiucEduHomeDeptName:\s*(.*)$/im ? $1 : "";
	   $dept =~ s/campus information technologies & educ services/CITES/i;
	   $dept =~ s/natural resources and environmental sciences/NRES/i;
	my $leftuiuc = /^uiucEduPhInactiveDate:\s*(.*)$/im 
			? $1 : "";
	my $curric = /^uiucEduCurriculum:\s*(.*)$/im ? lc $1 : "Unknown";
	if ($type eq 'Unaffiliated' && $leftuiuc =~ /^(\d\d\d\d)(\d\d)$/) { 
          $type = "left_uiuc ($1/$2)";
	} elsif ($type eq 'Student') { $type = "Student ($curric)"; }
	my @tojoin = ( $title || $type || "Unknown" );
	push @tojoin, $dept if $dept;
        my $string = sprintf ("%s\t%s\t%s\n", $email, join(" ", @name), join(', ', @tojoin) );
	# push @results, "$email\t@name\t$phone\n";
	push @results, $string;
    }

    close( LDAPQUERY ) or (warn "ldapsearch failed: $!\n");


print "LDAP query: found ", scalar(@results), "\n", @results;
exit 1 if ! @results;

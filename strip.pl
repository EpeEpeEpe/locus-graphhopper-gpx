#!/usr/bin/perl -CSAD

use Modern::Perl '2015';
use utf8;

while (<>) {
	s%<name>\K(?:Na kruhovém.*?směrem na |Odbočte (?:mírně |ostře )?(?:vlevo|vpravo|doleva|doprava)(?: na )?|Pokračujte(?: na )?)(.*</name>)%$1%;
	print;
}

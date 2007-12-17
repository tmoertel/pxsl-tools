#!/usr/bin/perl -lp
#
# Copyright (C) 2003 Thomas Moertel <tom@moertel.com>.
# The PXSL toolkit is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.
#
# The text of the GNU GPL may be found in the LICENSE file,
# included with this software.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Except as provided for under the terms of the GNU GPL, all rights
# are reserved worldwide.

BEGIN { print <<EOF;
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
        "http://www.w3.org/TR/html4/strict.dtd">
<html lang=en>
<head>
<title>PXSL README</title>
<meta name="Content-Type" content="text/html; encoding=ISO-8559-1">
<style type="text/css">
body { background-color: #ffc; }
tt { color: #036; }
</style>
</head><body>
EOF
}

s/&/&amp;/g;
s/</&lt;/g;
s/>/&gt;/g;
s/\(C\)/&copy;/g;

my $line = $_;

print "<h1>Parsimonious XML Shorthand Language</h1>\n\n" unless $first++;

if (/^\s/) {
    s/ /&nbsp;/g;
    s/&nbsp;&nbsp;(?=&nbsp;)/&nbsp; /g;
    $_ = "<tt class=ind>$_<br></tt>";
}
else {
    if (s/^(\*+)//) {
        my $depth = 1 + length $1;
        (my $id = lc $_) =~ tr/a-z0-9/-/cs;
        $id =~ s/^-*(.+?)-*$/$1/s;
        $_ = "<h$depth id='$id'>$_</h$depth>";
    }
    else {
        s{(\S+)}{my$a=$1;$a=~/(^-)|(^,)|&lt;|&gt;|=/ ? "<tt>$a</tt>" : $1}eg;
    }
}

s!(http:[^ \r\n<]+)!<a href="$1">$1</a>!g;

$_ = "<p>$_" if ($prev_line_was_blank && $line =~ /\S/ && !/^<h/);
$prev_line_was_blank = ! $line =~ /\S/;

END { print "</body></html>" }

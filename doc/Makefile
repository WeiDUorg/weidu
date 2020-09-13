# This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
# starting from 18 December 2012 and WeiDU 231.06.

# Note added due to LGPL terms.
#
# This file was edited by Valerio Bigiani, AKA The Bigg, starting from
# 6 November 2005. All changes for this file are listed in
# diffs/doc.Makefile.diff file, as the output of a diff -Bw -c -N command.
#
# It was originally taken from Westley Weimer's WeiDU 185.

all: ../README-WeiDU.html clean

../README-WeiDU.html: base.tex
	perl -p -e "s/([A-Z2*])!/\1_/g" base.tex > README.tex
		(hevea article.hva fancysection.hva -fix -O README.tex) 2>&1 | grep -v "occuring outside math mode"
	cat README.html | sed -e "s/&ndash;/--/g" -e's/&#X2013;/--/g' -e 's/–/--/g' > README-WeiDU.html
	perl -pi -e "s/^a:.+//g" README-WeiDU.html # HeVeA colors a elements green; we don't want that
	perl -pi -e "s/flnumber/#/g" README-WeiDU.html # For some reason this breaks with an escaped symbol
	mv README-WeiDU.html ..

clean:
	${RM} README.*

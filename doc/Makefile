# This file has been edited by Fredrik Lindgren, a.k.a. Wisp,
# starting from 18 December 2012 and WeiDU 232.

# Note added due to LGPL terms.
#
# This file was edited by Valerio Bigiani, AKA The Bigg, starting from
# 6 November 2005. All changes for this file are listed in
# diffs/doc.Makefile.diff file, as the output of a diff -Bw -c -N command.
#
# It was originally taken from Westley Weimer's WeiDU 185.

all: README.html clean

README.html: base.tex
	perl -p -e "s/([A-Z2*])!/\1_/g" base.tex > README-WeiDU.tex
		(hevea article.hva fancysection.hva -fix -O README-WeiDU.tex) 2>&1 | grep -v "occuring outside math mode"
	mv README-WeiDU.html ../README.html

clean:
	${RM} README-WeiDU.*

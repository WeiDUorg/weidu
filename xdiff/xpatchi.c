/*
 *  LibXDiff by Davide Libenzi ( File Differential Library )
 *  Copyright (C) 2003	Davide Libenzi
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Davide Libenzi <davidel@xmailserver.org>
 *
 */

#include "xinclude.h"



typedef struct s_recinfo {
	char const *ptr;
	long size;
} recinfo_t;

typedef struct s_recfile {
	mmfile_t *mf;
	long nrec;
	recinfo_t *recs;
} recfile_t;

typedef struct s_hunkinfo {
	long s1, s2;
	long c1, c2;
	long cmn, radd, rdel;
} hunkinfo_t;

typedef struct s_patch {
	recfile_t rf;
	hunkinfo_t hi;
	long hkrec;
	long hklen;
} patch_t;

typedef struct s_patchstats {
	long adds, dels;
} patchstats_t;



static int xdl_load_hunk_info(char const *line, long size, hunkinfo_t *hki);
static int xdl_init_recfile(mmfile_t *mf, recfile_t *rf);
static void xdl_free_recfile(recfile_t *rf);
static char const *xdl_recfile_get(recfile_t *rf, long irec, long *size);
static int xdl_init_patch(mmfile_t *mf, patch_t *pch);
static void xdl_free_patch(patch_t *pch);
static int xdl_load_hunk(patch_t *pch, long hkrec);
static int xdl_first_hunk(patch_t *pch);
static int xdl_next_hunk(patch_t *pch);
static int xdl_hunk_match(recfile_t *rf, long irec, patch_t *pch, int mode);
static int xdl_find_hunk(recfile_t *rf, long ibase, patch_t *pch, int mode,
			 long *hkpos);
static int xdl_flush_section(recfile_t *rf, long start, long top, xdemitcb_t *ecb);
static int xdl_apply_hunk(recfile_t *rf, long hkpos, patch_t *pch, int mode,
			  long *ibase, xdemitcb_t *ecb, patchstats_t *ps);
static int xdl_reject_hunk(recfile_t *rf, patch_t *pch, int mode,
			   xdemitcb_t *rjecb, patchstats_t *ps);




static int xdl_load_hunk_info(char const *line, long size, hunkinfo_t *hki) {
	char const *next;

	if (memcmp(line, "@@ -", 4))
		return -1;
	line += 4;
	size -= 4;

	for (; size && !XDL_ISDIGIT(*line); size--, line++);
	if (!size || !XDL_ISDIGIT(*line))
		return -1;
	hki->s1 = xdl_atol(line, &next) - 1;
	size -= next - line;
	line = next;

	for (; size && !XDL_ISDIGIT(*line); size--, line++);
	if (!size || !XDL_ISDIGIT(*line))
		return -1;
	hki->c1 = xdl_atol(line, &next);
	size -= next - line;
	line = next;

	for (; size && !XDL_ISDIGIT(*line); size--, line++);
	if (!size || !XDL_ISDIGIT(*line))
		return -1;
	hki->s2 = xdl_atol(line, &next) - 1;
	size -= next - line;
	line = next;

	for (; size && !XDL_ISDIGIT(*line); size--, line++);
	if (!size || !XDL_ISDIGIT(*line))
		return -1;
	hki->c2 = xdl_atol(line, &next);
	size -= next - line;
	line = next;

	return 0;
}


static int xdl_init_recfile(mmfile_t *mf, recfile_t *rf) {
	long narec, nrec, bsize;
	recinfo_t *recs, *rrecs;
	char const *blk, *cur, *top, *eol;

	narec = xdl_guess_lines(mf);
	if (!(recs = (recinfo_t *) malloc(narec * sizeof(recinfo_t)))) {

		return -1;
	}

	nrec = 0;
	if ((cur = blk = xdl_mmfile_first(mf, &bsize)) != NULL) {
		for (top = blk + bsize;;) {
			if (cur >= top) {
				if (!(cur = blk = xdl_mmfile_next(mf, &bsize)))
					break;
				top = blk + bsize;
			}
			if (nrec >= narec) {
				narec *= 2;
				if (!(rrecs = (recinfo_t *) realloc(recs, narec * sizeof(recinfo_t)))) {

					free(recs);
					return -1;
				}
				recs = rrecs;
			}

			recs[nrec].ptr = cur;
			if (!(eol = memchr(cur, '\n', top - cur)))
				eol = top - 1;
			recs[nrec].size = (long) (eol - cur) + 1;
			nrec++;
			cur = eol + 1;
		}
	}

	rf->mf = mf;
	rf->nrec = nrec;
	rf->recs = recs;

	return 0;
}


static void xdl_free_recfile(recfile_t *rf) {

	free(rf->recs);
}


static char const *xdl_recfile_get(recfile_t *rf, long irec, long *size) {

	if (irec < 0 || irec >= rf->nrec)
		return NULL;

	*size = rf->recs[irec].size;

	return rf->recs[irec].ptr;
}


static int xdl_init_patch(mmfile_t *mf, patch_t *pch) {

	if (xdl_init_recfile(mf, &pch->rf) < 0) {

		return -1;
	}

	pch->hkrec = 0;
	pch->hklen = 0;

	return 0;
}


static void xdl_free_patch(patch_t *pch) {

	xdl_free_recfile(&pch->rf);
}


static int xdl_load_hunk(patch_t *pch, long hkrec) {
	long size, i;
	char const *line;

	for (;; hkrec++) {
		pch->hkrec = hkrec;
		if (!(line = xdl_recfile_get(&pch->rf, pch->hkrec, &size)))
			return 0;
		if (*line == '@')
			break;
	}
	if (xdl_load_hunk_info(line, size, &pch->hi) < 0) {

		return -1;
	}

	pch->hi.cmn = pch->hi.radd = pch->hi.rdel = 0;
	for (i = pch->hkrec + 1;
	     (line = xdl_recfile_get(&pch->rf, i, &size)) != NULL; i++) {
		if (*line == '@' || *line == '\n')
			break;
		if (*line == ' ')
			pch->hi.cmn++;
		else if (*line == '+')
			pch->hi.radd++;
		else if (*line == '-')
			pch->hi.rdel++;
		else {

			return -1;
		}
	}
	if (pch->hi.cmn + pch->hi.radd != pch->hi.c2 ||
	    pch->hi.cmn + pch->hi.rdel != pch->hi.c1) {

		return -1;
	}

	pch->hklen = i - pch->hkrec - 1;

	return 1;
}


static int xdl_first_hunk(patch_t *pch) {

	return xdl_load_hunk(pch, 0);
}


static int xdl_next_hunk(patch_t *pch) {

	return xdl_load_hunk(pch, pch->hkrec + pch->hklen + 1);
}


static int xdl_hunk_match(recfile_t *rf, long irec, patch_t *pch, int mode) {
	long i, j, fsize, psize, ptop;
	char const *fline, *pline;

	for (i = irec, j = pch->hkrec + 1, ptop = pch->hkrec + 1 + pch->hklen;
	     i < rf->nrec && j < ptop; i++, j++) {
		for (; j < ptop; j++) {
			if (!(pline = xdl_recfile_get(&pch->rf, j, &psize)))
				return 0;
			if (*pline == ' ' || *pline == mode)
				break;
		}
		if (j == ptop)
			break;
		if (!(fline = xdl_recfile_get(rf, i, &fsize)))
			return 0;
		if (fsize != --psize || memcmp(fline, pline + 1, fsize))
			return 0;
	}
	for (; j < ptop; j++)
		if (!(pline = xdl_recfile_get(&pch->rf, j, &psize)) ||
		    *pline == ' ' || *pline == mode)
			return 0;

	return 1;
}


static int xdl_find_hunk(recfile_t *rf, long ibase, patch_t *pch, int mode,
			 long *hkpos) {
	long hpos, hlen, i, j;
	long pos[2];

	hpos = mode == '-' ? pch->hi.s1: pch->hi.s2;
	hlen = mode == '-' ? pch->hi.cmn + pch->hi.rdel: pch->hi.cmn + pch->hi.radd;
	if (xdl_hunk_match(rf, hpos, pch, mode)) {
		*hkpos = hpos;
		return 1;
	}

	for (i = 1;; i++) {
		j = 0;
		if (hpos - i >= ibase)
			pos[j++] = hpos - i;
		if (hpos + i + hlen < rf->nrec)
			pos[j++] = hpos + i;
		if (!j)
			break;
		for (j--; j >= 0; j--)
			if (xdl_hunk_match(rf, pos[j], pch, mode)) {
				*hkpos = pos[j];
				return 1;
			}
	}

	return 0;
}


static int xdl_flush_section(recfile_t *rf, long start, long top, xdemitcb_t *ecb) {
	long i;
	mmbuffer_t mb;

	for (i = start; i <= top; i++) {
		if (!(mb.ptr = (char *) xdl_recfile_get(rf, i, &mb.size))) {

			return -1;
		}
		if (ecb->outf(ecb->priv, &mb, 1) < 0) {

			return -1;
		}
	}

	return 0;
}


static int xdl_apply_hunk(recfile_t *rf, long hkpos, patch_t *pch, int mode,
			  long *ibase, xdemitcb_t *ecb, patchstats_t *ps) {
	long i, size;
	char const *line;
	mmbuffer_t mb;

	if (xdl_flush_section(rf, *ibase, hkpos - 1, ecb) < 0) {

		return -1;
	}
	*ibase = hkpos;

	for (i = pch->hkrec + 1;
	     (line = xdl_recfile_get(&pch->rf, i, &size)) != NULL; i++) {
		if (*line == '@' || *line == '\n')
			break;

		if (*line == ' ' || *line != mode) {
			mb.ptr = (char *) line + 1;
			mb.size = size - 1;
			if (ecb->outf(ecb->priv, &mb, 1) < 0) {

				return -1;
			}
		}

		if (*line == ' ' || *line == mode)
			(*ibase)++;
		if (*line == mode)
			ps->dels++;
		else if (*line != ' ')
			ps->adds++;
	}

	return 0;
}


static int xdl_reject_hunk(recfile_t *rf, patch_t *pch, int mode,
			   xdemitcb_t *rjecb, patchstats_t *ps) {
	long i, size, s1, s2, c1, c2;
	char const *line, *pre;
	mmbuffer_t mb;

	if (mode == '-') {
		s1 = pch->hi.s1;
		s2 = pch->hi.s2;
		c1 = pch->hi.c1;
		c2 = pch->hi.c2;
	} else {
		s1 = pch->hi.s2;
		s2 = pch->hi.s1;
		c1 = pch->hi.c2;
		c2 = pch->hi.c1;
	}
	s1 += ps->adds - ps->dels;

	if (xdl_emit_hunk_hdr(s1 + 1, c1, s2 + 1, c2, rjecb) < 0) {

		return -1;
	}

	for (i = pch->hkrec + 1;
	     (line = xdl_recfile_get(&pch->rf, i, &size)) != NULL; i++) {
		if (*line == '@' || *line == '\n')
			break;

		if (mode == '-' || *line == ' ') {
			mb.ptr = (char *) line;
			mb.size = size;
			if (rjecb->outf(rjecb->priv, &mb, 1) < 0) {

				return -1;
			}
		} else {
			pre = *line == '+' ? "-": "+";

			if (xdl_emit_diffrec(line + 1, size - 1, pre, strlen(pre),
					     rjecb) < 0) {

				return -1;
			}
		}
	}

	return 0;
}


int xdl_patch(mmfile_t *mf, mmfile_t *mfp, int mode, xdemitcb_t *ecb,
	      xdemitcb_t *rjecb) {
	int hkres;
	long hkpos, ibase;
	recfile_t rff;
	patch_t pch;
	patchstats_t ps;

	if (xdl_init_recfile(mf, &rff) < 0) {

		return -1;
	}
	if (xdl_init_patch(mfp, &pch) < 0) {

		xdl_free_recfile(&rff);
		return -1;
	}

	ps.adds = ps.dels = 0;
	ibase = 0;
	if ((hkres = xdl_first_hunk(&pch)) > 0) {
		do {
			if (xdl_find_hunk(&rff, ibase, &pch, mode, &hkpos)) {
				if (xdl_apply_hunk(&rff, hkpos, &pch, mode,
						   &ibase, ecb, &ps) < 0) {

					xdl_free_patch(&pch);
					xdl_free_recfile(&rff);
					return -1;
				}
			} else {
				if (xdl_reject_hunk(&rff, &pch, mode, rjecb, &ps) < 0) {

					xdl_free_patch(&pch);
					xdl_free_recfile(&rff);
					return -1;
				}
			}
		} while ((hkres = xdl_next_hunk(&pch)) > 0);
	}
	if (hkres < 0) {

		xdl_free_patch(&pch);
		xdl_free_recfile(&rff);
		return -1;
	}

	if (xdl_flush_section(&rff, ibase, rff.nrec - 1, ecb) < 0) {

		xdl_free_patch(&pch);
		xdl_free_recfile(&rff);
		return -1;
	}

	xdl_free_patch(&pch);
	xdl_free_recfile(&rff);

	return 0;
}


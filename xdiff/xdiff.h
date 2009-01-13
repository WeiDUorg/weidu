/*
 *  LibXDiff by Davide Libenzi ( File Differential Library )
 *  Copyright (C) 2003  Davide Libenzi
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

#if !defined(XDIFF_H)
#define XDIFF_H

#ifdef __cplusplus
extern "C" {
#endif /* #ifdef __cplusplus */


#define XDF_NEED_MINIMAL (1 << 1)

#define XDL_PATCH_NORMAL '-'
#define XDL_PATCH_REVERSE '+'

#define XDL_MMF_ATOMIC (1 << 0)

#define XDL_BDOP_INS 1
#define XDL_BDOP_CPY 2


typedef struct s_mmblock {
	struct s_mmblock *next;
	long size, bsize;
} mmblock_t;

typedef struct s_mmfile {
	unsigned long flags;
	mmblock_t *head, *tail;
	long bsize, fsize, rpos;
	mmblock_t *rcur, *wcur;
} mmfile_t;

typedef struct s_mmbuffer {
	char *ptr;
	long size;
} mmbuffer_t;

typedef struct s_xpparam {
	unsigned long flags;
} xpparam_t;

typedef struct s_xdemitcb {
	void *priv;
	int (*outf)(void *, mmbuffer_t *, int);
} xdemitcb_t;

typedef struct s_xdemitconf {
	long ctxlen;
} xdemitconf_t;

typedef struct s_bdiffparam {
	long bsize;
} bdiffparam_t;



int xdl_init_mmfile(mmfile_t *mmf, long bsize, unsigned long flags);
void xdl_free_mmfile(mmfile_t *mmf);
int xdl_mmfile_iscompact(mmfile_t *mmf);
int xdl_seek_mmfile(mmfile_t *mmf, long off);
long xdl_read_mmfile(mmfile_t *mmf, void *data, long size);
long xdl_write_mmfile(mmfile_t *mmf, void const *data, long size);
long xdl_writem_mmfile(mmfile_t *mmf, mmbuffer_t *mb, int nbuf);
void *xdl_mmfile_writeallocate(mmfile_t *mmf, long size);
void *xdl_mmfile_first(mmfile_t *mmf, long *size);
void *xdl_mmfile_next(mmfile_t *mmf, long *size);
long xdl_mmfile_size(mmfile_t *mmf);
int xdl_mmfile_cmp(mmfile_t *mmf1, mmfile_t *mmf2);
int xdl_mmfile_compact(mmfile_t *mmfo, mmfile_t *mmfc, long bsize, unsigned long flags);

int xdl_diff(mmfile_t *mf1, mmfile_t *mf2, xpparam_t const *xpp,
	     xdemitconf_t const *xecfg, xdemitcb_t *ecb);
int xdl_patch(mmfile_t *mf, mmfile_t *mfp, int mode, xdemitcb_t *ecb,
	      xdemitcb_t *rjecb);

int xdl_bdiff(mmfile_t *mmf1, mmfile_t *mmf2, bdiffparam_t const *bdp, xdemitcb_t *ecb);
long xdl_bdiff_tgsize(mmfile_t *mmfp);
int xdl_bpatch(mmfile_t *mmf, mmfile_t *mmfp, xdemitcb_t *ecb);



#ifdef __cplusplus
}
#endif /* #ifdef __cplusplus */

#endif /* #if !defined(XDIFF_H) */


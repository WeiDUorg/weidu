#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
 #include <fcntl.h> */

#include <xdiff.h>

#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>


#define XDLT_STD_BLKSIZE (1024 * 8)

static int xdlt_outf(void *priv, mmbuffer_t *mb, int nbuf) 
{
    mmfile_t *mf = (mmfile_t*) priv;
    int i;
    
    for (i = 0; i < nbuf; i++)
        if (xdl_write_mmfile(mf, mb[i].ptr, mb[i].size) != mb[i].size)
            return -1;
    return 0;
}
static int xdlt_store_mmfile(char const *data, long size, mmfile_t *mf) 
{
    if (size < 0) {
        return -1;
    }
    if (xdl_init_mmfile(mf, XDLT_STD_BLKSIZE, XDL_MMF_ATOMIC) < 0) {
        return -1;
    }
    if (xdl_write_mmfile(mf, data, size) != size) {
        return -1;
    }
    return 0;
}

static long xdlt_mmfile_size(mmfile_t *mf) {
    char const *blk;
    long sz;
    long size = 0;
    for (blk = xdl_mmfile_first(mf, &sz); blk != NULL; blk = xdl_mmfile_next(mf, &sz)) {
        size += sz;
    }
    return (size);
}
static int xdlt_read_mmfile(char *data, mmfile_t *mf) {
    char const *blk;
    long sz;
    long size = 0;
    for (blk = xdl_mmfile_first(mf, &sz); blk != NULL; blk = xdl_mmfile_next(mf, &sz)) {
        memcpy(String_val(data) + size, blk, sz);
        size += sz;
    }
    return size;
}
static int xdlt_write_mmfile(FILE *fp, mmfile_t *mf) {
    char const *blk;
    long sz;
    long size = 0;
    long nblks = 0;
    for (blk = xdl_mmfile_first(mf, &sz); blk != NULL; blk = xdl_mmfile_next(mf, &sz)) {
        fwrite(blk, sz, 1, fp);
        size += sz;
        nblks ++;
    }
    fprintf(fp, "==> %ld blks, %ld size\n", nblks, size);
    return size;
}

static char ELINE[1024];

value xdiff_diff( value old_data, value new_data, value ctxlen ) 
{
    CAMLparam3 (old_data, new_data, ctxlen);
    CAMLlocal1(dif_data);
    
    mmfile_t mf1, mf2, mf3;
    xdemitcb_t ecb;
    xpparam_t xpp;
    xdemitconf_t xecfg;
    long dif_size;
    

    if (xdlt_store_mmfile(String_val(old_data), string_length(old_data), &mf1) < 0) {
        sprintf(ELINE, "%s:%d failed", __FILE__, __LINE__);
        failwith(ELINE);
    }
    if (xdlt_store_mmfile(String_val(new_data), string_length(new_data), &mf2) < 0) {
        xdl_free_mmfile(&mf1);
        sprintf(ELINE, "%s:%d failed", __FILE__, __LINE__);
        failwith(ELINE);
    }
    if (xdl_init_mmfile(&mf3, XDLT_STD_BLKSIZE, XDL_MMF_ATOMIC) < 0) {
        xdl_free_mmfile(&mf1);
        xdl_free_mmfile(&mf2);
        sprintf(ELINE, "%s:%d failed", __FILE__, __LINE__);
        failwith(ELINE);
    }
    ecb.priv = &mf3;
    ecb.outf = xdlt_outf;
    xpp.flags = 0;
    xecfg.ctxlen = Int_val(ctxlen);

    if (xdl_diff(&mf1, &mf2, &xpp, &xecfg, &ecb) < 0) {
        xdl_free_mmfile(&mf1);
        xdl_free_mmfile(&mf2);
        xdl_free_mmfile(&mf3);
        sprintf(ELINE, "%s:%d failed", __FILE__, __LINE__);
        failwith(ELINE);
    }
    dif_size = xdlt_mmfile_size(&mf3);
    dif_data = alloc_string(dif_size);
    if (xdlt_read_mmfile(String_val(dif_data), &mf3) < 0) {
        xdl_free_mmfile(&mf1);
        xdl_free_mmfile(&mf2);
        xdl_free_mmfile(&mf3);
        sprintf(ELINE, "%s:%d failed", __FILE__, __LINE__);
        failwith(ELINE);
    }
    xdl_free_mmfile(&mf1);
    xdl_free_mmfile(&mf2);
    xdl_free_mmfile(&mf3);

    CAMLreturn(dif_data);
}

value xdiff_patch( value old_data, value patch) 
{
    CAMLparam2 (old_data, patch);
    CAMLlocal1(res);
    mmfile_t mf1, mf2, mf3, mf4;
    xdemitcb_t ecb, rjecb;
    long new_size, rej_size;

    res = alloc_tuple(2);

    if (xdlt_store_mmfile(String_val(old_data), string_length(old_data), &mf1) < 0) {
        sprintf(ELINE, "%s:%d xdlt_store_mmfile() failed (%ld)", __FILE__, __LINE__, 
                string_length(old_data));
        failwith(ELINE);
    }
    if (xdlt_store_mmfile(String_val(patch), string_length(patch), &mf2) < 0) {
        xdl_free_mmfile(&mf1);
        sprintf(ELINE, "%s:%d xdlt_store_mmfile() failed (%ld)", __FILE__, __LINE__,
                string_length(patch));
        failwith(ELINE);
    }
    if (xdl_init_mmfile(&mf3, XDLT_STD_BLKSIZE, XDL_MMF_ATOMIC) < 0) {
        xdl_free_mmfile(&mf1);
        xdl_free_mmfile(&mf2);
        sprintf(ELINE, "%s:%d failed", __FILE__, __LINE__);
        failwith(ELINE);
    }
    if (xdl_init_mmfile(&mf4, XDLT_STD_BLKSIZE, XDL_MMF_ATOMIC) < 0) {
        xdl_free_mmfile(&mf1);
        xdl_free_mmfile(&mf2);
        xdl_free_mmfile(&mf3);
        sprintf(ELINE, "%s:%d failed", __FILE__, __LINE__);
        failwith(ELINE);
    }

    ecb.priv = &mf3;
    ecb.outf = xdlt_outf;
    rjecb.priv = &mf4;
    rjecb.outf = xdlt_outf;

    /* fprintf(stdout, "OLD_DATA:\n");
       xdlt_write_mmfile(stdout, &mf1);
       printf("PATCH:\n");
       xdlt_write_mmfile(stdout, &mf2);
       fflush(stdout); */

    if (xdl_patch(&mf1, &mf2, XDL_PATCH_NORMAL, &ecb, &rjecb) < 0) {
        xdl_free_mmfile(&mf1);
        xdl_free_mmfile(&mf2);
        xdl_free_mmfile(&mf3);
        xdl_free_mmfile(&mf4);
        sprintf(ELINE, "%s:%d failed", __FILE__, __LINE__);
        failwith(ELINE);
    }
    new_size = xdlt_mmfile_size(&mf3);
    rej_size = xdlt_mmfile_size(&mf4);
    Field(res, 0) = alloc_string(new_size);
    Field(res, 1) = alloc_string(rej_size);
    if (xdlt_read_mmfile(String_val(Field(res, 0)), &mf3) < 0) {
        xdl_free_mmfile(&mf1);
        xdl_free_mmfile(&mf2);
        xdl_free_mmfile(&mf3);
        xdl_free_mmfile(&mf4);
        sprintf(ELINE, "%s:%d failed", __FILE__, __LINE__);
        failwith(ELINE);
    }
    if (xdlt_read_mmfile(String_val(Field(res, 1)), &mf4) < 0) {
        xdl_free_mmfile(&mf1);
        xdl_free_mmfile(&mf2);
        xdl_free_mmfile(&mf3);
        xdl_free_mmfile(&mf4);
        sprintf(ELINE, "%s:%d failed", __FILE__, __LINE__);
        failwith(ELINE);
    }

    xdl_free_mmfile(&mf1);
    xdl_free_mmfile(&mf2);
    xdl_free_mmfile(&mf3);
    xdl_free_mmfile(&mf4);

    CAMLreturn(res);
}

value xdiff_revpatch( value old_data, value patch) 
{
    CAMLparam2 (old_data, patch);
    CAMLlocal1(res);
    mmfile_t mf1, mf2, mf3, mf4;
    xdemitcb_t ecb, rjecb;
    long new_size, rej_size;


    res = alloc_tuple(2);

    if (xdlt_store_mmfile(String_val(old_data), string_length(old_data), &mf1) < 0) {
        sprintf(ELINE, "%s:%d failed", __FILE__, __LINE__);
        failwith(ELINE);
    }
    if (xdlt_store_mmfile(String_val(patch), string_length(patch), &mf2) < 0) {
        xdl_free_mmfile(&mf1);
        sprintf(ELINE, "%s:%d failed", __FILE__, __LINE__);
        failwith(ELINE);
    }
    if (xdl_init_mmfile(&mf3, XDLT_STD_BLKSIZE, XDL_MMF_ATOMIC) < 0) {
        xdl_free_mmfile(&mf1);
        xdl_free_mmfile(&mf2);
        sprintf(ELINE, "%s:%d failed", __FILE__, __LINE__);
        failwith(ELINE);
    }
    if (xdl_init_mmfile(&mf4, XDLT_STD_BLKSIZE, XDL_MMF_ATOMIC) < 0) {
        xdl_free_mmfile(&mf1);
        xdl_free_mmfile(&mf2);
        xdl_free_mmfile(&mf3);
        sprintf(ELINE, "%s:%d failed", __FILE__, __LINE__);
        failwith(ELINE);
    }
    ecb.priv = &mf3;
    ecb.outf = xdlt_outf;
    rjecb.priv = &mf4;
    rjecb.outf = xdlt_outf;

    if (xdl_patch(&mf1, &mf2, XDL_PATCH_REVERSE, &ecb, &rjecb) < 0) {
        xdl_free_mmfile(&mf1);
        xdl_free_mmfile(&mf2);
        xdl_free_mmfile(&mf3);
        xdl_free_mmfile(&mf4);
        sprintf(ELINE, "%s:%d failed", __FILE__, __LINE__);
        failwith(ELINE);
    }
    new_size = xdlt_mmfile_size(&mf3);
    rej_size = xdlt_mmfile_size(&mf4);
    Field(res, 0) = alloc_string(new_size);
    Field(res, 1) = alloc_string(rej_size);
    if (xdlt_read_mmfile(String_val(Field(res, 0)), &mf3) < 0) {
        xdl_free_mmfile(&mf1);
        xdl_free_mmfile(&mf2);
        xdl_free_mmfile(&mf3);
        xdl_free_mmfile(&mf4);
        sprintf(ELINE, "%s:%d failed", __FILE__, __LINE__);
        failwith(ELINE);
    }
    if (xdlt_read_mmfile(String_val(Field(res, 1)), &mf4) < 0) {
        xdl_free_mmfile(&mf1);
        xdl_free_mmfile(&mf2);
        xdl_free_mmfile(&mf3);
        xdl_free_mmfile(&mf4);
        sprintf(ELINE, "%s:%d failed", __FILE__, __LINE__);
        failwith(ELINE);
    }

    xdl_free_mmfile(&mf1);
    xdl_free_mmfile(&mf2);
    xdl_free_mmfile(&mf3);
    xdl_free_mmfile(&mf4);

    CAMLreturn(res);
}


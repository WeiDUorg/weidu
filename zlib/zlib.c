#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>

#include <zlib.h>

#ifdef __linux__
/* find uint32_t also on Linux, *sigh* */
#include <elf.h>
#endif

/* ERROR REPORTING */
/* raise the Caml exception */
static void raise_mlgz_exn(const char *msg) 
{
  static value * exn = NULL;
  if(exn == NULL)
    exn = caml_named_value ("mlgz_exn");
  caml_raise_with_string(*exn, (char *)msg) ;
}

value mlgz_compress(value v_lvl, value v_src, value v_pos, value v_len)
{
	value v_ret;
	int pos, len, out_buf_len, r;
	int level = Z_BEST_COMPRESSION ;
	uLong out_len;
	const unsigned char *in_buf;
	unsigned char *out_buf;

	level = Int_val(v_lvl) ;
	pos = Int_val(v_pos);
	len = Int_val(v_len);
	in_buf = (unsigned char *) String_val(v_src) + pos;
	if(level < 0 || level > 9 
			|| pos < 0 || len < 0
			|| pos + len > string_length(v_src))
		invalid_argument("Gz.compress");
	out_buf_len = len + 12;
	out_buf_len += out_buf_len / 1000;
	out_buf = malloc(out_buf_len);
	if(out_buf == NULL)
		raise_out_of_memory();
	while(1) {
		out_len = out_buf_len;
		r = compress2(out_buf, &out_len, in_buf, len, level);
		if(r == Z_OK) {
			break;
		} else if(r == Z_BUF_ERROR) {
			unsigned char *new_buf;

			out_buf_len *= 2;
			new_buf = realloc(out_buf, out_buf_len);
			if(new_buf == NULL) {
				free(out_buf);
				raise_out_of_memory();
			}
			out_buf = new_buf;
		} else {
			free(out_buf);
			raise_out_of_memory();
		}
	}
	v_ret = alloc_string(out_len);
	memcpy(String_val(v_ret), out_buf, out_len);
	free(out_buf);
	return v_ret ;
}

CAMLprim value mlgz_uncompress(value v_src, value v_pos, value v_len, value unc_len)
{
  CAMLparam4(v_src, v_pos, v_len, unc_len);
  CAMLlocal1(v_ret);
  int level, pos, len, out_buf_len, r;
  uLong out_len;
  const char *in_buf;
  char *out_buf;

  pos = Int_val(v_pos);
  len = Int_val(v_len);
  if(pos < 0 || len < 0 || pos + len > string_length(v_src))
    invalid_argument("Gz.uncompress");

  out_buf_len = Int_val(unc_len);
  v_ret = caml_alloc_string(Int_val(unc_len));
  out_buf = String_val(v_ret);
  in_buf = String_val(v_src) + pos;

  if(out_buf == NULL)
    raise_out_of_memory();
  while(1) {
    out_len = out_buf_len;
    r = uncompress(out_buf, &out_len, in_buf, len);
    if(r == Z_OK) {
      break;
    } else if(r == Z_BUF_ERROR) {
      char *new_buf;

      raise_mlgz_exn("uncompress");
      out_buf_len *= 2;
      new_buf = realloc(out_buf, out_buf_len);
      if(new_buf == NULL) {
	free(out_buf);
	raise_out_of_memory();
      }
      out_buf = new_buf;
    } else if(r == Z_MEM_ERROR) { 
      free(out_buf);
      raise_out_of_memory();
    } else {
      // FIXME: check for additional values like Z_DATA_ERROR
      fflush(stdout); 
      out_len = Int_val(unc_len);
      break; 
    }
  }
  if (Int_val(unc_len) != out_len)
      raise_mlgz_exn("wrong uncompressed size");

  /*
   v_ret = alloc_string(out_len);
   memcpy(String_val(v_ret), out_buf, out_len);
   free(out_buf);
   */
  CAMLreturn (v_ret) ;
}

/* zerr() and def() are copied directly from zlib example code. */

#if defined(MSDOS) || defined(OS2) || defined(WIN32) || defined(__CYGWIN__)
#  include <fcntl.h>
#  include <io.h>
#  define SET_BINARY_MODE(file) setmode(fileno(file), O_BINARY)
#else
#  define SET_BINARY_MODE(file)
#endif

#define CHUNK 16384


/* Raise an exception for a zlib or i/o error */
void mlgz_zerr(int ret)
{
    switch (ret) {
    case Z_ERRNO:
        raise_sys_error(copy_string(strerror(errno))) ;
        break;
    case Z_STREAM_ERROR:
        raise_mlgz_exn("invalid compression level");
        break;
    case Z_DATA_ERROR:
        raise_mlgz_exn("invalid or incomplete deflate data");
        break;
    case Z_MEM_ERROR:
        raise_out_of_memory() ;
        break;
    case Z_VERSION_ERROR:
        raise_mlgz_exn("zlib version mismatch!");
    }
}

/* Yes, this is bad practice.  I compensated by using "%.256s" instead of just "%s": */
static char errstr[1024];

/* This is a bit better for error checking: */
int fread_check(void* b, size_t sz, size_t cnt, FILE* fp, const char* fn)
{
    if (fread(b, sz, cnt, fp) != cnt) {
        sprintf(errstr, "Failed to read %d bytes from file %.256s", (int)(sz*cnt), fn);
        raise_mlgz_exn(errstr);
        return 1;
    }
    return 0;
}

int fread_uint(uint32_t* i, FILE* fp, const char* fn)
{
    if (fread_check(i, 4, 1, fp, fn))
        return 1;
#if defined(__ppc__) || defined(__ppc64__)
    *i = OSSwapInt32(*i);
#endif
    return 0;
}


/* Decompress from file source to file dest until stream ends or EOF.
   inf() returns Z_OK on success, Z_MEM_ERROR if memory could not be
   allocated for processing, Z_DATA_ERROR if the deflate data is
   invalid or incomplete, Z_VERSION_ERROR if the version of zlib.h and
   the version of the library linked do not match, or Z_ERRNO if there
   is an error reading or writing the files. */
int inf(FILE *source, FILE *dest)
{
    int ret;
    unsigned have;
    z_stream strm;
    unsigned char in[CHUNK];
    unsigned char out[CHUNK];
   
    /* allocate inflate state */
    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    strm.avail_in = 0;
    strm.next_in = Z_NULL;
    ret = inflateInit(&strm);
    if (ret != Z_OK)
        return ret;
    /* decompress until deflate stream ends or end of file */
    do {
        strm.avail_in = fread(in, 1, CHUNK, source);
        if (ferror(source)) {
            (void)inflateEnd(&strm);
            return Z_ERRNO;
        }
        if (strm.avail_in == 0)
            break;
        strm.next_in = in;
        /* run inflate() on input until output buffer not full */
        do {
            strm.avail_out = CHUNK;
            strm.next_out = out;
            ret = inflate(&strm, Z_NO_FLUSH);
            /* assert(ret != Z_STREAM_ERROR); */  /* state not clobbered */ /* no asserts for Ocaml */
            switch (ret) {
            case Z_NEED_DICT:
                ret = Z_DATA_ERROR;     /* and fall through */
            case Z_DATA_ERROR:
            case Z_MEM_ERROR:
                (void)inflateEnd(&strm);
                return ret;
            }
            have = CHUNK - strm.avail_out;
            if (fwrite(out, 1, have, dest) != have || ferror(dest)) {
                (void)inflateEnd(&strm);
                return Z_ERRNO;
            }
        } while (strm.avail_out == 0);
        /* done when inflate() says it's done */
    } while (ret != Z_STREAM_END);
    /* clean up and return */
    (void)inflateEnd(&strm);
    return ret == Z_STREAM_END ? Z_OK : Z_DATA_ERROR;
}

/*
   Unompresses a CBF files to a specified BIF file.  Return 0 on failure or the
   number of uncompressed bytes on success.
 */
CAMLprim value mlgz_cbf2bif(value _cbf_file, value _bif_file)
{
  CAMLparam2(_cbf_file, _bif_file);
  const char* cbf_file = String_val(_cbf_file);
  const char* bif_file = String_val(_bif_file);
  FILE *cbf_fp;
  FILE *bif_fp;
  char sigver[9];
  uint32_t bif_file_len, cmplen, uncmplen;
  int zret;
   
  if (!(cbf_fp = fopen(cbf_file, "rb"))) {
    sprintf(errstr, "failure opening file: %.256s", cbf_file);
    raise_mlgz_exn(errstr);
    CAMLreturn(Val_int(0));
  }

  if (!(bif_fp = fopen(bif_file, "wb"))) {
    fclose(cbf_fp);
    sprintf(errstr, "failure opening file: %.256s", bif_file);
    raise_mlgz_exn(errstr);
    CAMLreturn(Val_int(0));
  }

  sigver[8] = 0;
  if (fread_check(sigver, 1, 8, cbf_fp, cbf_file)) {
    fclose(cbf_fp);
    fclose(bif_fp);
    CAMLreturn(Val_int(0));
  }
   
  if (strcmp(sigver, "BIF V1.0")) {
    fclose(cbf_fp);
    fclose(bif_fp);
    sprintf(errstr, "incorrect CBF header for file %.256s", cbf_file);
    raise_mlgz_exn(errstr);
    CAMLreturn(Val_int(0));
  }

  if (fread_uint(&bif_file_len, cbf_fp, cbf_file)) {
    fclose(cbf_fp);
    fclose(bif_fp);
    CAMLreturn(Val_int(0));
  }
   
  if (bif_file_len <=0 || bif_file_len > 128) {
    fclose(cbf_fp);
    fclose(bif_fp);
    sprintf(errstr, "corrupt CBF file %.256s", cbf_file);
    raise_mlgz_exn(errstr);
    CAMLreturn(Val_int(0));
  }

  /* Seek ahead past embedded file name, doesn't really matter what it is */
  if (fseek(cbf_fp, bif_file_len, SEEK_CUR)) {
    fclose(cbf_fp);
    fclose(bif_fp);
    sprintf(errstr, "failure seeking %d bytes into file %.256s", bif_file_len, cbf_file);
    raise_mlgz_exn(errstr);
    CAMLreturn(Val_int(0));
  }

  if (fread_uint(&uncmplen, cbf_fp, cbf_file)) {
    fclose(cbf_fp);
    fclose(bif_fp);
    CAMLreturn(Val_int(0));
  }
   
  if (fread_uint(&cmplen, cbf_fp, cbf_file)) {
    fclose(cbf_fp);
    fclose(bif_fp);
    CAMLreturn(Val_int(0));
  }
  /* printf("CBF %s (%ld bytes) -> BIF %s [%ld bytes]", cbf_file, cmplen, bif_file, uncmplen); */

  if ((zret=inf(cbf_fp, bif_fp)) != Z_OK) {
    fclose(cbf_fp);
    fclose(bif_fp);
    mlgz_zerr(zret);
    CAMLreturn(Val_int(0));
  }

  if (fclose(cbf_fp)) {
    fclose(bif_fp);
    sprintf(errstr, "failure closing file %.256s", cbf_file);
    raise_mlgz_exn(errstr);
    CAMLreturn(Val_int(0));
  }
  if (fclose(bif_fp)) {
    sprintf(errstr, "failure closing file %.256s", bif_file);
    raise_mlgz_exn(errstr);
    CAMLreturn(Val_int(0));
  }
  CAMLreturn(Val_int(uncmplen));
}

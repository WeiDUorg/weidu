// Code taken from here: https://github.com/OneSadCookie/fcaseopen/blob/master/fcaseopen.c

/* Copyright (c) 2009 Keith Bauer
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include "caml/mlvalues.h"
#include "caml/alloc.h"

#if !defined(_WIN32) || !__APPLE__
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <dirent.h>
#include <errno.h>
#include <unistd.h>

/* --------------------------------------------------------------------------
 * Directory-listing cache.
 *
 * casepath() used to opendir()+readdir()-scan the parent directory for every
 * path component of every file operation.  On Linux, where WeiDU emulates a
 * case-insensitive file system, that is O(entries) syscalls per lookup and it
 * dominates the cost of a mod install (WeiDUorg/weidu#327: sys time went from
 * ~1s to ~2m43s between v249 and v251rc4).
 *
 * We cache, per resolved directory path, a hashtable mapping a lowercased base
 * name to the real on-disk base name.  A directory is scanned once (on first
 * touch) and reused, turning each per-component lookup from O(entries) into
 * O(1).  The cache is kept coherent by the OCaml wrappers in case_ins_linux.ml,
 * which call fcase_cache_add / _remove / _flush whenever they create, delete or
 * rename a file, and fcase_cache_clear after running an external command.  The
 * OCaml side knows the semantic operation, which is what makes correct
 * invalidation feasible (doing it blindly inside C would not be).
 * ------------------------------------------------------------------------ */

typedef struct fc_entry {
  char *lower;                 /* key: lowercased base name */
  char *real;                  /* value: real on-disk base name */
  struct fc_entry *next;
} fc_entry;

typedef struct fc_dir {
  char *path;                  /* key: resolved directory path */
  fc_entry **buckets;
  size_t nbuckets;
  size_t nentries;
  struct fc_dir *next;
} fc_dir;

#define FC_DIR_NBUCKETS 1024
static fc_dir *fc_dirs[FC_DIR_NBUCKETS];

static unsigned long fc_hash(const char *s)
{
  unsigned long h = 5381;
  int c;
  while ((c = (unsigned char) *s++))
    h = ((h << 5) + h) + c;
  return h;
}

static char *fc_strdup(const char *s)
{
  size_t n = strlen(s) + 1;
  char *r = malloc(n);
  if (r) memcpy(r, s, n);
  return r;
}

static char *fc_strdup_lower(const char *s)
{
  size_t n = strlen(s);
  char *r = malloc(n + 1);
  size_t i;
  if (!r) return NULL;
  for (i = 0; i < n; i++)
    r[i] = (char) tolower((unsigned char) s[i]);
  r[n] = 0;
  return r;
}

static void fc_dir_rehash(fc_dir *d)
{
  size_t newn = d->nbuckets ? d->nbuckets * 2 : 16;
  fc_entry **nb = calloc(newn, sizeof(fc_entry *));
  size_t i;
  if (!nb) return;                     /* keep old table on OOM */
  for (i = 0; i < d->nbuckets; i++) {
    fc_entry *e = d->buckets[i];
    while (e) {
      fc_entry *next = e->next;
      size_t idx = fc_hash(e->lower) % newn;
      e->next = nb[idx];
      nb[idx] = e;
      e = next;
    }
  }
  free(d->buckets);
  d->buckets = nb;
  d->nbuckets = newn;
}

/* Insert or replace one entry (keyed on lowercase(real)) in a dir map. */
static void fc_dir_put(fc_dir *d, const char *real)
{
  char *lower = fc_strdup_lower(real);
  size_t idx;
  fc_entry *e;
  if (!lower) return;
  if (d->nbuckets == 0 || d->nentries + 1 > d->nbuckets * 2)
    fc_dir_rehash(d);
  if (d->nbuckets == 0) { free(lower); return; }   /* rehash failed (OOM) */
  idx = fc_hash(lower) % d->nbuckets;
  for (e = d->buckets[idx]; e; e = e->next) {
    if (strcmp(e->lower, lower) == 0) {
      char *nr = fc_strdup(real);      /* replace real name, keep key */
      if (nr) { free(e->real); e->real = nr; }
      free(lower);
      return;
    }
  }
  e = malloc(sizeof(fc_entry));
  if (!e) { free(lower); return; }
  e->lower = lower;
  e->real = fc_strdup(real);
  e->next = d->buckets[idx];
  d->buckets[idx] = e;
  d->nentries++;
}

/* Find the real name for a case-insensitive base name; NULL if absent. */
static const char *fc_dir_get(fc_dir *d, const char *name)
{
  char *lower = fc_strdup_lower(name);
  const char *res = NULL;
  size_t idx;
  fc_entry *e;
  if (!lower) return NULL;
  if (d->nbuckets == 0) { free(lower); return NULL; }
  idx = fc_hash(lower) % d->nbuckets;
  for (e = d->buckets[idx]; e; e = e->next) {
    if (strcmp(e->lower, lower) == 0) { res = e->real; break; }
  }
  free(lower);
  return res;
}

/* Remove one entry by case-insensitive base name. */
static void fc_dir_del(fc_dir *d, const char *name)
{
  char *lower = fc_strdup_lower(name);
  size_t idx;
  fc_entry *e, *prev = NULL;
  if (!lower) return;
  if (d->nbuckets == 0) { free(lower); return; }
  idx = fc_hash(lower) % d->nbuckets;
  for (e = d->buckets[idx]; e; prev = e, e = e->next) {
    if (strcmp(e->lower, lower) == 0) {
      if (prev) prev->next = e->next; else d->buckets[idx] = e->next;
      free(e->lower); free(e->real); free(e);
      d->nentries--;
      break;
    }
  }
  free(lower);
}

static void fc_dir_free(fc_dir *d)
{
  size_t i;
  for (i = 0; i < d->nbuckets; i++) {
    fc_entry *e = d->buckets[i];
    while (e) {
      fc_entry *n = e->next;
      free(e->lower); free(e->real); free(e);
      e = n;
    }
  }
  free(d->buckets);
  free(d->path);
  free(d);
}

static fc_dir *fc_dir_find(const char *path)
{
  size_t idx = fc_hash(path) % FC_DIR_NBUCKETS;
  fc_dir *d;
  for (d = fc_dirs[idx]; d; d = d->next)
    if (strcmp(d->path, path) == 0) return d;
  return NULL;
}

/* Scan a directory into the cache. Returns NULL if it cannot be opened. */
static fc_dir *fc_dir_load(const char *path)
{
  DIR *dh = opendir(path);
  struct dirent *e;
  fc_dir *d;
  size_t idx;
  if (!dh) return NULL;
  d = malloc(sizeof(fc_dir));
  if (!d) { closedir(dh); return NULL; }
  d->path = fc_strdup(path);
  d->buckets = NULL;
  d->nbuckets = 0;
  d->nentries = 0;
  while ((e = readdir(dh)))
    fc_dir_put(d, e->d_name);
  closedir(dh);
  idx = fc_hash(path) % FC_DIR_NBUCKETS;
  d->next = fc_dirs[idx];
  fc_dirs[idx] = d;
  return d;
}

/* Resolve one path component inside dirpath.
 *   returns  1 and sets *real_out -> matched, real name lives in the cache
 *            0                     -> directory exists but nothing matched
 *           -1                     -> directory could not be opened */
static int fc_resolve(const char *dirpath, const char *name, const char **real_out)
{
  fc_dir *d = fc_dir_find(dirpath);
  const char *r;
  if (!d) {
    d = fc_dir_load(dirpath);
    if (!d) return -1;
  }
  r = fc_dir_get(d, name);
  if (r) { *real_out = r; return 1; }
  return 0;
}

// r must have strlen(path) + 3 bytes
static int casepath(char const *path, char *r)
{
  size_t l = strlen(path);
  char *pbuf = alloca(l + 1);
  char *p = pbuf;
  size_t rl;
  char *c;
  int last = 0;
  strcpy(pbuf, path);

  if (p[0] == '/')
  {
    r[0] = 0;
    rl = 0;
    p = p + 1;
  }
  else
  {
    r[0] = '.';
    r[1] = 0;
    rl = 1;
  }

  c = strsep(&p, "/");
  while (c)
  {
    const char *real = NULL;
    int rc;

    /* A previous component matched but was not a directory, yet more
     * components follow: the path cannot be resolved. */
    if (last)
      return 0;

    rc = fc_resolve(rl == 0 ? "/" : r, c, &real);
    if (rc < 0)
      return 0;

    r[rl] = '/';
    rl += 1;

    if (rc == 1)
    {
      size_t rn = strlen(real);
      memcpy(r + rl, real, rn);
      rl += rn;
    }
    else
    {
      size_t cn = strlen(c);
      memcpy(r + rl, c, cn);
      rl += cn;
      last = 1;
    }
    r[rl] = 0;

    c = strsep(&p, "/");
  }

  return 1;
}

CAMLprim value fcase_cache_add(value vdir, value vname)
{
  fc_dir *d = fc_dir_find(String_val(vdir));
  if (d) fc_dir_put(d, String_val(vname));
  return Val_unit;
}

CAMLprim value fcase_cache_remove(value vdir, value vname)
{
  fc_dir *d = fc_dir_find(String_val(vdir));
  if (d) fc_dir_del(d, String_val(vname));
  return Val_unit;
}

CAMLprim value fcase_cache_flush(value vdir)
{
  const char *path = String_val(vdir);
  size_t idx = fc_hash(path) % FC_DIR_NBUCKETS;
  fc_dir *d = fc_dirs[idx], *prev = NULL;
  while (d)
  {
    if (strcmp(d->path, path) == 0)
    {
      if (prev) prev->next = d->next; else fc_dirs[idx] = d->next;
      fc_dir_free(d);
      break;
    }
    prev = d;
    d = d->next;
  }
  return Val_unit;
}

CAMLprim value fcase_cache_clear(value unit)
{
  size_t i;
  (void) unit;
  for (i = 0; i < FC_DIR_NBUCKETS; i++)
  {
    fc_dir *d = fc_dirs[i];
    while (d) { fc_dir *n = d->next; fc_dir_free(d); d = n; }
    fc_dirs[i] = NULL;
  }
  return Val_unit;
}
#else
static int casepath(char const *path, char *r)
{
  r = path;
  return 0;
}

CAMLprim value fcase_cache_add(value vdir, value vname)
{ (void) vdir; (void) vname; return Val_unit; }
CAMLprim value fcase_cache_remove(value vdir, value vname)
{ (void) vdir; (void) vname; return Val_unit; }
CAMLprim value fcase_cache_flush(value vdir)
{ (void) vdir; return Val_unit; }
CAMLprim value fcase_cache_clear(value unit)
{ (void) unit; return Val_unit; }
#endif

CAMLprim value fcase(value path)
{
  const char * cpath = String_val(path);
  char *r = alloca(strlen(cpath) + 3);
  if (casepath(cpath, r))
  {
    return copy_string(r);
  }
  return path;
}

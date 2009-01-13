/* code written by Devon Smith */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/param.h>
#include <glob.h>

#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>

value myglob(value v_pat, value v_fun) {

  CAMLparam2(v_pat, v_fun);
  glob_t g;
  int x, returnValue, patlen;
  CAMLlocal1(v_string);
  const char * pat = String_val(v_pat);
  char convertedPat[MAXPATHLEN];

  returnValue = EXIT_FAILURE;
  patlen = strlen(pat);
  if (patlen < MAXPATHLEN) {
    for(x=0;x<patlen;x++) {
      if (pat[x] == '\\') {
        convertedPat[x] = '/';
      } else {
        convertedPat[x] = pat[x];
      } //if
    } //for
    convertedPat[patlen] = '\0';
    returnValue = glob(convertedPat, GLOB_MARK, NULL, &g);
    if (returnValue == 0) {
      for(x = 0;x<g.gl_pathc;x++) {
        const char * tempstring = g.gl_pathv[x];
        if (tempstring[strlen(tempstring)-1] != '/') {
          v_string = copy_string(tempstring);
          callback(v_fun, v_string);
        } //if
      } //for
    } //if
    globfree(&g);
  } //if
  CAMLreturn(Val_int(returnValue));
}

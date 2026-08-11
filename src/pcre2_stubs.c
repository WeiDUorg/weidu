/*
 * Minimal OCaml binding for WeiDU's bundled PCRE2 8-bit engine.
 *
 * Only compilation, matching, and capture metadata cross this boundary.
 * Replacement and global-match policy stay in OCaml, where their interaction
 * with TP2 variables is explicit and testable.
 */

#define CAML_NAME_SPACE
#define PCRE2_CODE_UNIT_WIDTH 8
#define PCRE2_STATIC

#include <stdio.h>

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "pcre2.h"

typedef struct {
  pcre2_code *code;
} weidu_pcre2_code;

#define Code_val(value) (((weidu_pcre2_code *)Data_custom_val(value))->code)

static void weidu_pcre2_finalize(value wrapped)
{
  pcre2_code *code = Code_val(wrapped);
  if (code != NULL) {
    pcre2_code_free(code);
  }
}

static struct custom_operations weidu_pcre2_code_operations = {
  "org.weidu.pcre2.code",
  weidu_pcre2_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

static void weidu_pcre2_raise_error(const char *operation, int error_code)
{
  PCRE2_UCHAR message[256];
  char full_message[320];
  int result = pcre2_get_error_message(error_code, message, sizeof(message));

  if (result < 0) {
    snprintf(full_message, sizeof(full_message),
             "PCRE2 %s failed with error %d", operation, error_code);
  } else {
    snprintf(full_message, sizeof(full_message),
             "PCRE2 %s failed: %s", operation, (char *)message);
  }
  caml_failwith(full_message);
}

CAMLprim value weidu_pcre2_compile(value pattern, value case_sensitive)
{
  CAMLparam2(pattern, case_sensitive);
  CAMLlocal1(wrapped);
  int error_code;
  PCRE2_SIZE error_offset;
  uint32_t options = Bool_val(case_sensitive) ? 0 : PCRE2_CASELESS;
  pcre2_code *code;

  wrapped = caml_alloc_custom(&weidu_pcre2_code_operations,
                              sizeof(weidu_pcre2_code), 0, 1);
  Code_val(wrapped) = NULL;

  code = pcre2_compile((PCRE2_SPTR)String_val(pattern),
                       (PCRE2_SIZE)caml_string_length(pattern),
                       options, &error_code, &error_offset, NULL);
  if (code == NULL) {
    PCRE2_UCHAR message[256];
    char full_message[384];
    int result = pcre2_get_error_message(error_code, message, sizeof(message));

    if (result < 0) {
      snprintf(full_message, sizeof(full_message),
               "PCRE2 compilation failed at offset %lu with error %d",
               (unsigned long)error_offset, error_code);
    } else {
      snprintf(full_message, sizeof(full_message),
               "PCRE2 compilation failed at offset %lu: %s",
               (unsigned long)error_offset, (char *)message);
    }
    caml_failwith(full_message);
  }

  Code_val(wrapped) = code;
  CAMLreturn(wrapped);
}

CAMLprim value weidu_pcre2_exec(value wrapped, value subject, value start,
                                value anchored, value notempty_at_start)
{
  CAMLparam5(wrapped, subject, start, anchored, notempty_at_start);
  CAMLlocal3(result, offsets, some);
  pcre2_code *code = Code_val(wrapped);
  pcre2_match_data *match_data;
  PCRE2_SIZE *ovector;
  mlsize_t subject_length = caml_string_length(subject);
  intnat start_offset = Long_val(start);
  uint32_t options = 0;
  int match_count;
  uint32_t capture_count;
  uint32_t index;

  if (start_offset < 0 || (uintnat)start_offset > (uintnat)subject_length) {
    caml_invalid_argument("PCRE2 search offset is outside the subject");
  }
  if (Bool_val(anchored)) {
    options |= PCRE2_ANCHORED;
  }
  if (Bool_val(notempty_at_start)) {
    options |= PCRE2_NOTEMPTY_ATSTART;
  }

  match_data = pcre2_match_data_create_from_pattern(code, NULL);
  if (match_data == NULL) {
    caml_raise_out_of_memory();
  }

  match_count = pcre2_match(code, (PCRE2_SPTR)String_val(subject),
                            (PCRE2_SIZE)subject_length,
                            (PCRE2_SIZE)start_offset, options,
                            match_data, NULL);
  if (match_count == PCRE2_ERROR_NOMATCH) {
    pcre2_match_data_free(match_data);
    CAMLreturn(Val_int(0));
  }
  if (match_count < 0) {
    pcre2_match_data_free(match_data);
    weidu_pcre2_raise_error("match", match_count);
  }

  if (pcre2_pattern_info(code, PCRE2_INFO_CAPTURECOUNT, &capture_count) != 0) {
    pcre2_match_data_free(match_data);
    caml_failwith("PCRE2 failed to report the capture count");
  }

  ovector = pcre2_get_ovector_pointer(match_data);
  offsets = caml_alloc((capture_count + 1) * 2, 0);
  for (index = 0; index <= capture_count; ++index) {
    PCRE2_SIZE first = index < (uint32_t)match_count
      ? ovector[index * 2] : PCRE2_UNSET;
    PCRE2_SIZE last = index < (uint32_t)match_count
      ? ovector[(index * 2) + 1] : PCRE2_UNSET;
    Store_field(offsets, index * 2,
                first == PCRE2_UNSET ? Val_int(-1) : Val_long(first));
    Store_field(offsets, (index * 2) + 1,
                last == PCRE2_UNSET ? Val_int(-1) : Val_long(last));
  }
  pcre2_match_data_free(match_data);

  some = caml_alloc(1, 0);
  Store_field(some, 0, offsets);
  result = some;
  CAMLreturn(result);
}

CAMLprim value weidu_pcre2_utf(value wrapped)
{
  CAMLparam1(wrapped);
  uint32_t options;

  if (pcre2_pattern_info(Code_val(wrapped), PCRE2_INFO_ALLOPTIONS,
                         &options) != 0) {
    caml_failwith("PCRE2 failed to report compiled-pattern options");
  }
  CAMLreturn(Val_bool((options & PCRE2_UTF) != 0));
}

CAMLprim value weidu_pcre2_capture_names(value wrapped)
{
  CAMLparam1(wrapped);
  CAMLlocal4(result, pair, name, number);
  pcre2_code *code = Code_val(wrapped);
  uint32_t name_count;
  uint32_t entry_size;
  PCRE2_SPTR name_table;
  uint32_t index;

  if (pcre2_pattern_info(code, PCRE2_INFO_NAMECOUNT, &name_count) != 0 ||
      pcre2_pattern_info(code, PCRE2_INFO_NAMEENTRYSIZE, &entry_size) != 0 ||
      pcre2_pattern_info(code, PCRE2_INFO_NAMETABLE, &name_table) != 0) {
    caml_failwith("PCRE2 failed to report named-capture metadata");
  }

  result = caml_alloc(name_count, 0);
  for (index = 0; index < name_count; ++index) {
    PCRE2_SPTR entry = name_table + (index * entry_size);
    uint32_t group_number = ((uint32_t)entry[0] << 8) | entry[1];

    name = caml_copy_string((const char *)(entry + 2));
    number = Val_long(group_number);
    pair = caml_alloc_tuple(2);
    Store_field(pair, 0, name);
    Store_field(pair, 1, number);
    Store_field(result, index, pair);
  }

  CAMLreturn(result);
}

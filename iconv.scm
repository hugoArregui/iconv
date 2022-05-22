;; This file is in the public domain and may be reproduced or copied without
;; permission from its author.  Citation of the source is appreciated.
;;
;; Alejandro Forero Cuervo <bachue@bachue.com>
;;
;; This file implements an egg for Chicken Scheme that allows conversion of
;; multi-byte sequences from one character set (encoding) to another by means
;; of the iconv functionality present in glibc.
;;
;; Documentation is available in HTML format.

(define-external (iconv_build_result (int len)) scheme-object
  (make-string len))

(module iconv
  (iconv-open iconv)

(cond-expand
    (chicken-4
     (import scheme chicken extras foreign))
    (chicken-5
     (import scheme (chicken base) (chicken foreign) (chicken gc)))
    (else (error "Unsupported chicken version.")))

(declare (foreign-declare "#include <iconv.h>\n#include <errno.h>\n"))

(define iconv-open-inner
  (foreign-lambda* c-pointer ((c-string tocode) (c-string fromcode))
    "iconv_t result = iconv_open(tocode, fromcode);"
    "return(result == (iconv_t) -1 ? NULL : result);"))

(define (iconv-open tocode fromcode)
  (and-let* ((value (iconv-open-inner tocode fromcode)))
    (set-finalizer! value (foreign-lambda void "iconv_close" c-pointer))
    value))

(define iconv
  (case-lambda
    ((cd src) (iconv cd src "?"))
    ((cd src invalid) (iconv cd src invalid (* (string-length src) 2)))
    ((cd src invalid dstlen) (iconv-real cd src invalid dstlen))))

(define iconv-real
  (foreign-safe-lambda* scheme-object ((nonnull-c-pointer cd)
                                       (scheme-object srco)
                                       (scheme-object invalido)
                                       (int bufsize)) #<<EOF
  C_word result;
  size_t srclen = C_header_size(srco), left;
  char *src = C_c_string(srco), *buffer = NULL, *dst = buffer, *tmp;
  int resize = 1;

  do
    {
      if (resize)
        {
          bufsize *= buffer ? 2 : 1;
          tmp = realloc(buffer, bufsize);
          if (!tmp)
            {
              free(buffer);
              return(C_SCHEME_FALSE);
            }
          dst = tmp + (dst - buffer);
          buffer = tmp;
          resize = 0;
        }

      left = buffer + bufsize - dst;
      if (iconv((iconv_t) cd, (char** __restrict) &src, &srclen, &dst, &left) == -1)
        switch (errno)
          {
          case E2BIG:
            resize = 1;
            break;
          case EILSEQ:
          case EINVAL:
            if (dst + C_header_size(invalido) > buffer + bufsize)
              resize = 1;
            else
              {
                C_memcpy(dst, C_c_string(invalido), C_header_size(invalido));
                dst += C_header_size(invalido);
                src ++;
                srclen --;
              }
            break;
          default:
            free(buffer);
            return(C_SCHEME_FALSE);
          }
    }
  while (srclen > 0);

  left = dst - buffer;
  result = iconv_build_result(left);
  C_memcpy(C_c_string(result), buffer, left);
  free(buffer);
  return(result);
EOF
))

)

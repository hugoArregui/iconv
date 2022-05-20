(cond-expand
    (chicken-4
     (import scheme chicken extras foreign iconv tests))
    (chicken-5
     (import scheme (chicken base) (chicken blob) (chicken format) iconv srfi-4 test))
    (else (error "Unsupported chicken version.")))

(define utf8->utf8 (make-iconv "utf8" "utf8"))
(define utf8->utf16 (compose blob->u8vector string->blob (make-iconv "utf16be" "utf8")))
(define utf16->utf8 (compose (make-iconv "utf8" "utf16be") blob->string u8vector->blob))
(define utf8->iso-8859-1 (make-iconv "iso-8859-1" "utf8"))
(define iso-8859-1->utf8 (make-iconv "utf8" "iso-8859-1"))

(test-error "unknown output character set rejected" (make-iconv "abcdef" "utf8"))
(test-error "unknown input character set rejected" (make-iconv "utf8" "abcdef"))

(test "bad UTF-8 sequence" "?abc?def" (utf8->utf8 "\x80abc\x81def"))
(test "Legal UTF8 to legal UTF8" "\xe2\x9c\x8c" (utf8->utf8 "\xe2\x9c\x8c"))
(test "UTF8 to UTF16 big endian" #u8(#x27 #x0c) (utf8->utf16 "\xe2\x9c\x8c"))
(test "UTF16 big endian to UTF8" "\xe2\x9c\x8c" (utf16->utf8 #u8(#x27 #x0c)))
(test "ISO-8859-1 to UTF8" "2\xc3\x972=4" (iso-8859-1->utf8 "2\xd72=4"))
(test "UTF8 to ISO-8859-1" "2\xd72=4" (utf8->iso-8859-1 "2\xc3\x972=4"))

(unless (zero? (test-failure-count))
  (print "=====")
  (printf "===== ~a ~a failed!\n"
          (test-failure-count)
          (if (> (test-failure-count) 1) "tests" "test"))
  (print "====="))
(test-exit)

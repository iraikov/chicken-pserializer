;;;
;;; pserializer-deflate.scm - zlib-compatible compression for pserializer
;;;
;;;   Copyright (c) 2026 CHICKEN adaptation of Shiro Kawai's portable
;;;   serializer.  Original serializer: Copyright (c) 1999 Shiro Kawai,
;;;   distributed under the same conditions as STk.  This module follows
;;;   the design of the pdf egg's pdf-deflate component.
;;;
;;; Bundles miniz.c/miniz.h (public domain / MIT-compatible) to provide
;;; zlib-compatible deflate/inflate with no external library dependency.
;;; The compressed form is an RFC 1950 container: a 2-byte zlib header,
;;; deflate data, and a 4-byte big-endian Adler-32 trailer.
;;;
;;; The pserializer core embeds these byte strings directly in its
;;; compressed frames; nothing in this module needs to know about the
;;; serializer's tag structure.

(module pserializer-deflate

  (flate-compress
   flate-decompress
   flate-compress-bytevector
   flate-compress-bytevector/bv
   flate-decompress-bytevector
   string->byte-bytevector
   bytevector->byte-string)

(import scheme (chicken base) (chicken foreign) (chicken bytevector) pserializer)

(define (subbytevector bv start end)
  (let* ((n (- end start))
         (out (make-bytevector n)))
    (let loop ((i 0))
      (when (< i n)
        (bytevector-u8-set! out i (bytevector-u8-ref bv (+ start i)))
        (loop (+ i 1))))
    out))

;; Installs the compression hooks consulted by the pserializer core,
;; so that loading this module enables compressed frames without the
;; core importing anything from here.
(pserializer-deflate-compress
 (lambda (blob n)
   (call-with-values
       (lambda () (flate-compress-bytevector/bv blob n))
     (lambda (bv len) (values (subbytevector bv 0 len) len)))))

(pserializer-deflate-decompress
 (lambda (blob n)
   (flate-decompress-bytevector blob n)))

#>
#include "miniz.c"

static long pserializer_compress(const unsigned char *src, unsigned long src_len,
                                 unsigned char *dst, unsigned long dst_cap) {
  unsigned long out_len = dst_cap;
  int rc = mz_compress2(dst, &out_len, src, src_len, MZ_DEFAULT_LEVEL);
  return (rc == MZ_OK) ? (long)out_len : -1L;
}

static long pserializer_uncompress(const unsigned char *src, unsigned long src_len,
                                    unsigned char *dst, unsigned long dst_cap) {
  unsigned long out_len = dst_cap;
  unsigned long in_len = src_len;
  int rc = mz_uncompress2(dst, &out_len, src, &in_len);
  if (rc == MZ_OK) return (long)out_len;
  if (rc == MZ_BUF_ERROR) return -1L;   /* destination too small: caller retries bigger */
  return -2L;                          /* corrupt/invalid data: do not retry */
}

static unsigned long pserializer_bound(unsigned long src_len) {
  return mz_compressBound(src_len);
}
<#

(define %compress
  (foreign-lambda long "pserializer_compress"
    nonnull-bytevector unsigned-long nonnull-bytevector unsigned-long))

(define %uncompress
  (foreign-lambda long "pserializer_uncompress"
    nonnull-bytevector unsigned-long nonnull-bytevector unsigned-long))

(define %bound
  (foreign-lambda unsigned-long "pserializer_bound" unsigned-long))

;; Converts a byte string (characters 0..255) to a bytevector.  The
;; string's character codes become byte values; an out-of-range
;; character raises an error.
(define (string->byte-bytevector s)
  (let* ((n (string-length s))
         (bv (make-bytevector n)))
    (let loop ((i 0))
      (when (< i n)
        (bytevector-u8-set! bv i (char->integer (string-ref s i)))
        (loop (+ i 1))))
    bv))

;; Converts the first LEN bytes of a bytevector to a byte string,
;; one character per byte.
(define (bytevector->byte-string bv len)
  (let ((s (make-string len)))
    (let loop ((i 0))
      (when (< i len)
        (string-set! s i (integer->char (bytevector-u8-ref bv i)))
        (loop (+ i 1))))
    s))

;; Compresses a byte string, returning (values bytevector length).
;; The bytevector's capacity may exceed length; callers must use only
;; the first length bytes.
(define (flate-compress-bytevector s)
  (let* ((src (string->byte-bytevector s))
         (n   (string-length s))
         (cap (max 16 (%bound n)))
         (dst (make-bytevector cap)))
    (let ((clen (%compress src n dst cap)))
      (when (< clen 0)
        (error 'flate-compress "compression failed" (string-length s)))
      (values dst clen))))

;; Compresses the first N bytes of a bytevector, returning
;; (values bytevector length).  Same convention as
;; flate-compress-bytevector; the returned bytevector's capacity may
;; exceed length.
(define (flate-compress-bytevector/bv src n)
  (let* ((cap (max 16 (%bound n)))
         (dst (make-bytevector cap)))
    (let ((clen (%compress src n dst cap)))
      (when (< clen 0)
        (error 'flate-compress "compression failed" n))
      (values dst clen))))

;; Compresses a byte string to another byte string holding an
;; RFC 1950 (zlib) stream.
(define (flate-compress s)
  (let-values (((dst clen) (flate-compress-bytevector s)))
    (bytevector->byte-string dst clen)))

;; zlib/RFC1950 streams do not embed the uncompressed length, so
;; flate-decompress cannot size its destination buffer exactly up
;; front.  Guess, and grow on MZ_BUF_ERROR (miniz's signal that the
;; destination was too small and input remains unconsumed).
(define minimum-decompress-buffer 256)
(define initial-decompress-factor 4)
(define maximum-decompress-buffer (* 16 1024 1024))

;; Decompresses an RFC 1950 (zlib) byte string to the original
;; byte string.  Raises an error for corrupt input.
(define (flate-decompress s)
  (let* ((src (string->byte-bytevector s))
         (n   (string-length s)))
    (let loop ((cap (max minimum-decompress-buffer (* initial-decompress-factor (max n 1)))))
      (when (> cap maximum-decompress-buffer)
        (error 'flate-decompress "decompressed data exceeds maximum buffer size"
               maximum-decompress-buffer))
      (let* ((dst (make-bytevector cap))
             (rc  (%uncompress src n dst cap)))
        (cond ((>= rc 0) (bytevector->byte-string dst rc))
              ((= rc -1) (loop (min maximum-decompress-buffer (* cap 2))))
              (else (error 'flate-decompress "invalid or corrupt zlib stream" (string-length s))))))))

;; Decompresses the first N bytes of a bytevector holding an RFC 1950
;; stream, returning the decompressed payload as a bytevector sized
;; exactly to its contents.
(define (flate-decompress-bytevector src n)
  (let loop ((cap (max minimum-decompress-buffer (* initial-decompress-factor (max n 1)))))
    (when (> cap maximum-decompress-buffer)
      (error 'flate-decompress "decompressed data exceeds maximum buffer size"
             maximum-decompress-buffer))
    (let* ((dst (make-bytevector cap))
           (rc  (%uncompress src n dst cap)))
      (cond ((>= rc 0) (let ((out (make-bytevector rc)))
                         (let copy ((i 0))
                           (when (< i rc)
                             (bytevector-u8-set! out i (bytevector-u8-ref dst i))
                             (copy (+ i 1))))
                         out))
            ((= rc -1) (loop (min maximum-decompress-buffer (* cap 2))))
            (else (error 'flate-decompress "invalid or corrupt zlib stream" n))))))

)
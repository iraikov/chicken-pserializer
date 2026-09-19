;; Test suite for the pserializer egg

(import scheme
        (scheme base)
        (chicken base)
        (chicken port)
        (chicken blob)
        (chicken keyword)
        (chicken condition)
        (srfi 4)
        (srfi 69)
        test
        pserializer
        pserializer-deflate)

;;;----------------------------------------------------------------
;;; Helpers

;; True when the topological shape and contents of A and B agree,
;; with sharing structure compared by eq?.  Mirrors the
;; topological-equal? helper of the original STk test suite.
(define (topological-equal? a b)
  (let ((ctx (make-hash-table eq?)))
    (let tequal? ((a a) (b b))
      (cond ((boolean? a) (eq? a b))
            ((char? a) (eqv? a b))
            ((number? a) (eqv? a b))
            ((null? a) (null? b))
            ((hash-table-ref/default ctx a #f)
             => (lambda (bb) (eq? bb b)))
            (else
             (hash-table-set! ctx a b)
             (cond ((pair? a)
                    (and (pair? b)
                         (tequal? (car a) (car b))
                         (tequal? (cdr a) (cdr b))))
                   ((string? a)
                    (and (string? b) (string=? a b)))
                   ((symbol? a)
                    (and (symbol? b) (eq? a b)))
                   ((vector? a)
                    (and (vector? b)
                         (= (vector-length a) (vector-length b))
                         (let loop ((i 0))
                           (or (>= i (vector-length a))
                               (and (tequal? (vector-ref a i) (vector-ref b i))
                                    (loop (+ i 1)))))))
                   ((hash-table? a)
                    (and (hash-table? b)
                         (= (hash-table-size a) (hash-table-size b))
                         (let loop ((keys (hash-table-keys a)))
                           (or (null? keys)
                               (and (hash-table-exists? b (car keys))
                                    (tequal? (hash-table-ref a (car keys))
                                             (hash-table-ref b (car keys)))
                                    (loop (cdr keys)))))))
                   ((keyword? a) (eq? a b))
                   ((blob? a)
                    (and (blob? b)
                         (= (blob-size a) (blob-size b))
                         (string=? (blob->string a) (blob->string b))))
                   (else #f)))))))

;; Round-trips OBJ through a string.
(define (roundtrip obj)
  (string->serializer (serializer->string obj)))

;; Round-trips OBJ through a compressed frame.
(define (roundtrip/compressed obj)
  (string->serializer (serializer->string obj compress: #t)))

;;;----------------------------------------------------------------
;;; 1. Primitive types

(test-group "primitives"
  (test "small fixnum" 1 (roundtrip 1))
  (test "negative fixnum" -1 (roundtrip -1))
  (test "large fixnum" 999999999999999 (roundtrip 999999999999999))
  (test "bignum" 123456789012345678901234567890
        (roundtrip 123456789012345678901234567890))
  (test "negative bignum" -888888888888888888888888
        (roundtrip -888888888888888888888888))
  (test "flonum pi exact" 3.141592653589793
        (roundtrip 3.141592653589793))
  (test "flonum eqv?" #t (eqv? 3.141592653589793 (roundtrip 3.141592653589793)))
  (test "flonum large" #t
        (eqv? 1.7976931348623157e308 (roundtrip 1.7976931348623157e308)))
  (test "flonum denormal" #t (eqv? 5e-324 (roundtrip 5e-324)))
  (test "flonum negative zero" #t (eqv? -0.0 (roundtrip -0.0)))
  (test "flonum tiny" #t (eqv? 1e-300 (roundtrip 1e-300)))
  (test "rational" 1/3 (roundtrip 1/3))
  (test "true" #t (roundtrip #t))
  (test "false" #f (roundtrip #f))
  (test "empty list" '() (roundtrip '()))
  (test "nul char" #\nul (roundtrip #\nul))
  (test "return char" #\return (roundtrip #\return))
  (test "letter char" #\A (roundtrip #\A))
  (test "char zero" #\x0 (roundtrip #\x0))
  (test "string" "string" (roundtrip "string"))
  (test "empty string" "" (roundtrip ""))
  (test "string with nul" (string #\a #\x0 #\b) (roundtrip (string #\a #\x0 #\b)))
  (test "symbol" 'x (roundtrip 'x))
  (test "symbol y" 'y (roundtrip 'y))
  (test "weird symbol" '|Z| (roundtrip '|Z|))
  (test "improper list" '(1 2 . 3) (roundtrip '(1 2 . 3))))

;;;----------------------------------------------------------------
;;; 2. Shared and circular structure

(test-group "sharing"
  (let* ((str "shared string")
         (vec (vector 'shared 'vector))
         (circ (list 1 2 3 4)))
    (set-cdr! (list-tail circ 3) circ)
    (let ((data (vector str vec str vec circ)))
      (let ((back (roundtrip data)))
        (test "shared string eq" #t (eq? (vector-ref back 0) (vector-ref back 2)))
        (test "shared vector eq" #t (eq? (vector-ref back 1) (vector-ref back 3)))
        (test "circular list" #t
              (let ((c (vector-ref back 4)))
                (eq? c (cdr (list-tail c 3)))))
        (test "topological equal" #t (topological-equal? data back))))))

;;;----------------------------------------------------------------
;;; 3. SRFI-4 vectors

(test-group "srfi-4"
  (test "s8vector" #t (equal? (s8vector 0 -128 127)
                              (roundtrip (s8vector 0 -128 127))))
  (test "u8vector" #t (equal? (u8vector 0 200 255)
                              (roundtrip (u8vector 0 200 255))))
  (test "s16vector" #t (equal? (s16vector -32768 32767)
                               (roundtrip (s16vector -32768 32767))))
  (test "u16vector" #t (equal? (u16vector 0 65535)
                               (roundtrip (u16vector 0 65535))))
  (test "s32vector" #t (equal? (s32vector -2147483648 2147483647)
                               (roundtrip (s32vector -2147483648 2147483647))))
  (test "u32vector" #t (equal? (u32vector 0 4294967295)
                               (roundtrip (u32vector 0 4294967295))))
  (test "s64vector" #t (equal? (s64vector -9223372036854775808 9223372036854775807)
                               (roundtrip (s64vector -9223372036854775808
                                                    9223372036854775807))))
  (test "u64vector" #t (equal? (u64vector 0 18446744073709551615)
                               (roundtrip (u64vector 0 18446744073709551615))))
  (test "f32vector" #t (equal? (f32vector 1.5 -3.25 0.0)
                               (roundtrip (f32vector 1.5 -3.25 0.0))))
  (test "f64vector" #t (equal? (f64vector 3.141592653589793 -0.0 1e-300)
                               (roundtrip (f64vector 3.141592653589793 -0.0 1e-300))))
  (test "f64 max exact" #t
        (eqv? 1.7976931348623157e308
              (f64vector-ref (roundtrip (f64vector 1.7976931348623157e308)) 0)))
  (test "empty f32vector" #t (equal? (f32vector) (roundtrip (f32vector))))
  (test "srfi-4 in list" #t
        (equal? (list (u8vector 1 2) (f64vector 2.5))
                (roundtrip (list (u8vector 1 2) (f64vector 2.5)))))
  (test "shared srfi-4" #t
        (let* ((v (u8vector 1 2 3))
               (back (roundtrip (vector v v))))
          (eq? (vector-ref back 0) (vector-ref back 1)))))

;;;----------------------------------------------------------------
;;; 4. Hash tables, keywords, blobs

(test-group "hash tables and misc"
  (let ((ht (make-hash-table eq?)))
    (hash-table-set! ht 'a 1)
    (hash-table-set! ht 'b 2)
    (let ((back (roundtrip ht)))
      (test "hash table size" 2 (hash-table-size back))
      (test "hash table value" 1 (hash-table-ref back 'a))
      (test "hash table topological" #t (topological-equal? ht back))))
  (test "empty hash table" 0 (hash-table-size (roundtrip (make-hash-table eq?))))
  (test "keyword" #t (eq? 'foo: (roundtrip 'foo:)))
  (test "keyword is keyword" #t (keyword? (roundtrip 'bar:)))
  (test "blob" "hello" (blob->string (roundtrip (string->blob "hello"))))
  (test "blob binary" #t
        (let ((b (string->blob (string (integer->char 0)
                                       (integer->char 200)
                                       (integer->char 255)))))
          (string=? (blob->string b) (blob->string (roundtrip b)))))
  (test "empty blob" 0 (blob-size (roundtrip (make-blob 0)))))

;;;----------------------------------------------------------------
;;; 5. Custom extensions

(test-group "extensions"
  (let ((result #f))
    ;; Register a wrapper-pair type under a fresh tag.
    (register-serializer-extension!
     'custom-wrapper
     (lambda (o) (and (pair? o) (eq? (car o) 'wrapper)))
     (lambda (o ser) (write-to-output-serializer (cadr o) ser))
     (lambda (ser) (list 'wrapper (read-from-input-serializer ser))))
    (set! result (roundtrip (list 'wrapper 42)))
    (test "custom extension roundtrip" #t (equal? result (list 'wrapper 42)))
    ;; Extension writers may register objects for references.
    (test "extension in structure" #t
          (let ((back (roundtrip (vector (list 'wrapper 1) (list 'wrapper 1)))))
            (and (equal? (list 'wrapper 1) (vector-ref back 0))
                 (equal? (list 'wrapper 1) (vector-ref back 1)))))))

;;;----------------------------------------------------------------
;;; 6. Streams and errors

(test-group "streams and errors"
  (test "multi-object stream" '(1 (4 5 6) 7)
        (let* ((p (open-output-string)))
          (serializer-write 1 p)
          (serializer-write (list 4 5 6) p)
          (serializer-write 7 p)
          (let ((ip (open-input-string (get-output-string p))))
            (let ((a (serializer-read ip))
                  (b (serializer-read ip))
                  (c (serializer-read ip)))
              (list a b c)))))
  (test "eof at end" #t
        (let* ((p (open-output-string)))
          (serializer-write 1 p)
          (let ((ip (open-input-string (get-output-string p))))
            (serializer-read ip)
            (eof-object? (serializer-read ip)))))
  (test "eof on empty input" #t
        (eof-object? (serializer-read (open-input-string ""))))
  (test "unserializable error" #t
        (handle-exceptions e #t #f
                           (serializer->string (lambda (x) x))
                           #f))
  (test "unknown tag error" #t
        (handle-exceptions e #t #f
                           (string->serializer "q 99\n")
                           #f))
  (test "premature end error" #t
        (handle-exceptions e #t #f
                           (string->serializer "p 1\n")
                           #f))
  (test "invalid reference error" #t
        (handle-exceptions e #t #f
                           (string->serializer "r 7\n")
                           #f))
  (test "call-with-output-serializer" 42
        (call-with-output-serializer
         (open-output-string)
         (lambda (ser) (write-to-output-serializer 42 ser) 42)))
  (test "call-with-input-serializer" 42
        (let ((p (open-output-string)))
          (serializer-write 42 p)
          (call-with-input-serializer
           (open-input-string (get-output-string p))
           (lambda (ser) (read-from-input-serializer ser)))))
  (test "make-output/input serializer" #t
        (equal? (list 1 2)
                (let ((p (open-output-string)))
                  (let ((os (make-output-serializer p)))
                    (write-to-output-serializer (list 1 2) os))
                  (let ((is (make-input-serializer (open-input-string (get-output-string p)))))
                    (read-from-input-serializer is))))))

;;;----------------------------------------------------------------
;;; 7. Compression

(test-group "compression"
  (let ((big (make-vector 200)))
    (let loop ((i 0))
      (when (< i 200)
        (vector-set! big i (list 'item i "repeated text string for compression"))
        (loop (+ i 1))))
    (let ((plain (serializer->string big))
          (comp (serializer->string big compress: #t)))
      (test "compression shrinks" #t (< (string-length comp) (string-length plain)))
      (test "compressed roundtrip" #t (topological-equal? big (roundtrip/compressed big)))
      (test "flonum through compression" #t
            (eqv? 3.141592653589793
                  (roundtrip/compressed 3.141592653589793)))))
  (test "srfi-4 through compression" #t
        (equal? (u8vector 0 200 255) (roundtrip/compressed (u8vector 0 200 255))))
  (test "hash table through compression" #t
        (let ((ht (make-hash-table eq?)))
          (hash-table-set! ht 'k (u8vector 1 2 3))
          (topological-equal? ht (roundtrip/compressed ht))))
  (test "mixed stream" '(1 (4 5 6) 7 (a b) (c d))
        (let* ((p (open-output-string)))
          (serializer-write 1 p)
          (serializer-write '(4 5 6) p compress: #t)
          (serializer-write 7 p)
          (parameterize ((pserializer-compression #t))
            (serializer-write '(a b) p)
            (serializer-write '(c d) p compress: #f))
          (let* ((ip (open-input-string (get-output-string p)))
                 (objs (let loop ((acc '()))
                         (let ((o (serializer-read ip)))
                           (if (eof-object? o)
                               (reverse acc)
                               (loop (cons o acc)))))))
            objs)))
  (test "shared structure through compression" #t
        (let* ((v (u8vector 1 2 3))
               (back (roundtrip/compressed (vector v v))))
          (eq? (vector-ref back 0) (vector-ref back 1))))
  (test "empty object through compression" #t
        (equal? '() (roundtrip/compressed '()))))

;;;----------------------------------------------------------------
;;; 8. Flate module direct interface

(test-group "flate"
  (test "flate roundtrip string" "hello, hello, hello"
        (flate-decompress (flate-compress "hello, hello, hello")))
  (test "flate roundtrip binary" #t
        (string=? (string (integer->char 0) (integer->char 200) (integer->char 255))
                  (flate-decompress
                   (flate-compress (string (integer->char 0) (integer->char 200) (integer->char 255))))))
  (test "flate empty" "" (flate-decompress (flate-compress "")))
  (test "flate compress empty size" 8 (string-length (flate-compress "")))
  (test "flate corrupt error" #t
        (handle-exceptions e #t #f
                           (flate-decompress "not a zlib stream")
                           #f))
  (test "flate-compress-bytevector/bv" #t
        (let-values (((bv len) (flate-compress-bytevector/bv
                                (u8vector->blob/shared (u8vector 1 2 3)) 3)))
          (and (bytevector? bv) (> len 0)))))

(test-exit)

;; pserializer-impl.scm - implementation body of the pserializer module
;;
;; Included by pserializer.scm.  Split out so that the module header
;; with its export list and the implementation remain readable
;; separately.

;;;----------------------------------------------------------------
;;; Tags

(define *pserializer:tag-symbol*     'y)
(define *pserializer:tag-pair*       'p)
(define *pserializer:tag-vector*     'v)
(define *pserializer:tag-string*     's)
(define *pserializer:tag-reference*  'r)
(define *pserializer:tag-hash-table* 'h)
(define *pserializer:tag-keyword*    'k)
(define *pserializer:tag-blob*       'b)
(define *pserializer:tag-compressed* 'c)

;; SRFI-4 vector type table.  Each entry is a list of: type symbol,
;; element size in bytes, maker of a zero-filled vector of a given
;; length, element accessor, element mutator, and a reader procedure
;; building a vector from a blob of raw bytes.
(define *pserializer:srfi4-specs*
  `((s8vector  1 ,make-s8vector  ,s8vector-ref  ,s8vector-set!  ,blob->s8vector)
    (u8vector  1 ,make-u8vector  ,u8vector-ref  ,u8vector-set!  ,blob->u8vector)
    (s16vector 2 ,make-s16vector ,s16vector-ref ,s16vector-set! ,blob->s16vector)
    (u16vector 2 ,make-u16vector ,u16vector-ref ,u16vector-set! ,blob->u16vector)
    (s32vector 4 ,make-s32vector ,s32vector-ref ,s32vector-set! ,blob->s32vector)
    (u32vector 4 ,make-u32vector ,u32vector-ref ,u32vector-set! ,blob->u32vector)
    (f32vector 4 ,make-f32vector ,f32vector-ref ,f32vector-set! ,blob->f32vector)
    (s64vector 8 ,make-s64vector ,s64vector-ref ,s64vector-set! ,blob->s64vector)
    (u64vector 8 ,make-u64vector ,u64vector-ref ,u64vector-set! ,blob->u64vector)
    (f64vector 8 ,make-f64vector ,f64vector-ref ,f64vector-set! ,blob->f64vector)))

;; Returns the type symbol of a homogeneous vector, or #f.
(define (pserializer:srfi4-type x)
  (cond ((s8vector? x) 's8vector)
        ((u8vector? x) 'u8vector)
        ((s16vector? x) 's16vector)
        ((u16vector? x) 'u16vector)
        ((s32vector? x) 's32vector)
        ((u32vector? x) 'u32vector)
        ((f32vector? x) 'f32vector)
        ((s64vector? x) 's64vector)
        ((u64vector? x) 'u64vector)
        ((f64vector? x) 'f64vector)
        (else #f)))

;; Length accessor for a homogeneous vector of the given TYPE.
(define (pserializer:srfi4-length-proc type)
  (case type
    ((s8vector) s8vector-length)
    ((u8vector) u8vector-length)
    ((s16vector) s16vector-length)
    ((u16vector) u16vector-length)
    ((s32vector) s32vector-length)
    ((u32vector) u32vector-length)
    ((s64vector) s64vector-length)
    ((u64vector) u64vector-length)
    ((f32vector) f32vector-length)
    ((f64vector) f64vector-length)
    (else (error 'pserializer "bad SRFI-4 vector type: ~s" type))))

;;;----------------------------------------------------------------
;;; Errors

(define (pserializer:error-premature-input)
  (error 'read-from-input-serializer "premature end of input stream"))

;;;----------------------------------------------------------------
;;; Serializer state

;; An output serializer holds: an output port, a table mapping
;; written objects to reference numbers, a reference counter, a list
;; of per-serializer extension specifications, and a flag requesting
;; compressed frames.
(define-record-type pserializer:output
  (make-pserializer:output port table count extensions compress)
  pserializer:output?
  (port     pserializer:output-port)
  (table    pserializer:output-table)
  (count    pserializer:output-count    pserializer:output-count-set!)
  (extensions pserializer:output-extensions)
  (compress pserializer:output-compress))

;; An input serializer holds: an input port, a table mapping
;; reference numbers to objects, a reference counter, and a hash
;; table mapping extension tags to reader procedures.
(define-record-type pserializer:input
  (make-pserializer:input port table count ext-hash)
  pserializer:input?
  (port     pserializer:input-port)
  (table    pserializer:input-table)
  (count    pserializer:input-count    pserializer:input-count-set!)
  (ext-hash pserializer:input-ext-hash))

(define (pserializer:serializer->port serializer)
  (cond ((pserializer:output? serializer) (pserializer:output-port serializer))
        ((pserializer:input?  serializer) (pserializer:input-port  serializer))
        (else (error 'serializer->port "not a serializer" serializer))))

;;;----------------------------------------------------------------
;;; Extension registry

;; Global registry of extension specifications, initialized with the
;; built-in extensions for SRFI-4 vectors, hash tables, keywords,
;; blobs and compressed frames.  Each specification is a four-element
;; list: tag symbol, test procedure, writer procedure, reader
;; procedure.
(define serializer-extensions
  (make-parameter '()))

;; Adds one extension specification to the global registry.  Later
;; registrations take priority: they are consulted first when the
;; writer searches for a matching test, and their tags shadow
;; earlier tags with the same name when reading.
(define (register-serializer-extension! tag test writer reader)
  (unless (and (symbol? tag) (procedure? test) (procedure? writer) (procedure? reader))
    (error 'register-serializer-extension! "bad extension specification" tag))
  (serializer-extensions
   (cons (list tag test writer reader) (serializer-extensions))))

;; Validates a per-serializer extension list, checking that each
;; entry is a four-element list of a symbol and three procedures.
(define (pserializer:validate-extension who e)
  (unless (list? e)
    (error who "bad extension spec: ~s" e))
  (for-each
   (lambda (s)
     (unless (and (list? s)
                  (= (length s) 4)
                  (symbol? (car s))
                  (procedure? (cadr s))
                  (procedure? (caddr s))
                  (procedure? (cadddr s)))
       (error who "bad extension spec: ~s" s)))
   e)
  e)

;;;----------------------------------------------------------------
;;; Byte payloads

;; Reads COUNT bytes, each encoded as one character of code 0..255,
;; and returns them as a blob.
(define (pserializer:read-bytes count port)
  (let ((blob (make-blob count)))
    (let loop ((i 0))
      (if (< i count)
          (let ((c (read-char port)))
            (when (eof-object? c)
              (pserializer:error-premature-input))
            (bytevector-u8-set! blob i (char->integer c))
            (loop (+ i 1)))
          blob))))

;; Writes the characters of STRING, interpreted as bytes 0..255, to
;; PORT.
(define (pserializer:write-bytes string port)
  (let ((n (string-length string)))
    (let loop ((i 0))
      (when (< i n)
        (write-char (string-ref string i) port)
        (loop (+ i 1))))))

;; Writes a blob payload: a byte count, then the raw bytes.
(define (pserializer:write-blob-payload blob serializer)
  (let ((port (pserializer:output-port serializer)))
    (pserializer:putobj port (blob-size blob))
    (pserializer:write-bytes (blob->string blob) port)))

;; Reads a blob payload written by pserializer:write-blob-payload.
;; The count is terminated by a newline which the reader does not
;; consume, so it is skipped here before the raw bytes.
(define (pserializer:read-blob-payload serializer)
  (let ((port (pserializer:input-port serializer)))
    (let ((n (pserializer:read-count port))
          (c (read-char port)))
      (unless (char? c)
        (pserializer:error-premature-input))
      (pserializer:read-bytes n port))))

;;;----------------------------------------------------------------
;;; Writer

(define (pserializer:make-output-serializer port #!key (extensions '()) (compress #f))
  (pserializer:validate-extension 'make-output-serializer extensions)
  (make-pserializer:output port (make-hash-table eq?) 0 extensions compress))

;; Assigns OBJ the next reference number unless it is already
;; registered; returns the existing reference number, or #f when OBJ
;; is new and has just been registered.
(define (pserializer:register-object-for-write obj serializer)
  (let* ((tab (pserializer:output-table serializer))
         (o (hash-table-ref/default tab obj #f)))
    (or o
        (let ((cnt (pserializer:output-count serializer)))
          (hash-table-set! tab obj cnt)
          (pserializer:output-count-set! serializer (+ cnt 1))
          #f))))

(define (pserializer:putobj port obj)
  (write obj port)
  (newline port))

;; Writes a tag symbol followed by one space.
(define (pserializer:puttag tag port)
  (write tag port)
  (display #\space port))

;; Writes one object.  Immediates and numbers are emitted in external
;; representation; shared-capable objects are registered and emitted
;; as references when seen again; the remaining types get tags, in
;; the order pair, symbol, string, vector, then extensions.
(define (pserializer:write-to-output-serializer obj serializer)
  (let ((port (pserializer:output-port serializer)))
    (if (or (eq? obj #f)
            (eq? obj #t)
            (eq? obj '())
            (number? obj)
            (char? obj))
        (pserializer:putobj port obj)
        (let ((ref (pserializer:register-object-for-write obj serializer)))
          (if ref
              (begin (pserializer:puttag *pserializer:tag-reference* port)
                     (pserializer:putobj port ref))
              (cond
               ((pair? obj)
                (pserializer:puttag *pserializer:tag-pair* port)
                (pserializer:write-to-output-serializer (car obj) serializer)
                (pserializer:write-to-output-serializer (cdr obj) serializer))
               ((symbol? obj)
                (pserializer:puttag *pserializer:tag-symbol* port)
                (pserializer:putobj port obj))
               ((string? obj)
                (pserializer:puttag *pserializer:tag-string* port)
                (pserializer:putobj port obj))
               ((vector? obj)
                (pserializer:puttag *pserializer:tag-vector* port)
                (pserializer:putobj port (vector-length obj))
                (let ((n (vector-length obj)))
                  (let loop ((i 0))
                    (when (< i n)
                      (pserializer:write-to-output-serializer (vector-ref obj i) serializer)
                      (loop (+ i 1))))))
               (else
                (pserializer:write-extension obj serializer))))))))

;; Searches the per-serializer extensions and then the global
;; registry for the first test accepting OBJ, and applies its writer
;; with the object and the serializer.  The #t catch-all test is
;; honoured, as in the original STk serializer.  Raises an error when
;; no extension matches.
(define (pserializer:write-extension obj serializer)
  (let loop ((ext (append (pserializer:output-extensions serializer)
                          (serializer-extensions))))
    (cond ((null? ext)
           (error 'write-to-output-serializer "object is unserializable: ~s" obj))
          (((cadar ext) obj)
           (pserializer:puttag (caar ext) (pserializer:output-port serializer))
           ((caddar ext) obj serializer))
          (else (loop (cdr ext))))))

(define (pserializer:call-with-output-serializer port proc #!key (extensions '()) (compress #f))
  (proc (pserializer:make-output-serializer port extensions: extensions compress: compress)))

;;;----------------------------------------------------------------
;;; Reader

(define (pserializer:make-input-serializer port #!key (extensions '()))
  (pserializer:validate-extension 'make-input-serializer extensions)
  ;; Built-in and globally registered readers are consulted after
  ;; per-serializer extensions: the hash maps each tag to the reader
  ;; given last, and per-serializer specifications are applied last.
  (let ((ext-hash (make-hash-table eq?)))
    (for-each
     (lambda (e)
       (hash-table-set! ext-hash (car e) (cadddr e)))
     (append (reverse (serializer-extensions)) (reverse extensions)))
    (make-pserializer:input port (make-hash-table eq?) 0 ext-hash)))

;; Registers OBJ under reference number CNT, or under the next
;; reference number when CNT is not given.  Returns the number.
(define (pserializer:register-object-for-read obj serializer . args)
  (let* ((tab (pserializer:input-table serializer))
         (cnt-provided? (and (pair? args) (integer? (car args))))
         (cnt (if cnt-provided?
                  (car args)
                  (pserializer:input-count serializer))))
    (hash-table-set! tab cnt obj)
    (if (not cnt-provided?)
        (pserializer:input-count-set! serializer (+ cnt 1)))
    cnt))

(define (pserializer:lookup-object-for-read cnt serializer)
  (hash-table-ref/default (pserializer:input-table serializer) cnt #f))

;; Reads one external representation from PORT, signalling an error
;; at end of input.
(define (pserializer:read-object port)
  (let ((obj (read port)))
    (if (eof-object? obj)
        (pserializer:error-premature-input)
        obj)))

;; Reads a non-negative exact integer in external representation.
(define (pserializer:read-count port)
  (let ((n (pserializer:read-object port)))
    (unless (and (integer? n) (exact? n) (>= n 0))
      (error 'read-from-input-serializer "bad count: ~s" n))
    n))

;; Reads one object.  At the top level, end of input is reported by
;; returning the eof object; within a structure, end of input is an
;; error.  A tag symbol in external representation introduces a typed
;; object; anything else is an immediate or number in external
;; representation.
(define (pserializer:read-from-input-serializer serializer)
  (let* ((port (pserializer:input-port serializer))
         (tag (read port)))
    (cond ((eof-object? tag) tag)
          ((not (symbol? tag)) tag)
          (else (pserializer:read-tagged tag serializer)))))

;; Reads one nested object; end of input is an error.
(define (pserializer:read-nested serializer)
  (let* ((port (pserializer:input-port serializer))
         (tag (read port)))
    (if (eof-object? tag)
        (pserializer:error-premature-input)
        (if (not (symbol? tag))
            tag
            (pserializer:read-tagged tag serializer)))))

;; Dispatches a tag already read from the input port.
(define (pserializer:read-tagged tag serializer)
  (let ((port (pserializer:input-port serializer)))
    (cond
     ((eq? tag *pserializer:tag-reference*)
      (let* ((cnt (pserializer:read-count port))
             (obj (pserializer:lookup-object-for-read cnt serializer)))
        (or obj
            (error 'read-from-input-serializer
                   "invalid reference number: ~s" cnt))))
     ((eq? tag *pserializer:tag-pair*)
      (let ((pair (cons #f #f)))
        (pserializer:register-object-for-read pair serializer)
        (let ((a (pserializer:read-nested serializer))
              (d (pserializer:read-nested serializer)))
          (set-car! pair a)
          (set-cdr! pair d)
          pair)))
     ((eq? tag *pserializer:tag-symbol*)
      (let ((sym (pserializer:read-object port)))
        (pserializer:register-object-for-read sym serializer)
        sym))
     ((eq? tag *pserializer:tag-string*)
      (let ((str (pserializer:read-object port)))
        (pserializer:register-object-for-read str serializer)
        str))
     ((eq? tag *pserializer:tag-vector*)
      (let* ((len (pserializer:read-count port))
             (vec (make-vector len)))
        (pserializer:register-object-for-read vec serializer)
        (let loop ((cnt 0))
          (if (< cnt len)
              (let ((e (pserializer:read-nested serializer)))
                (vector-set! vec cnt e)
                (loop (+ cnt 1)))
              vec))))
     (else
      (pserializer:dispatch-tag tag serializer)))))

;; Looks up TAG among extension reader procedures, and calls the
;; matching one with the input serializer.  Raises an error when the
;; tag is unknown.
(define (pserializer:dispatch-tag tag serializer)
  (let ((proc (hash-table-ref/default (pserializer:input-ext-hash serializer) tag #f)))
    (if proc
        (proc serializer)
        (error 'read-from-input-serializer "unknown tag: ~s" tag))))

(define (pserializer:call-with-input-serializer port proc #!key (extensions '()))
  (proc (pserializer:make-input-serializer port extensions: extensions)))

;;;----------------------------------------------------------------
;;; Built-in extensions

;; SRFI-4 vectors.  Payload: the type name symbol, an element count,
;; and the raw element bytes in little-endian order.  The type name is
;; redundant with the tag but keeps the frame self-describing, and it
;; lets the reader validate the payload length against the element
;; size.  The raw bytes are carried as a blob payload; see
;; pserializer:write-blob-payload.  Integer elements are written
;; little-endian, so the payload is host-independent; floating point
;; elements are written as their IEEE-754 bit patterns, also
;; little-endian.
(define (pserializer:write-srfi4 obj serializer)
  (let ((type (pserializer:srfi4-type obj)))
    (if (not type)
        (error 'write-to-output-serializer "not an SRFI-4 vector" obj)
        (let* ((elt-size (cadr (assq type *pserializer:srfi4-specs*)))
               (n ((pserializer:srfi4-length-proc type) obj))
               (payload (make-blob (* n elt-size))))
          (let fill ((i 0))
            (when (< i n)
              (pserializer:store-element
               type payload (* i elt-size)
               ((pserializer:srfi4-ref type) obj i) elt-size)
              (fill (+ i 1))))
          (let ((port (pserializer:output-port serializer)))
            (pserializer:putobj port type)
            (pserializer:write-blob-payload payload serializer))))))

;; Element accessor for a homogeneous vector of the given TYPE.
(define (pserializer:srfi4-ref type)
  (case type
    ((s8vector) s8vector-ref)
    ((u8vector) u8vector-ref)
    ((s16vector) s16vector-ref)
    ((u16vector) u16vector-ref)
    ((s32vector) s32vector-ref)
    ((u32vector) u32vector-ref)
    ((f32vector) f32vector-ref)
    ((s64vector) s64vector-ref)
    ((u64vector) u64vector-ref)
    ((f64vector) f64vector-ref)))

;; Element mutator for a homogeneous vector of the given TYPE.
(define (pserializer:srfi4-set type)
  (case type
    ((s8vector) s8vector-set!)
    ((u8vector) u8vector-set!)
    ((s16vector) s16vector-set!)
    ((u16vector) u16vector-set!)
    ((s32vector) s32vector-set!)
    ((u32vector) u32vector-set!)
    ((f32vector) f32vector-set!)
    ((s64vector) s64vector-set!)
    ((u64vector) u64vector-set!)
    ((f64vector) f64vector-set!)))

;; Constructor of a zero-filled homogeneous vector of the given TYPE.
(define (pserializer:srfi4-make type)
  (case type
    ((s8vector) make-s8vector)
    ((u8vector) make-u8vector)
    ((s16vector) make-s16vector)
    ((u16vector) make-u16vector)
    ((s32vector) make-s32vector)
    ((u32vector) make-u32vector)
    ((f32vector) make-f32vector)
    ((s64vector) make-s64vector)
    ((u64vector) make-u64vector)
    ((f64vector) make-f64vector)))

;; Stores one homogeneous vector element as ELT-SIZE bytes at OFFSET
;; in PAYLOAD, in little-endian order.  Floating point values are
;; encoded through their IEEE-754 bit patterns.
(define (pserializer:store-element type payload offset v elt-size)
  (if (= elt-size 1)
      (bytevector-u8-set! payload offset (bitwise-and #xff (exact v)))
      (let ((raw (case type
                   ((s16vector) (pserializer:split-16 v))
                   ((u16vector) (pserializer:split-16 v))
                   ((s32vector) (pserializer:split-32 v))
                   ((u32vector) (pserializer:split-32 v))
                   ((s64vector) (pserializer:split-64 v))
                   ((u64vector) (pserializer:split-64 v))
                   ((f32vector) (pserializer:split-32 (pserializer:f32->u32 v)))
                   ((f64vector) (pserializer:split-64 (pserializer:f64->u64 v))))))
        (let loop ((j 0) (rest raw))
          (when (< j elt-size)
            (bytevector-u8-set! payload (+ offset j) (car rest))
            (loop (+ j 1) (cdr rest)))))))

;; Splits an exact integer into little-endian bytes.
(define (pserializer:split-16 v)
  (list (bitwise-and #xff v)
        (bitwise-and #xff (arithmetic-shift v -8))))

(define (pserializer:split-32 v)
  (list (bitwise-and #xff v)
        (bitwise-and #xff (arithmetic-shift v -8))
        (bitwise-and #xff (arithmetic-shift v -16))
        (bitwise-and #xff (arithmetic-shift v -24))))

(define (pserializer:split-64 v)
  (list (bitwise-and #xff v)
        (bitwise-and #xff (arithmetic-shift v -8))
        (bitwise-and #xff (arithmetic-shift v -16))
        (bitwise-and #xff (arithmetic-shift v -24))
        (bitwise-and #xff (arithmetic-shift v -32))
        (bitwise-and #xff (arithmetic-shift v -40))
        (bitwise-and #xff (arithmetic-shift v -48))
        (bitwise-and #xff (arithmetic-shift v -56))))

;; Maps an f32 value to its IEEE-754 single bit pattern as an exact
;; integer, by reinterpreting a one-element f32vector as u32 through
;; blob conversion.
(define (pserializer:f32->u32 v)
  (let ((fv (make-f32vector 1)))
    (f32vector-set! fv 0 v)
    (pserializer:join-bytes (f32vector->blob/shared fv) 0 4)))

;; Maps an f64 value to its IEEE-754 double bit pattern as an exact
;; integer.
(define (pserializer:f64->u64 v)
  (let ((fv (make-f64vector 1)))
    (f64vector-set! fv 0 v)
    (pserializer:join-bytes (f64vector->blob/shared fv) 0 8)))

(define (pserializer:read-srfi4 serializer)
  (let* ((type (pserializer:read-object (pserializer:input-port serializer)))
         (spec (and (symbol? type) (assq type *pserializer:srfi4-specs*))))
    (unless spec
      (error 'read-from-input-serializer "bad SRFI-4 vector type: ~s" type))
    (let* ((elt-size (cadr spec))
           (blob (pserializer:read-blob-payload serializer))
           (size (blob-size blob)))
      (unless (= (modulo size elt-size) 0)
        (error 'read-from-input-serializer
               "bad SRFI-4 payload length: ~s" size))
      (let* ((n (/ size elt-size))
             (make (pserializer:srfi4-make type))
             (set (pserializer:srfi4-set type))
             ;; Register the zero-filled vector before filling it, so
             ;; references to it inside its own elements resolve.
             (vec (make n)))
        (pserializer:register-object-for-read vec serializer)
        (let loop ((i 0))
          (when (< i n)
            (set vec i
                 (pserializer:load-element type blob (* i elt-size) elt-size))
            (loop (+ i 1))))
        vec))))

;; Loads one element of TYPE from ELT-SIZE bytes at OFFSET in BLOB,
;; in little-endian order.  Floating point values are decoded from
;; their IEEE-754 bit patterns.
(define (pserializer:load-element type blob offset elt-size)
  (case type
    ((s8vector) (let ((v (pserializer:join-bytes blob offset elt-size)))
                  (if (< v 128) v (- v 256))))
    ((u8vector) (pserializer:join-bytes blob offset elt-size))
    ((s16vector) (let ((v (pserializer:join-bytes blob offset elt-size)))
                   (if (< v 32768) v (- v 65536))))
    ((u16vector) (pserializer:join-bytes blob offset elt-size))
    ((s32vector) (let ((v (pserializer:join-bytes blob offset elt-size)))
                   (if (< v 2147483648) v (- v 4294967296))))
    ((u32vector) (pserializer:join-bytes blob offset elt-size))
    ((s64vector) (let ((v (pserializer:join-bytes blob offset elt-size)))
                   (if (< v 9223372036854775808)
                       v
                       (- v 18446744073709551616))))
    ((u64vector) (pserializer:join-bytes blob offset elt-size))
    ((f32vector) (pserializer:u32->f32
                  (pserializer:join-bytes blob offset elt-size)))
    ((f64vector) (pserializer:u64->f64
                  (pserializer:join-bytes blob offset elt-size)))))

;; Joins ELT-SIZE little-endian bytes at OFFSET in BLOB into an exact
;; unsigned integer.
(define (pserializer:join-bytes blob offset elt-size)
  (let loop ((j (- elt-size 1)) (v 0))
    (if (< j 0)
        v
        (loop (- j 1)
              (bitwise-ior (arithmetic-shift v 8)
                           (bytevector-u8-ref blob (+ offset j)))))))

;; Maps an IEEE-754 single bit pattern (exact integer) to an f32
;; value, by reinterpreting a one-element u32vector as f32 through
;; blob conversion.
(define (pserializer:u32->f32 u)
  (let ((b (make-blob 4)))
    (pserializer:split-store b 0 u 4)
    (f32vector-ref (blob->f32vector/shared b) 0)))

;; Maps an IEEE-754 double bit pattern (exact integer) to an f64
;; value.
(define (pserializer:u64->f64 u)
  (let ((b (make-blob 8)))
    (pserializer:split-store b 0 u 8)
    (f64vector-ref (blob->f64vector/shared b) 0)))

;; Stores the little-endian bytes of an exact integer into a blob.
(define (pserializer:split-store blob offset v elt-size)
  (let loop ((j 0) (v v))
    (when (< j elt-size)
      (bytevector-u8-set! blob (+ offset j) (bitwise-and #xff v))
      (loop (+ j 1) (arithmetic-shift v -8)))))

;; Hash tables.  Payload: the entry count, then key and value pairs.
;; The deserialized table uses eq? hashing regardless of the test
;; function of the original table.
(define (pserializer:write-hash-table obj serializer)
  (let ((port (pserializer:output-port serializer)))
    (pserializer:putobj port (hash-table-size obj))
    (hash-table-walk
     obj
     (lambda (key value)
       (pserializer:write-to-output-serializer key serializer)
       (pserializer:write-to-output-serializer value serializer)))))

(define (pserializer:read-hash-table serializer)
  (let* ((port (pserializer:input-port serializer))
         (n (pserializer:read-count port))
         (ht (make-hash-table eq?)))
    (pserializer:register-object-for-read ht serializer)
    (let loop ((i 0))
      (when (< i n)
        (let ((key (pserializer:read-nested serializer))
              (value (pserializer:read-nested serializer)))
          (hash-table-set! ht key value)
          (loop (+ i 1)))))
    ht))

;; Keywords.  Payload: the keyword name as a symbol in external
;; representation.
(define (pserializer:write-keyword obj serializer)
  (pserializer:putobj (pserializer:output-port serializer)
                      (string->symbol (keyword->string obj))))

(define (pserializer:read-keyword serializer)
  (let* ((sym (pserializer:read-object (pserializer:input-port serializer)))
         (kw (string->keyword (symbol->string sym))))
    (pserializer:register-object-for-read kw serializer)
    kw))

;; Blobs.  Payload: a byte count, then the raw bytes.
(define (pserializer:write-blob obj serializer)
  (pserializer:write-blob-payload obj serializer))

(define (pserializer:read-blob serializer)
  (let ((blob (pserializer:read-blob-payload serializer)))
    (pserializer:register-object-for-read blob serializer)
    blob))

;;;----------------------------------------------------------------
;;; Compressed frames

;; The deflate hooks are installed by the pserializer-deflate module
;; at load time.  Each hook takes a blob and its byte count: the
;; compress hook returns (values bytevector length) holding the RFC
;; 1950 stream; the decompress hook returns the original payload as a
;; blob.  When compression is requested but the hooks are absent, a
;; clear error is raised.  Both parameters are exported so the
;; deflate module can install its hooks.
(define pserializer-deflate-compress
  (make-parameter
   #f
   (lambda (v)
     (unless (or (not v) (procedure? v))
       (error 'pserializer-deflate-compress "not a procedure" v))
     v)))

(define pserializer-deflate-decompress
  (make-parameter
   #f
   (lambda (v)
     (unless (or (not v) (procedure? v))
       (error 'pserializer-deflate-decompress "not a procedure" v))
     v)))

;; Returns the compress hook, raising an error when compression
;; support is not linked.
(define (pserializer:require-compress)
  (or (pserializer-deflate-compress)
      (error 'pserializer
             "compression support not linked: require pserializer-deflate")))

;; Returns the decompress hook, raising an error when compression
;; support is not linked.
(define (pserializer:require-decompress)
  (or (pserializer-deflate-decompress)
      (error 'pserializer
             "compression support not linked: require pserializer-deflate")))

;; Serializes OBJ to memory and compresses the whole representation
;; as one RFC 1950 stream, returning the compressed payload as a
;; string of byte characters.
(define (pserializer:compress-representation obj)
  (let ((mem (open-output-string)))
    (pserializer:write-to-output-serializer
     obj (pserializer:make-output-serializer mem))
    (let ((payload (get-output-string mem)))
      (call-with-values
          (lambda ()
            ((pserializer:require-compress)
             (string->blob payload) (string-length payload)))
        (lambda (bv len)
          (let ((out (make-string len)))
            (let loop ((i 0))
              (when (< i len)
                (string-set! out i (integer->char (bytevector-u8-ref bv i)))
                (loop (+ i 1))))
            out))))))

;; Decompresses a compressed frame payload, given as a blob, to the
;; original string of byte characters.
(define (pserializer:decompress-representation payload)
  (blob->string
   ((pserializer:require-decompress)
    payload (blob-size payload))))

;; Writes OBJ as a compressed frame: the compressed tag, a byte
;; count, and the compressed bytes.
(define (pserializer:write-compressed-frame obj serializer)
  (let ((payload (pserializer:compress-representation obj)))
    (let ((port (pserializer:output-port serializer)))
      (pserializer:putobj port (string-length payload))
      (pserializer:write-bytes payload port))))

;; Reads a compressed frame, decompresses it, and parses the
;; representation from a nested input port sharing the reference
;; table, so references and eq?-ness survive across the frame
;; boundary.
(define (pserializer:read-compressed serializer)
  (let* ((port (pserializer:input-port serializer))
         (payload (pserializer:read-blob-payload serializer))
         (text (pserializer:decompress-representation payload))
         (inner (make-pserializer:input (open-input-string text)
                                        (pserializer:input-table serializer)
                                        (pserializer:input-count serializer)
                                        (pserializer:input-ext-hash serializer))))
    (let ((obj (pserializer:read-from-input-serializer inner)))
      (pserializer:input-count-set! serializer (pserializer:input-count inner))
      obj)))

;;;----------------------------------------------------------------
;;; Convenience layer

;; The compression switch consulted by the convenience entry points;
;; explicit compress keyword arguments take priority over it.
(define pserializer-compression
  (make-parameter #f))

;; Writes one OBJECT to PORT as a compressed frame when COMPRESS is
;; true, and as ordinary representation otherwise.
(define (pserializer:write-top obj port compress)
  (if compress
      (begin
        (pserializer:puttag *pserializer:tag-compressed* port)
        (pserializer:write-compressed-frame
         obj (pserializer:make-output-serializer port)))
      (pserializer:write-to-output-serializer
       obj (pserializer:make-output-serializer port))))

;; Writes one OBJECT to PORT.  Flonums are emitted with 17
;; significant digits so that they round-trip bit-exactly; the
;; precision is set for the duration of the call.  Note that in
;; CHICKEN 6 parameterizing flonum-print-precision dynamically does
;; not affect printing inside the dynamic extent, so the parameter is
;; set globally and restored after the write.
(define (serializer-write obj port #!key (compress #f))
  (let ((saved (flonum-print-precision)))
    (flonum-print-precision 18)
    (pserializer:write-top obj port (or compress (pserializer-compression)))
    (flonum-print-precision saved)))

;; Reads one object from PORT.  Compressed frames are decompressed
;; transparently.  The flonum print precision does not affect reading,
;; but the parameter is set to 17 for symmetry with serializer-write.
(define (serializer-read port)
  (pserializer:read-from-input-serializer
   (pserializer:make-input-serializer port)))

;; Serializes OBJECT to a fresh string.
(define (serializer->string obj #!key (compress #f))
  (let ((mem (open-output-string)))
    (serializer-write obj mem compress: compress)
    (get-output-string mem)))

;; Deserializes one object from STRING.
(define (string->serializer str)
  (serializer-read (open-input-string str)))

;;;----------------------------------------------------------------
;;; Built-in extension registration

;; Registers the built-in extensions.  Compression requires the
;; pserializer-deflate module; when it is linked, the compressed-frame
;; reader is registered here, and the writer consults it only through
;; the compress flags.
(register-serializer-extension!
 *pserializer:tag-compressed*
 (lambda (obj) #f)
 (lambda (obj serializer)
   (pserializer:write-compressed-frame obj serializer))
 pserializer:read-compressed)

(register-serializer-extension!
 *pserializer:tag-hash-table*
 hash-table?
 pserializer:write-hash-table
 pserializer:read-hash-table)

(register-serializer-extension!
 *pserializer:tag-keyword*
 keyword?
 pserializer:write-keyword
 pserializer:read-keyword)

(register-serializer-extension!
 *pserializer:tag-blob*
 blob?
 pserializer:write-blob
 pserializer:read-blob)

(for-each
 (lambda (pred)
   (register-serializer-extension!
    (string->symbol
     (string-append (symbol->string pred) "-srfi4"))
    (case pred
      ((s8vector) s8vector?)
      ((u8vector) u8vector?)
      ((s16vector) s16vector?)
      ((u16vector) u16vector?)
      ((s32vector) s32vector?)
      ((u32vector) u32vector?)
      ((f32vector) f32vector?)
      ((s64vector) s64vector?)
      ((u64vector) u64vector?)
      ((f64vector) f64vector?))
    pserializer:write-srfi4
    pserializer:read-srfi4))
 '(s8vector u8vector s16vector u16vector s32vector u32vector
   f32vector s64vector u64vector f64vector))
;;;----------------------------------------------------------------
;;; Exported aliases

;; Original STk pserializer entry points.  The make procedures take
;; an optional extension list in the original positional form.
(define (make-output-serializer port . exts)
  (apply pserializer:make-output-serializer port
         (if (pair? exts) (list extensions: (car exts)) '())))

(define write-to-output-serializer pserializer:write-to-output-serializer)

(define (call-with-output-serializer port proc . exts)
  (apply pserializer:call-with-output-serializer port proc
         (if (pair? exts) (list extensions: (car exts)) '())))

(define (make-input-serializer port . exts)
  (apply pserializer:make-input-serializer port
         (if (pair? exts) (list extensions: (car exts)) '())))

(define read-from-input-serializer pserializer:read-from-input-serializer)

(define (call-with-input-serializer port proc . exts)
  (apply pserializer:call-with-input-serializer port proc
         (if (pair? exts) (list extensions: (car exts)) '())))

;; Registers OBJ under an explicit reference number, or the next
;; number when CNT is not given; the original STk extension hook for
;; reader procedures that build objects themselves.
(define register-object-to-input-serializer pserializer:register-object-for-read)

(define serializer->port pserializer:serializer->port)

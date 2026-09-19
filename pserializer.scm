;;;
;;; pserializer.scm - 'portable' serializer for CHICKEN Scheme
;;;
;;;   Copyright (c) 1999 Shiro Kawai (shiro@acm.org)
;;;   Permission to use, modify, and distribute of this code is granted
;;;   under the same condition as of STk.
;;;   CHICKEN 6 adaptation by Ivan Raikov 2026.
;;;
;;; This module serializes standard Scheme objects (boolean, pair,
;;; symbol, string, number, character, vector) plus SRFI-4 homogeneous
;;; numeric vectors, SRFI-69 hash tables, keywords, and blobs.
;;; Serialization converts a Scheme structure to a bytestream that is
;;; independent from the running process; reading it back
;;; ("deserializing") recovers a structure topologically equal to the
;;; original one.  Serialized form is useful to store a Scheme
;;; structure in a file (persistence), or to send it over the network.
;;;
;;; The wire format follows Shiro Kawai's STk pserializer: numbers,
;;; booleans, characters and the empty list are written the same way
;;; as their Scheme external representation; other objects are
;;; preceded by a tag presenting their type.  Objects that may be
;;; shared are assigned a reference number in order of appearance, and
;;; repeated occurrences are written as a reference to that number, so
;;; shared and circular structures round-trip with eq?-ness preserved.
;;; Flonums are written with 18 significant digits so that they
;;; round-trip bit-exactly.
;;;
;;; Extension facility
;;;
;;;   The serializer handles a fixed set of types, and additional
;;;   types can be registered.  A serializer extension consists of
;;;   four elements: a tag symbol, a test procedure, a writer
;;;   procedure and a reader procedure.  When the writer meets an
;;;   object of unknown type, it applies each test procedure in turn;
;;;   on the first true result it writes the tag, then calls the
;;;   writer procedure with two arguments: the object and the output
;;;   serializer.  The writer procedure is expected to emit the object
;;;   using pserializer primitives or ordinary port output.  When the
;;;   reader meets an unknown tag, it calls the reader procedure with
;;;   two arguments: the input serializer and a thunk that reads one
;;;   external representation from the input port.  Extensions are
;;;   registered with register-serializer-extension!, or supplied per
;;;   serializer via the extensions keyword of the serializer
;;;   constructors.  Per-serializer extensions take priority over
;;;   registered ones.
;;;
;;; Compressed frames
;;;
;;;   When compression is enabled at write time, the top-level object
;;;   is first serialized to memory, the whole representation is
;;;   compressed as one RFC 1950 stream by the pserializer-deflate
;;;   module, and it is emitted as a single frame: tag 'c', a byte
;;;   count, and the compressed bytes.  The reader decompresses the
;;;   frame and parses it from a nested port, so compressed and
;;;   uncompressed frames may coexist in one stream.  Compression
;;;   requires the pserializer-deflate extension to be linked.
;;;
;;; External interface
;;;
;;;   make-output-serializer port #!key extensions compress   [procedure]
;;;
;;;     Create an output serializer writing to PORT.  EXTENSIONS is a
;;;     list of extension specifications as described above.  COMPRESS,
;;;     if true, requests that objects written through this serializer
;;;     be emitted as compressed frames.
;;;
;;;   write-to-output-serializer object serializer              [function]
;;;
;;;     Write an OBJECT to the output serializer SERIALIZER.
;;;
;;;   call-with-output-serializer port proc #!key extensions compress
;;;                                                          [function]
;;;
;;;     PROC must take one argument.  An output serializer on PORT is
;;;     created and passed to PROC.
;;;
;;;   make-input-serializer port #!key extensions              [function]
;;;
;;;     Create an input serializer reading from PORT.  EXTENSIONS is
;;;     a list of extension specifications.
;;;
;;;   read-from-input-serializer serializer                    [function]
;;;
;;;     Read one object from the input serializer SERIALIZER.  Returns
;;;     an eof object at the end of the input stream.
;;;
;;;   call-with-input-serializer port proc #!key extensions
;;;                                                          [function]
;;;
;;;     PROC must take one argument.  An input serializer on PORT is
;;;     created and passed to PROC.
;;;
;;;   serializer-write object port #!key compress              [function]
;;;
;;;     Serialize one OBJECT to PORT, creating a temporary output
;;;     serializer.
;;;
;;;   serializer-read port                                     [function]
;;;
;;;     Deserialize one object from PORT, creating a temporary input
;;;     serializer.
;;;
;;;   serializer->string object #!key compress                 [function]
;;;
;;;     Serialize one OBJECT to a fresh string.
;;;
;;;   string->serializer string                                [function]
;;;
;;;     Deserialize one object from STRING.
;;;
;;;   register-serializer-extension! tag test writer reader    [function]
;;;
;;;     Register an extension specification globally, appending it to
;;;     the serializer-extensions parameter.
;;;
;;;   serializer-extensions                                    [parameter]
;;;
;;;     Parameter holding the list of globally registered extension
;;;     specifications.  It is initialized with the built-in
;;;     extensions for SRFI-4 vectors, hash tables, keywords, blobs
;;;     and compressed frames.
;;;
;;;   pserializer-compression                                  [parameter]
;;;
;;;     When set to true, serializer-write and serializer->string
;;;     compress their output by default; explicit compress keyword
;;;     arguments take priority over the parameter.
;;;
;;;   pserializer-deflate-compress, pserializer-deflate-decompress
;;;                                                          [parameter]
;;;
;;;     Hooks installed by the pserializer-deflate module.  Each hook
;;;     takes a blob and its byte count; the compress hook returns
;;;     (values bytevector length) holding the RFC 1950 stream, and
;;;     the decompress hook returns the original payload as a blob.
;;;     Compression is unavailable while both remain unset.

(module pserializer

  (make-output-serializer
   write-to-output-serializer
   call-with-output-serializer
   make-input-serializer
   read-from-input-serializer
   call-with-input-serializer
   serializer-write
   serializer-read
   serializer->string
   string->serializer
   register-serializer-extension!
   serializer-extensions
   pserializer-compression
   pserializer-deflate-compress
   pserializer-deflate-decompress
   register-object-to-input-serializer
   serializer->port)

(import scheme
        (scheme base)
        (chicken base)
        (chicken port)
        (chicken bitwise)
        (chicken blob)
        (chicken bytevector)
        (chicken flonum)
        (chicken keyword)
        (srfi 4)
        (srfi 69)
        )

(include "pserializer-impl.scm")

)

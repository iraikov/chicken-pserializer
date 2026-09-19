# pserializer

A portable serializer for CHICKEN Scheme. It converts Scheme objects to a
byte stream that can be stored in a file or sent over a network, and reads
the stream back into an equivalent object structure. Shared and circular
structure is preserved: objects that appear more than once keep their
`eq?` identity after a round trip.

The module handles standard Scheme data (pairs, vectors, strings, symbols,
numbers, characters, booleans) plus SRFI-4 homogeneous numeric vectors,
SRFI-69 hash tables, keywords, and blobs. Applications can register
handlers for additional types. An optional companion module compresses
serialized output with the zlib stream format.

This is a CHICKEN Scheme adaptation of the portable serializer from
Shiro Kawai's STk / Gauche serializer package. The wire format is unchanged, so
streams written by the original implementation can still be read.

## Installation

Run `chicken-install` in the source directory:

```
chicken-install
```

This builds and installs two extensions: `pserializer` (the serializer
core) and `pserializer-deflate` (compression, with a bundled copy of the
[miniz](https://github.com/richgel999/miniz) deflate implementation).

## Quick start

```scheme
(import pserializer)

(define data (list 1 2.5 "three" 'sym (vector 'a 'b)))

;; Serialize to a string and back
(define text (serializer->string data))
(define copy (string->serializer text))

;; Serialize to a port, one object per call
(serializer-write data (open-output-file "data.ser"))
(define restored (serializer-read (open-input-file "data.ser")))
```

With compression:

```scheme
(import pserializer pserializer-deflate)

(define big (make-vector 10000 "repeated text"))
(define compact (serializer->string big compress: #t))
(define original (string->serializer compact))
```

## Serializable types

The core handles these types:

- Booleans, numbers (exact and inexact), characters, the empty list
- Pairs and improper lists
- Symbols and strings
- Vectors

Registered extensions add:

- SRFI-4 homogeneous numeric vectors (`s8vector`,
  `u8vector`, `s16vector`, `u16vector`, `s32vector`, `u32vector`,
  `s64vector`, `u64vector`, `f32vector`, `f64vector`)
- SRFI-69 hash tables (read back with `eq?` hashing regardless of the
  original test function)
- Keywords
- Blobs (byte blocks)
- Compressed frames (written only on request; read transparently)

Procedures, records created by `define-record`, ports, and other
implementation-specific objects are not serializable unless an
extension is registered for them.

Shared and circular structure round-trips with identity preserved. Two
occurrences of the same string in a tree are again the same string after a
round trip; a cyclic list or vector reads back as the same cycle.

## Serialized format

The format is text and uses the Scheme external representation throughout,
so a serialized stream stays inspectable with ordinary tools.

- Numbers, booleans, characters, and `()` are written directly, one per
  line, as with `write`.
- Every other object starts with a tag symbol followed by its payload:

| Tag | Object      | Payload                                        |
|-----|-------------|------------------------------------------------|
| `y` | symbol      | the symbol, in external representation          |
| `p` | pair        | serialized car, then serialized cdr             |
| `s` | string      | the string, in external representation          |
| `v` | vector      | element count, then each element                |
| `h` | hash table  | entry count, then key/value pairs               |
| `k` | keyword     | keyword name as a symbol                        |
| `b` | blob        | byte count, then raw bytes                      |
| `c` | compressed  | byte count, then raw bytes of a zlib stream     |
| `r` | backref     | reference number of a previously written object |

Each tag line ends with a newline. Binary payloads (blob contents,
homogeneous vector data, compressed data) are embedded as raw bytes
between lines; byte counts are always written as text.

SRFI-4 vectors carry a type name, an element count, and the raw elements.
Integer elements are stored little-endian, so the format is independent of
host byte order. Floating point elements are stored as IEEE 754 bit
patterns, also little-endian.

A compressed frame holds a complete serialized object. The reader
decompresses the frame and parses its contents from memory, so a stream
may mix compressed and uncompressed frames and still share back-references
across frame boundaries.

Flonums are written with 18 significant digits so that every double
precision value round-trips bit-exactly.

## API

### Module `pserializer`

#### Convenience entry points

[procedure] `(serializer-write object port #!key compress)`

Writes one object to `port` as a serialized stream. With `compress: #t`
the object is written as one compressed frame.

[procedure] `(serializer-read port)`

Reads one object from `port`. Returns an eof object when the stream holds
no more objects. Compressed frames are decompressed transparently.

[procedure] `(serializer->string object #!key compress)`

Serializes one object to a fresh string.

[procedure] `(string->serializer string)`

Deserializes one object from `string`. The string must hold exactly one
serialized object.

#### Explicit serializers

[procedure] `(make-output-serializer port #!key extensions compress)`

Creates an output serializer that writes to `port`. `extensions` is a list
of extension specifications (see below). With `compress: #t` every object
written through this serializer is emitted as a compressed frame.

[procedure] `(write-to-output-serializer object serializer)`

Writes one object to an output serializer.

[procedure] `(call-with-output-serializer port proc #!key extensions compress)`

Calls `proc` with one argument: an output serializer on `port`. Returns
the value of `proc`.

[procedure] `(make-input-serializer port #!key extensions)`

Creates an input serializer that reads from `port`.

[procedure] `(read-from-input-serializer serializer)`

Reads one object from an input serializer. Returns an eof object at the
end of the stream.

[procedure] `(call-with-input-serializer port proc #!key extensions)`

Calls `proc` with one argument: an input serializer on `port`. Returns
the value of `proc`.

[accessor] `(serializer->port serializer)`

Returns the port associated with a serializer.

#### Extension registration

[procedure] `(register-serializer-extension! tag test writer reader)`

Registers a handler for one additional object type, globally.

- `tag` is a symbol naming the type in the serialized stream.
- `test` is a predicate; it returns true for objects the extension can
  serialize.
- `writer` is called with two arguments, the object and the output
  serializer, and must emit the object's payload. The tag itself is
  written by the serializer before `writer` runs. Payload elements that
  are ordinary objects should be written with
  `write-to-output-serializer`, so that sharing is preserved.
- `reader` is called with one argument, the input serializer, and must
  read the payload and return the reconstructed object. Payload elements
  should be read with `read-from-input-serializer`.

Later registrations take priority over earlier ones, and per-serializer
extensions (the `extensions:` keyword of the constructors) take priority
over registered ones.

[parameter] `serializer-extensions`

Holds the list of globally registered extension specifications. It is
initialized with the built-in extensions for SRFI-4 vectors, hash tables,
keywords, blobs, and compressed frames.

[procedure] `(register-object-to-input-serializer object serializer [count])`

Registers `object` under reference number `count`, or under the next
reference number when `count` is omitted. Extension readers that build an
object whose payload contains back-references to itself must call this
before reading those references.

#### Parameters

[parameter] `pserializer-compression`

When set to a true value, `serializer-write` and `serializer->string`
compress their output by default. An explicit `compress:` keyword argument
takes priority over this parameter.

[parameter] `pserializer-deflate-compress`

[parameter] `pserializer-deflate-decompress`

Hooks that the `pserializer-deflate` module installs at load time. The
compress hook takes a blob and its byte count and returns two values, a
bytevector and the number of bytes of the zlib stream in it. The
decompress hook takes a blob and its byte count and returns the
decompressed payload as a blob. These hooks are exported so that other
compression backends can be substituted.

### Module `pserializer-deflate`

Loads and bundles miniz, and installs the compression hooks described
above. Importing this module is the only step needed to enable
`compress:` support; the core module never imports it.

[procedure] `(flate-compress string)`

Compresses a byte string (characters with codes 0 to 255) to an RFC 1950
(zlib) stream, returned as a byte string.

[procedure] `(flate-decompress string)`

Decompresses an RFC 1950 stream to the original byte string. Signals an
error for corrupt input.

[procedure] `(flate-compress-bytevector/bv bytevector count)`

Compresses the first `count` bytes of `bytevector`, returning two values:
a bytevector holding the zlib stream, and its length. The returned
bytevector's capacity may exceed the length.

[procedure] `(flate-compress-bytevector string)`

Compresses a byte string, returning two values in the same style as
`flate-compress-bytevector/bv`.

[procedure] `(flate-decompress-bytevector bytevector count)`

Decompresses the first `count` bytes of `bytevector`, returning a
bytevector sized exactly to the decompressed contents.

[procedure] `(string->byte-bytevector string)`

Converts a byte string to a bytevector, one byte per character. Signals an
error for characters outside 0 to 255.

[procedure] `(bytevector->byte-string bytevector count)`

Converts the first `count` bytes of a bytevector to a byte string.

## Writing extensions

An extension serializes objects of one application-defined type. The
example below registers a record type `point`:

```scheme
(import pserializer)

(define-record point x y)

(register-serializer-extension!
 'point                                ; tag symbol
 point?                                ; test
 (lambda (obj ser)                     ; writer: emit payload
   (write-to-output-serializer (point-x obj) ser)
   (write-to-output-serializer (point-y obj) ser))
 (lambda (ser)                         ; reader: read payload, build object
   (let ((p (make-point 0 0)))
     ;; register before reading so back-references resolve
     (register-object-to-input-serializer p ser)
     (let ((x (read-from-input-serializer ser))
           (y (read-from-input-serializer ser)))
       (point-x-set! p x)
       (point-y-set! p y)
       p))))
```

The tag, the test, and the two procedures together form an extension
specification. `register-serializer-extension!` takes the four elements as
separate arguments; the `extensions:` keyword of the serializer
constructors takes a list of such four-element lists instead.

## Notes on behavior

- **eq?-ness.** Objects handled through the reference table (pairs,
  strings, symbols, vectors, SRFI-4 vectors, hash tables, blobs,
  keywords) keep their identity across a round trip. Numbers and
  characters bypass the table, so only their `eqv?`-ness is preserved.
- **Hash tables.** The deserialized table uses `eq?` hashing. The
  original table's test and hash functions are not serialized.
- **Flonums.** Written with 18 significant digits and read back
  bit-exactly.
- **Characters and strings.** Any character code, including codes above
  127 and NUL, round-trips exactly.
- **Errors.** The writer signals an error for objects no extension
  accepts. The reader signals errors for unknown tags, unknown reference
  numbers, and truncated input.

## Testing

Run the bundled test suite after installing:

```
csi -s tests/run.scm
```


## Version History

- 1.0 Initial release

## License

Based on the portable serializer by Shiro Kawai (1999).

Copyright 2026 Ivan Raikov.

BSD 3-Clause license. See [LICENSE](LICENSE).


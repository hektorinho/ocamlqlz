exception Invalid_Compression_Level of int
exception Invalid_QLZ

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let bigstring_empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0
let bigstring_length x = Bigarray.Array1.dim x [@@inline]

let bigstring_create l =
  Bigarray.Array1.create Bigarray.char Bigarray.c_layout l

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let invalid_bounds off len =
  invalid_arg "Out of bounds (off: %d, len: %d)" off len

external unsafe_get_uint8 : bigstring -> int -> int = "%caml_ba_ref_1"
external unsafe_get_char : bigstring -> int -> char = "%caml_ba_ref_1"
external unsafe_set_uint8 : bigstring -> int -> int -> unit = "%caml_ba_set_1"
external unsafe_get_uint16 : bigstring -> int -> int = "%caml_bigstring_get16"
external unsafe_get_uint32 : bigstring -> int -> int32 = "%caml_bigstring_get32"

external unsafe_set_uint32 : bigstring -> int -> int32 -> unit
  = "%caml_bigstring_set32"

external bytes_unsafe_get_uint32 : bytes -> int -> int32 = "%caml_bytes_get32"

let bytes_unsafe_get_uint8 : bytes -> int -> int =
 fun buf off -> Char.code (Bytes.get buf off)

let input_bigstring ic buf off len =
  let tmp = Bytes.create len in
  let res = input ic tmp 0 len in

  let len0 = res land 3 in
  let len1 = res asr 2 in

  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = bytes_unsafe_get_uint32 tmp i in
    unsafe_set_uint32 buf (off + i) v
  done;

  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    let v = bytes_unsafe_get_uint8 tmp i in
    unsafe_set_uint8 buf (off + i) v
  done;
  res

external string_unsafe_get_uint32 : string -> int -> int32
  = "%caml_string_get32"

let string_unsafe_get_uint8 : string -> int -> int =
 fun buf off -> Char.code buf.[off]

let bigstring_of_string v =
  let len = String.length v in
  let res = bigstring_create len in
  let len0 = len land 3 in
  let len1 = len asr 2 in

  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = string_unsafe_get_uint32 v i in
    unsafe_set_uint32 res i v
  done;

  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    let v = string_unsafe_get_uint8 v i in
    unsafe_set_uint8 res i v
  done;
  res

let _version_major = 1
let _version_minor = 5
let _version_revision = 0
let _min_offset = 2
let _unconditional_matchlen = 6
let _uncompressed_end = 4
let _cword_len = 4

type hash = { cache : int; offset : int; offset2 : int array }

type state = {
  stream_buffer : int array;
  stream_counter : int;
  hash : hash array;
  hash_counter : int array;
}

type config = {
  qlz_pointers : int;
  qlz_compression_level : int;
  qlz_hash_values : int;
  qlz_streaming_buffer : int;
  qlz_offset_base : int;
  src : int;
  src_idx : int;
  dst : int;
  dst_idx : int;
  last_byte : int;
  last_matchstart : int;
  fetch : int;
  lits : int;
  size : int;
  cword_ptr : int;
  cword_val : int;
  state : state;
}

let explode s =
  Array.init (String.length s) (fun c -> String.get s c |> Char.code)

let implode arr =
  String.init (Array.length arr) (fun a -> Array.get arr a |> Char.chr)

let size_header src =
  if Array.get src 0 |> Stdlib.Int.logand 2 |> Int.equal then 9 else 3

functor StreamifyFn(S : sig
  eqtype vector
  type slice
  eqtype elem

  val substring : vector * int * int -> slice
  val sub : slice * int -> elem
  val full : vector -> slice
  val size : slice -> int
  val concat : slice list -> vector
  val splitAt : slice * int -> slice * slice
  val isEmpty : slice -> bool
  val triml : int -> slice -> slice
  val splitl : (elem -> bool) -> slice -> slice * slice

  val empty : vector
  val newLine : vector
end) :> STREAMIFY where type vector = S.vector = struct
  type vector = S.vector
  type slice = S.slice

  datatype instream = Nil
                    | Cons of { car : slice,
                                cdr :  instream option ref,
                                read : unit -> slice }


  val emptySlice = S.full S.empty
  val newLine = S.full S.newLine

  fun fromFun read =
        (Cons { car = emptySlice,
                cdr = ref NONE,
                read = S.full o read })

  fun extend (Cons {car, cdr = cdr as (ref NONE), read}) =
        let
          val fragment = read ()
        in
          if S.size fragment = 0
          then 
            cdr := SOME Nil
          else
            cdr := SOME (Cons { car = fragment,
                                cdr = ref NONE,
                                read = read })
        end
    | extend (Cons {car, cdr = ref (SOME _), read}) = ()
    | extend (Nil) = ()

  val revcat = S.concat o rev

  fun inputN (strm, n) =
        let
          fun receive (strm, n, fragments) =
                case strm of
                     Nil => (revcat fragments, Nil)
                   | Cons {car, cdr, read} =>
                       if S.size car >= n then
                         let
                           val (ss, ss') = S.splitAt (car, n)
                         in
                           (revcat (ss::fragments), Cons { car = ss',
                                                           cdr = cdr,
                                                           read = read })
                         end
                       else (
                         extend strm;
                         case cdr of
                              ref (SOME next) =>
                                receive (next, n - S.size car, car::fragments)
                            | ref NONE =>
                                raise Fail "should never reach here")
        in
          receive (strm, n, [])
        end

  fun inputLine strm =
        let
          fun receive (strm, fragments) =
                case strm of
                     Nil =>
                       let
                         val line = revcat fragments
                       in
                         if S.isEmpty (S.full line) then NONE
                         else SOME (S.concat [S.full line, newLine], Nil)
                       end
                   | Cons {car, cdr, read} =>
                       let
                         val (ls, rs) = S.splitl (fn c => c <> S.sub (newLine, 0)) car
                       in
                         if S.isEmpty rs then (
                           extend strm;
                           case cdr of
                                ref (SOME next) =>
                                  receive (next, ls::fragments)
                              | ref NONE =>
                                  raise Fail "should never reach here")
                         else
                           let
                             val line = revcat (newLine::ls::fragments)
                             val rs' = S.triml 1 rs
                           in
                             SOME (line , Cons { car = rs',
                                                 cdr = cdr,
                                                 read = read })
                           end
                       end
        in
          receive (strm, [])
        end

  fun inputAll strm =
        let
          fun receive (strm, fragments) = (
                extend strm;
                case strm of
                     Nil => (revcat fragments, Nil)
                   | Cons {car, cdr = ref (SOME cdr), ...} =>
                       receive (cdr, car::fragments)
                   | Cons {cdr = ref NONE, ...} =>
                       raise Fail "should never reach here")
        in
          receive (strm, [])
        end

  fun inputNoExtend strm =
        let
          fun receive (strm, fragments) =
                case strm of
                     Nil => (revcat fragments, Nil)
                   | Cons {car, cdr = ref (SOME cdr), ...} =>
                       receive (cdr, car::fragments)
                   | Cons {car, cdr = cdr as (ref NONE), read} =>
                       (revcat (car::fragments), Cons {car = emptySlice, cdr = cdr, read = read})
        in
          receive (strm, [])
        end
end

structure TextStream = StreamifyFn(struct
  open Substring

  type vector = string
  type slice = substring
  type elem = char

  val empty = ""
  val newLine = "\n"
end)

structure BinStream = StreamifyFn(struct
  type vector = Word8VectorSlice.vector
  type slice = Word8VectorSlice.slice
  type elem = Word8VectorSlice.elem

  fun substring (s, i, j) = Word8VectorSlice.slice (s, i, SOME j)

  val sub = Word8VectorSlice.sub
  val full = Word8VectorSlice.full
  val size = Word8VectorSlice.length
  val concat = Word8VectorSlice.concat
  val isEmpty = Word8VectorSlice.isEmpty

  fun splitAt (s, i) =
        (Word8VectorSlice.subslice (s, 0, SOME i), Word8VectorSlice.subslice (s, i, NONE))
  fun triml k s = Word8VectorSlice.subslice (s, k, NONE)
  fun splitl f s =
        case Word8VectorSlice.findi (fn (i, e) => not (f e)) s of
             NONE =>
               (Word8VectorSlice.subslice (s, 0, SOME (size s)), Word8VectorSlice.subslice (s, size s, NONE))
           | SOME (i, e) =>
               (Word8VectorSlice.subslice (s, 0, SOME i), Word8VectorSlice.subslice (s, i, NONE))

  val empty = Byte.stringToBytes ""
  val newLine = Byte.stringToBytes "\n"
end)

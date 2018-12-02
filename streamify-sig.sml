signature STREAMIFY = sig
  type instream
  type vector

  (* create a stream from mutable read function *)
  val fromFun : (unit -> vector) -> instream

  val inputN : instream * int -> vector * instream
  val inputAll : instream -> vector * instream
  val inputLine : instream -> (vector * instream) option
  val inputNoExtend : instream -> vector * instream
end

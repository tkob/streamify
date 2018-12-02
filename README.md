# streamify

An SML library for creating a stream from a mutable read function

## Usage Example

    - CM.make "streamify.cm";
    val it = true : bool
    - val src = TextIO.openString "hello world"; (* TextIO.instream is mutable *)
    val src = - : TextIO.instream
    - val ins = TextStream.fromFun (fn () => TextIO.input src); (* create a functional stream *)
    val ins = - : TextStream.instream
    - val (s, ins') = TextStream.inputN (ins, 5); (* applied to ins to read first 5 bytes *)
    val s = "hello" : TextStream.vector
    val ins' = - : TextStream.instream
    - val (s, _) = TextStream.inputN (ins, 4); (* applied to ins and gets first 4 bytes *)
    val s = "hell" : TextStream.vector
    - val (s', _) = TextStream.inputN (ins', 4); (* applied to ins' to read next 4 bytes *)
    val s' = " wor" : TextStream.vector
    - val (s, _) = TextStream.inputN (ins, 4); (* applied to ins and still gets first 4 bytes *)
    val s = "hell" : TextStream.vector

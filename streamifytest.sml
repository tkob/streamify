functor StreamifyTest(S : sig
  include STREAMIFY
  val fromString : string -> vector
  val toString : vector -> string
end) = struct
  structure Assert = SMLUnit.Assert
  structure Test = SMLUnit.Test

  fun fromString (s, n) =
        let
          val strm = TextIO.openString s
        in
          fn () => S.fromString (TextIO.inputN (strm, n))
        end

  fun assert s v =
        Assert.assertEqual (fn (s, s') => s = s') (fn s => S.toString s) (S.fromString s) v

  fun testInputAll () =
        let
          val source = "abcdef"
          val strm = S.fromFun (fromString (source, 2))
          val (actual, strm') = S.inputAll strm
          val (actual2, strm'') = S.inputAll strm'
          val (actual3, strm') = S.inputAll strm
          val (actual4, strm') = S.inputAll strm'
        in
          assert source actual;
          assert  "" actual2;
          assert source actual3;
          assert ""actual4;
          ()
        end

  fun testInputN () =
        let
          val source = "abcdef"
          val strm = S.fromFun (fromString (source, 2))
          val (actual, strm') = S.inputN (strm, 6)
          val (actual2, _) = S.inputN (strm, 1)
          val (actual3, _) = S.inputN (strm, 2)
          val (actual4, strm'') = S.inputN (strm, 3)
          val (actual5, _) = S.inputAll strm'
          val (actual6, _) = S.inputN (strm'', 2)
          val (actual7, _) = S.inputN (strm'', 3)
        in
          assert source actual;
          assert "a" actual2;
          assert "ab" actual3;
          assert "abc" actual4;
          assert "" actual5;
          assert "de" actual6;
          assert "def" actual7;
          ()
        end

  fun testInputLine expectedLines source =
        let
          fun lines strm acc =
                case S.inputLine strm of
                     NONE => rev acc
                   | SOME (line, strm') =>
                       lines strm' (line::acc)
          fun s x = S.fromFun (fromString (source, 2))
          fun assert' l l' = Assert.assertEqualList Assert.assertEqualString l (map S.toString l')
        in
          fn () => assert' expectedLines (lines (s source) [])
        end

  fun testInputNoExtend () =
        let
          val source = "abcdefghijklmnopqrstuvwxyz"
          val strm = S.fromFun (fromString (source, 2))
          val (_, _) = S.inputN (strm, 3)
          val (actual, strm') = S.inputNoExtend strm
          val () = assert "abcd" actual
          val (_, _) = S.inputN (strm', 7)
          val (actual, _) = S.inputNoExtend strm'
          val () = assert "efghijkl" actual
          val (actual, _) = S.inputNoExtend strm
          val () = assert "abcdefghijkl" actual
        in
          ()
        end

  val suite = Test.labelTests [
    ("test inputAll", testInputAll),
    ("test inputN", testInputN),
    ("inputLine \"\"",          testInputLine []                 ""),
    ("inputLine \"\\n\"",       testInputLine ["\n"]             "\n"),
    ("inputLine \"abc\"",       testInputLine ["abc\n"]          "abc"),
    ("inputLine \"abc\\n\"",    testInputLine ["abc\n"]          "abc\n"),
    ("inputLine \"abc\\ndef\"", testInputLine ["abc\n", "def\n"] "abc\ndef"),
    ("inputLine \"a\\n\\nb\"",  testInputLine ["a\n", "\n", "b\n"] "a\n\nb"),
    ("inputNoExtend",  testInputNoExtend),
    ("placeholder", fn () => ())
  ]

  fun run () =
        SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} suite
end

structure TextStreamTest = StreamifyTest(struct
  open TextStream
  fun fromString s = s
  fun toString s = s
end)

structure BinStreamTest = StreamifyTest(struct
  open BinStream
  fun fromString s = Byte.stringToBytes s
  fun toString s = Byte.bytesToString s
end)

val () = TextStreamTest.run ()
val () = BinStreamTest.run ()

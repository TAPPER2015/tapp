structure Test =
struct
  structure Bag = Bag
  structure CLBag = ChunkedListBag
  structure CABag = ChunkedArrayBag
  structure BagTest = MkBagTest(structure Bag = Bag)
  structure CLBagTest = MkBagTest(structure Bag = CLBag)
  structure CABagTest = MkBagTest(structure Bag = CABag)

  val testCase = 10

  (*
    use "BAG.sig";
    use "CHUNK.sig";
    use "bag.sml";
    use "MkChunkedBag.sml";
    use "ChunkedArrayBag.sml";
    use "ChunkedListBag.sml";
    use "MkBagTest.sml";
    use "Test.sml";
  *)

  fun testSpeed n =
    let
      val () = BagTest.calcTimeAverage (BagTest.insNToList (0,n)) [] testCase
      (*val () = BagTest.calcTimeAverage (BagTest.insNToBag (0,n)) (Bag.mkEmpty ()) testCase*)
      val () = CLBagTest.calcTimeAverage (CLBagTest.insNToBag (0,n)) (CLBag.mkEmpty ()) testCase
      val () = CABagTest.calcTimeAverage (CABagTest.insNToBag (0,n)) (CABag.mkEmpty ()) testCase
    in
      ()
    end

  fun testCorrectness n =
    let
      val () = BagTest.stackOrderTest n;
      val () = CLBagTest.stackOrderTest n;
      val () = CABagTest.stackOrderTest n;
    in
      ()
    end
end

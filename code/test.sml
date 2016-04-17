structure Test =
struct
  structure Bag = Bag
  structure CLBag = ChunkedListBag
  structure BagTest = MkBagTest(structure Bag = Bag)
  structure CLBagTest = MkBagTest(structure Bag = CLBag)

  fun test n =
    let
      val () = BagTest.bulkInsertionTest n
      val () = CLBagTest.bulkInsertionTest n
    in
      ()
    end
end

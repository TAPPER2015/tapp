structure Test =
struct
  structure Bag = Bag
  structure CLBag = ChunkedListBag
  structure CABag = ChunkedArrayBag
  structure BagTest = MkBagTest(structure Bag = Bag)
  structure CLBagTest = MkBagTest(structure Bag = CLBag)
  structure CABagTest = MkBagTest(structure Bag = CABag)

  fun test n =
    let
      val () = CLBagTest.bulkInsertionTest n
      val () = CABagTest.bulkInsertionTest n
    in
      ()
    end
end

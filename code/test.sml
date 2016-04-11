structure Test =
struct
  structure Bag = Bag
  structure CBag = ChunkedBag

  val test =
    let
      val e = Bag.mkEmpty ()
      val za = Bag.insert (0, e)
      val zta = Bag.insert (1, e)
      val ce = CBag.mkEmpty ()
      val zca = CBag.insert (0, ce)
    in
      ()
    end
end

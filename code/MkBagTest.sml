functor MkBagTest (structure Bag : BAG) =
struct

  structure Bag = Bag
  open Bag

  fun bulkInsertionTest n =
    let
      fun insN (i,n) b =
        if i < n then
          let
            val _ = print ("Inserting " ^ (Int.toString i) ^ "\n")
            val b' = insert (i, b)
(*             val _ = printBag Int.toString b' *)
          in
            insN (i+1,n) b'
          end
        else
          b
      val empty = mkEmpty ()
      val b = insN (0,n) empty
      val _ = print "** First bag:\n"
      val _ = printBagAsDecimal b
      (*val _ = printBagContents Int.toString b*)
      val _ = printBag Int.toString b

      val m = if (Int.mod (n,2)) = 0 then
                2*n
              else
                2*n + 1
      val c = insN (n,m) empty
      val _ = print "** Second bag:\n"
      val _ = printBagAsDecimal c
      (*val _ = printBagContents Int.toString c*)
      val _ = printBag Int.toString c

      val d = union (b,c)
      val _ = print "** Their union:\n"
      val _ = printBagAsDecimal d
      (*val _ = printBagContents Int.toString d*)
      val _ = printBag Int.toString d

      val (e,f) = split d
      val _ = print "** Their split:\n"
      val _ = print "First bag:\n"
      val _ = printBagAsDecimal e
      (*val _ = printBagContents Int.toString e*)
      val _ = printBag Int.toString e
      val _ = print "Second bag:\n"
      val _ = printBagAsDecimal f
      (*val _ = printBagContents Int.toString f*)
      val _ = printBag Int.toString f
    in
       ()
    end


end

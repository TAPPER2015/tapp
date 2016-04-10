structure ChunkedBag =
struct
  datatype 'a tree =
    Leaf of 'a list
  | Node of int * 'a tree * 'a tree

  datatype 'a digit =
    Zero
  | One of 'a tree

  type 'a buffer = 'a list

  type 'a bag = 'a digit list

  type 'a chunkedbag = 'a buffer * 'a bag

  exception EmptyBag
  exception SingletonTree

  val maxLeafSize = 1024

  (** Utilities **)

  fun listToString toString l =
    let
      fun listToString' toString l' =
        case l' of
          [] => "]"
        | a::ll' => (toString a) ^ "," ^ (listToString' toString ll')
    in
      "[" ^ (listToString' toString l)
    end

  fun listContentToString toString l =
    case l of
      [] => ""
    | a::l' => (toString a) ^ (listContentToString toString l')

  fun treeToString toString t =
    case t of
      Leaf x => "Leaf: " ^ (listToString toString x)
    | Node (w, l, r) =>
      let
        val ls = treeToString toString l
        val rs = treeToString toString r
        val ws = Int.toString w
      in
         "(" ^
         " Node Weight = " ^ ws ^
         "\n... left = " ^ ls ^
         "\n... right = " ^ rs ^
         ")"
      end

  fun treeContentsToString toString t =
    case t of
      Leaf x => listContentToString toString x
    | Node (w, l, r) =>
      let
        val ls = treeContentsToString toString l
        val rs = treeContentsToString toString r
      in
         ls ^ ", " ^ rs
      end

  fun bagToString toString b =
    case b of
      nil => ""
    | d::b' =>
      case d of
        Zero =>
        let
          val bs' = bagToString toString b'
        in
          "__Zero " ^ "\n" ^ bs'
        end
      | One t =>
        let
          val ts = treeToString toString t
          val bs' = bagToString toString b'
        in
          "__One: " ^ ts ^ "\n" ^ bs'
        end

  fun bagContentsToString toString b =
    case b of
      nil => ""
    | d::b' =>
      case d of
        Zero =>
        let
          val bs' = bagContentsToString toString b'
        in
          bs'
        end
      | One t =>
        let
          val ts = treeContentsToString toString t
          val bs' = bagContentsToString toString b'
        in
           ts ^ ", " ^ bs'
        end


  fun bagToDecimal b =
    case b of
      nil => 0
    | d::b' =>
      case d of
        Zero =>
        let
          val n' = bagToDecimal b'
        in
          2*n'
        end
      | One _ =>
        let
          val n' = bagToDecimal b'
        in
          2*n'+1
        end

  fun printTree toString t =
    let
      val s = treeToString toString t
    in
      print ("Tree = \n" ^ s ^ "\n")
    end

  fun printBag toString b =
    let
      val s = bagToString toString b
    in
      print ("Bag = \n" ^ s ^ "\n")
    end

  fun printChunkedBag toString cb =
    let
      val (buf, b) = cb
      val bufs = "Buffer:" ^ "\n" ^ (listToString toString buf) ^ "\n"
    in
      print ("ChunkedBag = \n" ^ bufs);
      printBag toString b
    end

  fun printBagContents toString b =
    let
      val s = bagContentsToString toString b
    in
      print ("Bag Contents = \n " ^ s ^ "\n")
    end

  fun printChunkedBagContents toString cb =
    let
      val (buf, b) = cb
      val bufs = "Buffer Contents:" ^ "\n" ^ (listContentToString toString buf) ^ "\n"
    in
      print ("ChunkedBag Contents = \n" ^ bufs);
      printBagContents toString b
    end

  fun printBagAsDecimal b =
    print ("Decimal value: " ^ Int.toString (bagToDecimal b) ^ "\n")

  fun printChunkedBagAsDecimal cb =
    let
      val (buf, b) = cb
    in
      print ("Decimal value: " ^ Int.toString ((bagToDecimal b) * maxLeafSize + List.length buf) ^ "\n")
    end

  (* size of a tree, constant work *)
  fun sizeTree t =
    case t of
      Leaf x => List.length x
    | Node (w, l, r) =>  w

  (* link two trees, constant work *)
  fun link (l, r) =
    Node (sizeTree l + sizeTree r, l, r)

  (* unlink two trees, constant work *)
  fun unlink t =
    case t of
      Leaf _ => raise SingletonTree
    | Node (_, l, r) => (l,r)

  (* insert a tree into a bag. Interesting invariant re tree sizes.*)
  (* implement using chunking *)
  fun insertTree (t, b) =
    case b of
      nil => [One t]
    | Zero::b' => (One t)::b'
    | One t'::b' =>
    let
      val tt' = link (t,t')
      val b'' = insertTree (tt', b')
    in
      Zero::b''
    end

  (* borrow a tree from a bag. Interesting invariant with trees. *)
  (* implement using chunking *)
  fun borrowTree b =
    case b of
      nil => raise EmptyBag
    | (One t)::nil => (t, nil)
    | (One t)::b' => (t, Zero::b')
    | Zero::b' =>
      let
        val (t', b'') = borrowTree b'
        val Node(_, l, r) = t'
      in
        (l, (One r)::b'')
      end

(*  (** Mainline **)

  (* empty bag *)
  fun mkEmpty () =
    ([], nil)

  (* insert element into a bag *)
  fun insert (x, b) =
    insertTree (Leaf x, b)

  (* remove an element from a bag *)
  fun remove b =
    let
      val (Leaf x, b') = borrowTree b
    in
      (x, b')
    end

  (* union two bags. *)
  fun union (b, c) =
    case (b,c) of
      (_, nil) => b
    | (nil, _) => c
    | (d::b', Zero::c') => d::union(b',c')
    | (Zero::b', d::c') => d::union(b',c')
    | ((One tb)::b', (One tc)::c') =>
      let
        val t = link (tb, tc)
        val bc' = union (b',c')
      in
        Zero::(insertTree (t, bc'))
      end

  fun split b =
    let
      (* even number of elements, split all trees *)
      fun split_even b =
        case b of
          nil => (nil, nil)
        | Zero::b' =>
          let
            val (c,d) = split_even b'
          in
            (Zero::c, Zero::d)
          end
        | (One t)::b' =>
          let
            val (l,r) = unlink t
            val (c,d) = split_even b'
          in
            ((One l)::c, (One r)::d)
          end
     in
       case b of
         nil => (nil, nil)
       | Zero::b' =>
           (* Even number of elements *)
           split_even b'
       | (One t)::b' =>
         (* Odd number of elements *)
         let
           val (c,d) = split_even b'
         in
           (insertTree (t,c), d)
         end
     end

   fun test n =
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
       val _ = printBagContents Int.toString b
       val _ = printBag Int.toString b

       val m = if (Int.mod (n,2)) = 0 then
                 2*n
               else
                 2*n + 1
       val c = insN (n,m) empty
       val _ = print "** Second bag:\n"
       val _ = printBagAsDecimal c
       val _ = printBagContents Int.toString c
       val _ = printBag Int.toString c

       val d = union' (b,c)
       val _ = print "** Their union:\n"
       val _ = printBagAsDecimal d
       val _ = printBagContents Int.toString d
       val _ = printBag Int.toString d

       val (e,f) = split d
       val _ = print "** Their split:\n"
       val _ = print "First bag:\n"
       val _ = printBagAsDecimal e
       val _ = printBagContents Int.toString e
       val _ = printBag Int.toString e
       val _ = print "Second bag:\n"
       val _ = printBagAsDecimal f
       val _ = printBagContents Int.toString f
       val _ = printBag Int.toString f
     in
        ()
     end*)

end

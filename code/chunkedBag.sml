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

  (* invariant:
    1. two buffers act like a stack whose top is the front of bf1,
      and whose bottom is the back of bf2.
    2. when the first buffer is nonempty, the second buffer must be full
    3. when the second buffer is empty, the first buffer must be empty
  *)
  type 'a chunkedbag = 'a buffer * 'a buffer * 'a bag

  exception EmptyBag
  exception IncompleteList
  exception SingletonTree

  val maxLeafSize = 8

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
    | a::l' => (toString a) ^ "," ^ (listContentToString toString l')

  fun treeToString toString t =
    case t of
      Leaf x => "Leaf: " ^ (listToString toString x)
    | Node (w, l, r) =>
      let
        val ls = treeToString toString l
        val rs = treeToString toString r
        val ws = Int.toString (Int.div (w, maxLeafSize))
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

  fun printChunkedBag toString (cb as (bf1, bf2, b)) =
    let
      val bufs = "Buffers:" ^ "\n" ^ (listToString toString bf1) ^ "\n" ^
        (listToString toString bf2) ^ "\n"
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

  fun printChunkedBagContents toString (cb as (bf1, bf2, b)) =
    let
      val bufs = "Buffer Contents:" ^ "\n" ^ (listContentToString toString
        bf1) ^ "\n" ^ (listContentToString toString bf2) ^ "\n"
    in
      print ("ChunkedBag Contents = \n" ^ bufs);
      printBagContents toString b
    end

  fun printBagAsDecimal b =
    print ("Decimal value: " ^ Int.toString (bagToDecimal b) ^ "\n")

  fun printChunkedBagAsDecimal (cb as (bf1, bf2, b)) =
    print ("Decimal value: " ^ Int.toString ((bagToDecimal b) *
     maxLeafSize + (List.length bf1) + (List.length bf2)) ^ "\n")

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

  (* t must be a list with length maxLeafSize *)
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

  (** Mainline **)

  (* empty bag *)
  fun mkEmpty () = (nil, nil, nil)

  (* insert element into a bag *)
  (* insert into the second buffer first, then the first one *)
  (* when the second one is full, call insertTree on the first buffer *)
  (* and move the first one forward *)
  fun insert (x, cb as (bf1, bf2, b)) =
    if (List.length bf2 = maxLeafSize)
    then
      if (List.length bf1 = maxLeafSize)
      then ([], bf1, insertTree (Leaf bf2, b))
      else (x::bf1, bf2, b)
    else (* bf1 must be nil *)
      (bf1, x::bf2, b)


  (* remove an element from a bag *)
  (* remove a full list from tree only when the both list is empty *)
  (* if the second list is empty, the first one must be *)
  fun remove (cb as (bf1, bf2, b)) =
    case (bf1, bf2) of
      (_, nil) => (* bf1 must be nil *)
        let
          val (Leaf buf, b') = borrowTree b
        in
          case buf of
            nil => raise EmptyBag
          | (x::buf') => (x, (bf1, buf', b'))
        end
    | (nil, y2::bf2') => (y2, (bf1, bf2', b))
    | (y1::bf1', _) => (y1, (bf1', bf2, b))

  (* union two bags. *)
  fun unionTree (b, c) =
    case (b,c) of
      (_, nil) => b
    | (nil, _) => c
    | (d::b', Zero::c') => d::unionTree(b',c')
    | (Zero::b', d::c') => d::unionTree(b',c')
    | ((One tb)::b', (One tc)::c') =>
      let
        val t = link (tb, tc)
        val bc' = unionTree (b',c')
      in
        Zero::(insertTree (t, bc'))
      end

    fun union (cb1 as (bfb1, bfb2, b), cb2 as (bfc1, bfc2, c)) =
      let
        val bc = unionTree (b,c)
        (* Given two list whose total size is greater than maxLeafSize *)
        (* Return two list, one list with exactly maxLeafSize many elems *)
        fun takeMaxListOut (l1, l2) =
          let
          (* The second list is expected to be longer the the first one *)
            val r = maxLeafSize - List.length l2
            fun takeMaxListOut' a (l1', l2') =
              if (a = 0) then (l1', l2')
              else case l1' of
                    nil => raise IncompleteList
                  | (x::l1'') => takeMaxListOut' (a-1) (l1'', x::l2')
          in
            takeMaxListOut' r (l1, l2)
          end
        fun insertTwoListToBag (bf1,bf2) b =
          if (List.length bf1 + List.length bf2) <= maxLeafSize
          then (nil, bf1 @ bf2, b)
          else
            let
              val (nbf1, nbf2) = takeMaxListOut (bf1, bf2)
            in
            (* could also be (nil, nbf1, insertTree(Leaf nbf2, b)) *)
              (nbf1, nbf2, b)
            end
      in
        case (bfb1, bfc1) of
          (nil, nil) => insertTwoListToBag (bfb2, bfc2) bc
        | (x::_, nil) => insertTwoListToBag (bfb1, bfc2)
                              (insertTree (Leaf bfb2, bc))
        | (nil, y::_) => insertTwoListToBag (bfc1, bfb2)
                              (insertTree (Leaf bfc2, bc))
        (* Both bfb2 and bfc3 must be full *)
        | (x::_, y:: _) => insertTwoListToBag (bfb1, bfc1)
                  (insertTree (Leaf bfb2, (insertTree (Leaf bfc2, bc))))
      end

  fun splitTree b =
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

   fun split (cb as (bf1, bf2, b)) =
     let
       (* split the list into half *)
       fun splitList l =
         let
           val len = List.length l
           (* invariant: if l2 is nil, l1 must be nil as well *)
           fun splitList' n l1 l2 =
              if (n = 0) then (l1, l2)
              else
                case l2 of
                  nil => (l1, l2)
                | y::l2' => splitList' (n-1) (y::l1) l2'
          in
            splitList' (Int.div (len, 2)) nil l
          end
        val (nbf1, nbf1') = splitList bf1
        val (nbf2, nbf2') = splitList bf2
        val (nb, nb') = splitTree b
      in
        ((nbf1, nbf2, nb), (nbf1', nbf2', nb'))
      end

 fun test n =
     let
       fun insN (i,n) b =
         if i < n then
           let
             val _ = print ("Inserting " ^ (Int.toString i) ^ "\n")
             val b' = insert (i, b)
(*             val _ = printChunkedBag Int.toString b' *)
           in
             insN (i+1,n) b'
           end
         else
           b
       val empty = mkEmpty ()
       val b = insN (0,n) empty
       val _ = print "** First bag:\n"
       val _ = printChunkedBagAsDecimal b
       (*val _ = printChunkedBagContents Int.toString b*)
       val _ = printChunkedBag Int.toString b

       val m = if (Int.mod (n,2)) = 0 then
                 2*n
               else
                 2*n + 1
       val c = insN (n,m) empty
       val _ = print "** Second bag:\n"
       val _ = printChunkedBagAsDecimal c
       (*val _ = printChunkedBagContents Int.toString c*)
       val _ = printChunkedBag Int.toString c

       val d = union (b,c)
       val _ = print "** Their union:\n"
       val _ = printChunkedBagAsDecimal d
       (*val _ = printChunkedBagContents Int.toString d*)
       val _ = printChunkedBag Int.toString d

       val (e,f) = split d
       val _ = print "** Their split:\n"
       val _ = print "First bag:\n"
       val _ = printChunkedBagAsDecimal e
       (*val _ = printChunkedBagContents Int.toString e*)
       val _ = printChunkedBag Int.toString e
       val _ = print "Second bag:\n"
       val _ = printChunkedBagAsDecimal f
       (*val _ = printChunkedBagContents Int.toString f*)
       val _ = printChunkedBag Int.toString f
     in
        ()
     end

end

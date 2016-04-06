structure Bag = 
struct
  datatype 'a tree = 
    Leaf of 'a 
  | Node of int * 'a tree * 'a tree

  datatype 'a digit = 
    Zero 
  | One of 'a tree

  type 'a bag = 'a digit list

  exception EmptyBag
  exception SingletonTree

  (* empty bag *)
  fun mkEmpty () = 
    nil
  
  (* size of a tree, constant work *)
  fun sizeTree t =
    case t of
      Leaf x => 1
    | Node (w, l, r) =>  w

  (** Utilities **)

  fun treeToString toString t = 
    case t of 
      Leaf x => "Leaf: " ^ toString x ^ "" 
    | Node (w, l, r) => 
      let 
        val ls = treeToString toString l
        val rs = treeToString toString r
        val ws = Int.toString w
      in
         "(" ^ 
         " Node Weight = " ^ ws ^ 
         " left = " ^ ls ^ 
         " right = " ^ rs ^ 
         ")"
      end

  fun treeContentsToString toString t = 
    case t of 
      Leaf x =>  toString x 
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
          "__Zero " ^ " " ^ bs' ^ "\n"
        end
      | One t =>
        let 
          val ts = treeToString toString t
          val bs' = bagToString toString b'
        in
          "__One: " ^ ts ^ " " ^ bs'  ^ "\n"   
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
      print ("Bag = \n " ^ s ^ "\n")
    end

  fun printBagContents toString b = 
    let 
      val s = bagContentsToString toString b
    in 
      print ("Bag Contents = \n " ^ s ^ "\n")
    end

  fun printBagAsDecimal b = 
    print ("Decimal value: " ^ Int.toString (bagToDecimal b) ^ "\n")

  (* link two trees, constant work *)
  fun link (l, r) = 
    Node (sizeTree l + sizeTree r, l, r)

  (* unlink two trees, constant work *)
  fun unlink t = 
    case t of 
      Leaf _ => raise SingletonTree
    | Node (_, l, r) => (l,r)
 
  (* insert a tree into a bag. Interesting invariant re tree sizes.*)
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
(* Work in progress 
  (* union two bags with explicity carry. *)
  fun union (b, c) =
    let 
      unionWithCarry carry (b, c) = 
        case (b,c) of 
          (_, nil) => 
            case carry of 
              NONE => b
            | Some t => insertTree (t, b)
        
        | (nil, _) => 
            case carry of 
              NONE => c
            | SOME t => insertTree (t, c)

        | (d::b', Zero::c') => 
            case carry of 
              NONE => d::unionWithCarry NONE (b',c')
            | SOME t =>
                case d of 
                  Zero => (One t)::(unionWithCarry NONE (b',c'))
                | One tb => Zero::(unionWithCarry (SOME (link (t,tb))) (b',c'))

        | (Zero::b', d::c') => d::union(b',c')
            SYMMETRIC

        | ((One tb)::b', (One tc)::c') => 
            SLIGHTLY DIFFERENT
          let
            val t = link (tb, tc)
            val bc' = union (b',c')
          in 
            Zero::(insertTree (t, bc'))
          end
  in 
    unionWithCarry NONE (b, c)
  end
*)

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

       val d = union (b,c)      
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
     end



        
end

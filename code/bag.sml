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

  fun bagToString toString b = 
    case b of 
      nil => ""
    | d::b' =>
      case d of
        Zero => 
        let 
          val bs' = bagToString toString b'
        in
          "\n__Zero " ^ " " ^ bs'   
        end
      | One t =>
        let 
          val ts = treeToString toString t
          val bs' = bagToString toString b'
        in
          "\n__One: " ^ ts ^ " " ^ bs'   
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

  (* link two trees, constant work *)
  fun link (l, r) = 
    Node (sizeTree l + sizeTree r, l, r)

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
       val _ = printBag Int.toString b 
       val c = insN (n,2*n) empty
       val _ = printBag Int.toString c 
       val d = union (b,c)      
       val _ = printBag Int.toString d 
     in 
        ()
     end
        
end

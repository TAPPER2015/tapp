structure Bag = 
struct
  datatype 'a tree = 
    Leaf of 'a 
  | Node of int * 'a * 'a tree * 'a tree

  datatype 'a digit = 
    Zero 
  | One of 'a tree

  type 'a bag = 'a digit list

  (* empty bag *)
  fun mkEmpty () = 
    nil
  
  (* size of a tree, constant work *)
  fun sizeTree t =
    case t of
      Leaf x => 1
    | Node (w, l, r) =  w

  (** Utilities **)

  (* link two trees, constant work *)
  fun link (l, r) = 
    Node (sizeTree l + sizeTree r, l, r)

  (* insert a tree into a bag. Interesting invariant re tree sizes.*)
  fun insertTree (t, b) = 
    case b of
      nil => [One t]
    | Zero::b' => (One t)::b'
    | One t'::b' => 
    let tt' = link (t,t') in
      Zero::(insertTree (tt', b'))
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
  fun insert (x, b) = insertTree (Leaf x, b)


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
      (_, nil) => nil
    | (nil, _) => c
    | (d::b', Zero::c') => d::union(b',c')
    | (Zero::b', d::c') => d::union(b',c')
    | ((One tb::b'), (One tc)::c') => 
      let
        val t = link (tb, tc)
        val bc' = union (b',c')
      in 
        insertTree (t, bc')  *** NO I DON'T THINK THAT THIS IS CORRECT ***
      end
        
    
  
     
end

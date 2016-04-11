structure ArrayChunk : CHUNK = 
struct 
  fun insert x c =
    let (l, contents) = c
    in 
      if l < size then
        Array.modifyi ...
    
   
end

structure ChunkedListBag = MkChunkedBag(Chunk = ArrayChunk)


structure ChunkedListBag = 
struct
  structure Bag = Bag

  type 'a chunk = ('a option) Array.array 
  datatype 'a chunkedListBag = 
    Empty 
  | Full of ('a chunk option * 'a chunk option) Bag.bag

  (* empty bag *)
  fun mkEmpty () = Empty
  
  (** Utilities **)
  fun cbagToString toString b = 
    ...
  fun cbagContentsToString toString b = 
    ..
  (** Mainline **)

  (* insert element into a bag *)
  fun insert (x, b) = 
    case b of 
    ... 
       => Bag.insert (chunk)

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

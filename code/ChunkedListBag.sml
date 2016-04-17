structure ListChunk :> CHUNK =
struct

  type 'a t = 'a list
  type 'a chunk = 'a t

  exception EmptyChunk
  exception InCompleteChunk

  val maxChunkSize = 8

  fun size c = List.length c
  fun empty () = []
  fun isFull c = (size c = maxChunkSize)
  fun isEmpty c = (size c = 0)

  fun push (x,c) = x::c
  fun pop c = case c of nil => raise EmptyChunk
                      | (x::c') => (x, c')

  fun merge (c1, c2) =
    if (size c1 + size c2) <= maxChunkSize
    then (empty (), c1 @ c2)
    else
      let
      (* The second list is expected to be longer the the first one *)
        val r = maxChunkSize - List.length c2
        fun merge' a (c1', c2') =
          if (a = 0) then (c1', c2')
          else case c1' of
                nil => raise InCompleteChunk
              | (x::c1'') => merge' (a-1) (c1'', x::c2')
      in
        merge' r (c1, c2)
      end

 (* split the list into half *)
 fun split c =
   let
     val len = size c
     (* invariant: if l2 is nil, l1 must be nil as well *)
     fun split' n c1 c2 =
        if (n = 0) then (c1, c2)
        else
          case c2 of
            nil => (c1, c2)
          | y::c2' => split' (n-1) (y::c1) c2'
    in
      split' (Int.div (len, 2)) nil c
    end

  fun toString elemToString l =
    let
      fun toString' elemToString l' =
        case l' of
          [] => "]"
        | a::ll' => (elemToString a) ^ "," ^ (toString' elemToString ll')
    in
      "[" ^ (toString' elemToString l)
    end

  fun contentToString elemToString l =
    case l of
      [] => ""
    | a::l' => (elemToString a) ^ ","
              ^ (contentToString elemToString l')

end

structure ChunkedListBag =
  MkChunkedBag(structure Chunk = ListChunk)

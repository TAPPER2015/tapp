signature CHUNK =
sig

  type 'a t
  type 'a chunk = 'a t

  val maxChunkSize : int

  val size : 'a chunk -> int
  val empty : unit -> 'a chunk
  val isFull : 'a chunk -> bool
  val isEmpty : 'a chunk -> bool

  val push : 'a * 'a chunk -> 'a chunk
  val pop : 'a chunk -> 'a * 'a chunk
  val merge : 'a chunk * 'a chunk -> 'a chunk * 'a chunk
  val split : 'a chunk -> 'a chunk * 'a chunk

  val toString : ('a -> string) -> 'a chunk -> string
  val contentToString : ('a -> string) -> 'a chunk -> string

end

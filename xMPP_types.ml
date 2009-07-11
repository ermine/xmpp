module type NETWORK = sig
  type 'a t
  val return: 'a -> 'a t
  val (>>=): 'a t -> ('a -> 'b t) -> 'b t
  val fail: exn -> 'a t
    
  type in_channel
  type out_channel
  val connect: string -> int -> (in_channel * out_channel) t
  val read: in_channel -> string t
  val send: out_channel -> string -> unit t
end

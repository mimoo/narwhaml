module type T = sig
  val broadcast : label:string -> bytes -> unit

  val send : label:string -> public_key:Bls.PublicKey.t -> bytes -> unit
end
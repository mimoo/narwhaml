(** This is an educational implementation of the Narwhall protocol. *)

module Oracle (SomeModule : Hashtbl.HashedType) = struct
  let counter = ref 0

  module Hashtbl = Hashtbl.Make (SomeModule)

  let memory : int Hashtbl.t = Hashtbl.create 5

  let to_bytes (t : SomeModule.t) : string =
    Format.printf "using %d\n" (SomeModule.hash t);
    match Hashtbl.find_opt memory t with
    | Some d -> string_of_int d
    | None ->
        incr counter;
        Hashtbl.add memory t !counter;
        string_of_int !counter
end

module ReliableBroadcast = struct
  let broadcast _msg = failwith "unimplemented"
end

module Transaction = struct
  type t = int

  let for_test _ : t = Random.int 10000
end

type block = {
  source : Bls.PublicKey.t;
  round : int;
  transactions : Transaction.t list;
  certificates : certificate list;
} 
and signed_block = { block : block; signature : Bls.Signature.t }
and certificate = { signed_block : signed_block; signatures : Bls.Signature.t list }


module Block = struct
  type t = block

  let to_bytes t = Marshal.(to_bytes t [ No_sharing ])

  let digest (t : t) =
    let bytes = to_bytes t in
    let digest_str = Digestif.SHA3_256.(to_raw_string @@ digest_bytes bytes) in
    Bytes.of_string digest_str

  let%test_unit "to_bytes" =
    let privkey = Bls.SigningKey.generate () in
    let source = Bls.SigningKey.to_public privkey in
    let round = 1 in
    let transactions = [ Transaction.for_test () ] in
    let certificates = [] in
    let block = { source; round; transactions; certificates } in
    let b1 = to_bytes block in
    let b2 = to_bytes block in
    assert (b1 = b2);
    let other_block = { block with round = 2 } in
    let b3 = to_bytes other_block in
    assert (b3 <> b1)
end

module SignedBlock = struct
  type t = signed_block

  let create ~privkey ~round ~certificates transactions =
    let source = Bls.SigningKey.to_public privkey in
    let block = Block.{ source; round; transactions; certificates } in
    let digest = Block.digest block in
    let signature = Bls.SigningKey.sign privkey digest in
    { block; signature }

  let to_bytes t = Marshal.(to_bytes t [ No_sharing ])

  let digest { block; _ } = Block.digest block
end

module Certificate = struct
  type t = certificate

  let create signed_block : t = { signed_block; signatures = [] }

  let add_signature t signature =
    { t with signatures = signature :: t.signatures }
end

module PublicKeyToValidator = Map.Make (struct
  type t = Bls.PublicKey.t

  let compare = Bls.PublicKey.compare
end)

module RoundToAuthors = Set.Make (Bls.PublicKey)

(* messages that can be sent and received *)

type msg_type = Block | SignedBlock | Certificate

type message = {
  from : Bls.PublicKey.t; (** author of this message *)
  destination : Bls.PublicKey.t option; (** None indicates a broadcast *)
  label : msg_type; (** type of message *)
  data : bytes; (** serialized message *)
}

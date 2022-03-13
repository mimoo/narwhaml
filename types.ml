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

module rec Block : sig
  type t = {
    source : Bls.PublicKey.t;
    round : int;
    transactions : Transaction.t list;
    certificates : Certificate.t list;
  }

  val to_bytes : t -> bytes

  val digest : t -> bytes
end = struct
  type t = {
    source : Bls.PublicKey.t;
    round : int;
    transactions : Transaction.t list;
    certificates : Certificate.t list;
  }

  let to_bytes t = Marshal.(to_bytes t [ No_sharing ])

  let digest (t : Block.t) =
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

and SignedBlock : sig
  type t = { block : Block.t; signature : Bls.Signature.t }

  val create :
    privkey:Bls.SigningKey.t ->
    round:int ->
    certificates:Certificate.t list ->
    Transaction.t list ->
    t

  val to_bytes : t -> bytes

  val digest : t -> bytes
end = struct
  type t = { block : Block.t; signature : Bls.Signature.t }

  let create ~privkey ~round ~certificates transactions =
    let source = Bls.SigningKey.to_public privkey in
    let block = Block.{ source; round; transactions; certificates } in
    let digest = Block.digest block in
    let signature = Bls.SigningKey.sign privkey digest in
    { block; signature }

  let to_bytes t = Marshal.(to_bytes t [ No_sharing ])

  let digest { block; _ } = Block.digest block
end

and Certificate : sig
  type t = { block : Block.t; signatures : Bls.Signature.t list }

  val create : Block.t -> t

  val add_signature : t -> Bls.Signature.t -> t
end = struct
  type t = { block : Block.t; signatures : Bls.Signature.t list }

  let create block : t = { block; signatures = [] }

  let add_signature t signature =
    { t with signatures = signature :: t.signatures }
end

let%test_unit "thing" = print_endline "hello world"

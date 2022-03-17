(** A transaction*)
module Transaction = struct
  type t = int
  (* type t = {
       author: Bls.PublicKey.t;
       destination: Bls.PublicKey.t;
       nonce: int;
       amount: int;
     }*)

  let for_test _ : t = Random.int 10000
end

(* recursive types *)

type block = {
  source : Bls.PublicKey.t;
  round : int;
  transactions : Transaction.t list;
  certificates : certificate list;
}

and signed_block = { block : block; signature : Bls.Signature.t }

and certificate = {
  signed_block : signed_block;
  signatures : Bls.Signature.t list;
}

(** Block logic *)
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

(** Signed block logic *)
module SignedBlock = struct
  type t = signed_block

  let create ~privkey ~round ~certificates transactions =
    let source = Bls.SigningKey.to_public privkey in
    let block = { source; round; transactions; certificates } in
    let digest = Block.digest block in
    let signature = Bls.SigningKey.sign privkey digest in
    { block; signature }

  let to_bytes t = Marshal.(to_bytes t [ No_sharing ])

  let digest { block; _ } = Block.digest block
end

(** Certificate logic *)
module Certificate = struct
  type t = certificate

  let compare = Stdlib.compare

  let create signed_block : t = { signed_block; signatures = [] }

  let add_signature t signature =
    { t with signatures = signature :: t.signatures }
end

(* *)

type phase = Proposed of bytes | ReadyForNewRound

(* types *)

module FromPublicKey = Map.Make (Bls.PublicKey)
(** Hashmap of public key to something *)

module PublicKeySet = Set.Make (Bls.PublicKey)
(** set of public keys *)

module SignatureSet = Set.Make (Bls.Signature)
(** set of signatures *)

module CertificateSet = Set.Make (Certificate)
(** set of certificates *)

type message = {
  from : Bls.PublicKey.t;  (** author of this message *)
  destination : Bls.PublicKey.t option;  (** None indicates a broadcast *)
  label : string;  (** type of message *)
  data : bytes;  (** serialized message *)
}
(** A message that can used to send to a validator, or broadcast to all validators *)

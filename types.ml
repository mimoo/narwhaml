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

and certificate = { block_digest : bytes; signatures : Bls.SignatureSet.t }

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
  type t = { block : block; signature : Bls.Signature.t }

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

  let create block : t =
    let block_digest = Block.digest block in
    { block_digest; signatures = Bls.SignatureSet.empty }

  let add_signature t signature =
    let signatures = Bls.SignatureSet.add signature t.signatures in
    { t with signatures }

  let to_bytes t = Marshal.(to_bytes t [ No_sharing ])
end

(* *)

type phase = Proposed of bytes | WaitForCerts | ReadyForNewRound

(* types *)

module CertificateSet = Set.Make (Certificate)
(** set of certificates *)

type message = {
  from : Bls.PublicKey.t;  (** author of this message *)
  destination : Bls.PublicKey.t option;  (** None indicates a broadcast *)
  label : string;  (** type of message *)
  data : bytes;  (** serialized message *)
}
(** A message that can used to send to a validator, or broadcast to all validators *)

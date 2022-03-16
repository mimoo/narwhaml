(** BLS signature *)

module Signature = struct
  type t = Bls12_381.Signature.MinSig.signature

  let to_bytes : t -> bytes = Bls12_381.Signature.MinSig.signature_to_bytes

  let to_hex signature : string = Hexstring.encode @@ to_bytes signature
end

module PublicKey = struct
  type t = Bls12_381.Signature.MinSig.pk

  let to_bytes : t -> bytes = Bls12_381.Signature.MinSig.pk_to_bytes

  let to_hex pk : string = Hexstring.encode @@ to_bytes pk

  let verify pk msg signature : bool =
    Bls12_381.Signature.MinSig.Basic.verify pk msg signature

  let compare = Stdlib.compare
end

module SigningKey = struct
  type t = Bls12_381.Signature.sk

  let generate _ : t =
    let ikm = Bytes.create 32 in
    Randoml.rand_fill ikm;
    Bls12_381.Signature.generate_sk ikm

  let to_bytes : t -> bytes = Bls12_381.Signature.sk_to_bytes

  let to_hex sk : string = Hexstring.encode @@ to_bytes sk

  let to_public : t -> PublicKey.t = Bls12_381.Signature.MinSig.derive_pk

  let sign sk (msg : bytes) : Signature.t =
    Bls12_381.Signature.MinSig.Basic.sign sk msg
end

let%test "sign & verify" =
  let key = SigningKey.generate () in
  let serialized_key = SigningKey.to_hex key in
  Format.printf "priv: %s\n" serialized_key;
  let pubkey = SigningKey.to_public key in
  let serialized_pubkey = PublicKey.to_hex pubkey in
  Format.printf "pub: %s\n" serialized_pubkey;
  let msg = Bytes.of_string "hello" in
  let signature = SigningKey.sign key msg in
  let serialized_sig = Signature.to_hex signature in
  Format.printf "sig: %s\n" serialized_sig;
  let verified = PublicKey.verify pubkey msg signature in
  Format.printf "verified: %B\n" verified;
  verified

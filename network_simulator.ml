(*
module type Validators = sig
  val t : Types.Validator.t Types.PublicKeyToValidator.t
end

module Network (Validators : Validators) = struct
  let network_latency_in_secs = 10

  let send ~(label : string) ~(public_key : Bls.PublicKey.t) bytes =
    Unix.sleep network_latency_in_secs;
    let validator : Types.Validator.t =
      Types.PublicKeyToValidator.find public_key Validators.t
    in
    validator.recv ~label bytes
  (* Validators.t  *)

  let broadcast ~(label : string) bytes =
    Unix.sleep network_latency_in_secs;
    let f _public_key (thing : Types.Validator.t) = thing.recv ~label bytes in
    Types.PublicKeyToValidator.iter f Validators.t
end
*)
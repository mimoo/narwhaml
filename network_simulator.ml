module PublicKeyToValidator = Map.Make (struct
  type t = Bls.PublicKey.t

  let compare = Bls.PublicKey.compare
end)

module Validator = struct
  type t = { recv : label:string -> bytes -> unit }
end

module type Validators = sig
  val t : Validator.t PublicKeyToValidator.t
end

module Network (Validators : Validators) = struct
  (*
  let test_network ~num_validators =
    let _validators =
      List.init num_validators (fun _ -> Validator.new_validator ())
    in
    ()
    *)

  let network_latency_in_secs = 10

  let send ~(label : string) ~(public_key : Bls.PublicKey.t) bytes =
    Unix.sleep network_latency_in_secs;
    let validator : Validator.t =
      PublicKeyToValidator.find public_key Validators.t
    in
    validator.recv ~label bytes
  (* Validators.t  *)

  let broadcast ~(label : string) bytes =
    Unix.sleep network_latency_in_secs;
    let f _public_key (thing : Validator.t) = thing.recv ~label bytes in
    PublicKeyToValidator.iter f Validators.t
end

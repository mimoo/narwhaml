module type Validators = sig
  val validators : Core.Validator.t Types.PublicKeyToValidator.t
end

module NetworkSimulator (Validators : Validators) = struct
  let network_latency_in_secs = 10

  let validators : Core.Validator.t Types.PublicKeyToValidator.t =
    Validators.validators

  let send ~from ~(public_key : Bls.PublicKey.t) ~(label : string) bytes =
    Unix.sleep network_latency_in_secs;
    let validator : Core.Validator.t =
      Types.PublicKeyToValidator.find public_key validators
    in
    Core.Mempool.receive_data validator.mempool ~from ~label bytes

  let broadcast ~from ~(label : string) bytes =
    Unix.sleep network_latency_in_secs;
    let f _public_key (validator : Core.Validator.t) =
      Core.Mempool.receive_data validator.mempool ~from ~label bytes
    in
    Types.PublicKeyToValidator.iter f validators
end

module Network
    (Validators : Validators)
    (Mempool : module type of Core.Mempool
                 with module Network := NetworkSimulator(Validators)) =
struct
  include Mempool
end

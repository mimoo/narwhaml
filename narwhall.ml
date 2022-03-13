
(*
module Validator = struct
  type t = { privkey : Bls.SigningKey.t; mempool : Mempool.t }

  let new_validator _ =
    { privkey = Bls.SigningKey.generate (); mempool = Mempool.new_mempool () }
end

module Network = struct
  let test_network ~num_validators =
    let _validators =
      List.init num_validators (fun _ -> Validator.new_validator ())
    in
    ()
end
*)

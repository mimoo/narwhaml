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

(*
let () =
  let num_validators = ref 10 in
  let validators = ref [] in
  let committee = ref Types.PublicKeyToValidator.empty in
  for i = 0 to !num_validators do
    let signing_key = Bls.SigningKey.generate () in
    let public_key = Bls.SigningKey.to_public signing_key in
    let validator = {
      mempool = ;
      recv = ;
      send = ;
      broadcast = ;
    } in
    validators := validator :: !validators;
    committee := Types.PublicKeyToValidator.add public_key validator !committee;
    num_validators := !num_validators - 1
  done
*)
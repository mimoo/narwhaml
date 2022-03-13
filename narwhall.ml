let%test_unit "simulating" =
  let num_validators = ref 10 in
  let validators = ref [] in
  let validators_signing_keys = ref [] in
  let validators_public_keys = ref [] in
  let public_key_to_validator = ref Types.PublicKeyToValidator.empty in

  (* generate the public keys *)
  for _ = 0 to !num_validators do
    let signing_key = Bls.SigningKey.generate () in
    validators_signing_keys := signing_key :: !validators_signing_keys;
    let public_key = Bls.SigningKey.to_public signing_key in
    validators_public_keys := public_key :: !validators_public_keys
  done;

  let f (signing_key, public_key) =
    let validator =
      Core.Validator.
        {
          mempool =
            Core.Mempool.new_mempool ~signing_key
              ~validators:!validators_public_keys;
        }
    in
    validators := validator :: !validators;
    public_key_to_validator :=
      Types.PublicKeyToValidator.add public_key validator
        !public_key_to_validator
  in
  List.iter f (List.combine !validators_signing_keys !validators_public_keys)

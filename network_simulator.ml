module PubkeyToChannel = Map.Make (Bls.PublicKey)

let rec run_network (pubkey_to_channel, recv_channels) =
  print_endline "running network";

  (* listen from all validators *)
  let events = List.map Event.receive recv_channels in
  Format.printf "done receiving, now syncing\n";

  (* forward to validator *)
  let Types.{ from; label; _ } = Event.select events in
  Format.printf "received %s from %s\n" label (Bls.PublicKey.to_hex from);

  run_network (pubkey_to_channel, recv_channels)

let new_simulation num_validators =
  Format.printf "starting simulation with %d validators\n" num_validators;

  (* generate signing_keys *)
  let signing_keys = ref [] in
  for _ = 0 to num_validators do
    let signing_key = Bls.SigningKey.generate () in
    signing_keys := signing_key :: !signing_keys
  done;

  (* generate public keys *)
  let validators_pubkeys = List.map Bls.SigningKey.to_public !signing_keys in

  (* generate channels *)
  let send_channels = ref [] in
  let recv_channels = ref [] in
  for _ = 0 to num_validators do
    let send : Types.message Event.channel = Event.new_channel () in
    let recv : Types.message Event.channel = Event.new_channel () in
    send_channels := send :: !send_channels;
    recv_channels := recv :: !recv_channels
  done;

  (* map public keys to channels *)
  let pubkey_to_channel = ref PubkeyToChannel.empty in
  let add_to_hashmap public_key send_channel =
    pubkey_to_channel :=
      PubkeyToChannel.add public_key send_channel !pubkey_to_channel
  in
  List.iter2 add_to_hashmap validators_pubkeys !send_channels;

  (* create a new thread for every validator, returns the handle *)
  let create_validator_thread signing_key (send, recv) =
    (* a send (resp. recv) channel becomes a recv (resp. send) channel for a validator *)
    let send, recv = (recv, send) in
    let validator =
      Core.Validator.new_validator signing_key validators_pubkeys ~send ~recv
    in
    let handle = Thread.create Core.Validator.run validator in
    handle
  in
  let channels = List.combine !send_channels !recv_channels in
  let _handles = List.map2 create_validator_thread !signing_keys channels in

  (* run the validators *)
  run_network (!pubkey_to_channel, !recv_channels)

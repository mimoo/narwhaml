module PubkeyToChannel = Map.Make (Bls.PublicKey)

(** logging *)
let log public_key msg =
  let source = Bls.PublicKey.log public_key in
  Format.printf "[0x%s] %s\n" source msg;
  print_newline ()

(* logic behind dispatching messages from validators to validators *)
let dispatch_msg pubkey_to_channel (msg : Types.message) =
  match msg.destination with
  (* direct send *)
  | Some public_key ->
      log msg.from
        (Format.sprintf "sent a message to %s" (Bls.PublicKey.log public_key));
      let send = PubkeyToChannel.find public_key pubkey_to_channel in
      Event.sync (Event.send send msg)
  (* broadcast *)
  | None ->
      log msg.from (Format.sprintf "broadcasted a message");
      let f public_key send =
        if not (public_key = msg.from) then Event.sync (Event.send send msg)
      in
      PubkeyToChannel.iter f pubkey_to_channel

(** runs the network loop *)
let rec run_network (pubkey_to_channel, recv_channels) =
  print_endline "listening...";
  (* listen from all validators *)
  let events = List.map Event.receive recv_channels in

  (* forward to validator *)
  let msg : Types.message = Event.select events in
  print_endline "going to dispatch";
  dispatch_msg pubkey_to_channel msg;
  print_endline "dispatched";

  (* 5 seconds per msg, to be able to follow flow *)
  Unix.sleep 10;
  print_endline "ended sleeping";
  run_network (pubkey_to_channel, recv_channels)

(** sets up the simulated network given a number of validators *)
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
    let validator =
      Core.Validator.new_validator signing_key validators_pubkeys ~send:recv
        ~recv:send
    in
    let handle = Thread.create Core.Validator.run validator in
    handle
  in
  let channels = List.combine !send_channels !recv_channels in
  let _handles = List.map2 create_validator_thread !signing_keys channels in

  (* run the validators *)
  print_endline "starting the network";
  run_network (!pubkey_to_channel, !recv_channels)

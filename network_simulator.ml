module PubkeyToChannel = Map.Make (Bls.PublicKey)

(** logging *)
let log Types.{ from; destination; label; _ } action =
  let from = Bls.PublicKey.log from in
  let destination =
    match destination with
    | None -> "everyone"
    | Some destination -> Bls.PublicKey.log destination
  in
  Format.printf "[0x%s] %s '%s' to %s" from action label destination;
  print_newline ()

module Network = struct
  type state = {
    pubkey_to_channel : Types.message Event.channel PubkeyToChannel.t;
    mutable sending_queue : Types.message Queue.t;
    recv_channels : Types.message Event.channel list;
  }

  let gen pubkey_to_channel recv_channels =
    { pubkey_to_channel; sending_queue = Queue.create (); recv_channels }

  (* logic behind dispatching messages from validators to validators *)
  let dispatch_msg state (msg : Types.message) =
    let send_or_queue state send (msg : Types.message) =
      match Event.poll (Event.send send msg) with
      | None ->
          log msg "couldn't send";
          Queue.push msg state.sending_queue
      | Some () -> log msg "successfuly sent"
    in
    match msg.destination with
    (* direct send *)
    | Some public_key ->
        log msg "sent";
        let send = PubkeyToChannel.find public_key state.pubkey_to_channel in
        send_or_queue state send msg
    (* broadcast *)
    | None ->
        log msg "broadcasted";
        let f public_key send =
          let msg = { msg with destination = Some public_key } in
          if not (public_key = msg.from) then send_or_queue state send msg
        in
        PubkeyToChannel.iter f state.pubkey_to_channel

  (** runs the network loop *)
  let rec run_network state =
    print_endline "listening...";
    (* check queue first *)
    (match Queue.take_opt state.sending_queue with
    | None -> print_endline "nothing in the queue"
    | Some msg ->
        Format.printf "found %d messages in the queue\n"
          (Queue.length state.sending_queue);
        dispatch_msg state msg);

    (* then check recv_channels *)

    (* listen from all validators *)
    let events = List.map Event.receive state.recv_channels in

    (* forward to validator *)
    let msg : Types.message = Event.select events in
    dispatch_msg state msg;

    (* sleep to be able to follow the flow *)
    Unix.sleep 1;
    run_network state
end

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
  let validators_pubkeys = Bls.PublicKeySet.of_list validators_pubkeys in
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
  let state = Network.gen !pubkey_to_channel !recv_channels in
  Network.run_network state

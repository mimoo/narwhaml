module DigestMap = Map.Make (Bytes)

module Mempool = struct
  type round_state = {
    received_certificates : Types.CertificateSet.t;
    received_signatures : Bls.Signature.t Bls.PublicKeyMap.t;
    round : int;
    phase : Types.phase;
  }
  (** gets re-ininitialized at every new round *)

  type t = {
    (* about *)
    signing_key : Bls.SigningKey.t;
    public_key : Bls.PublicKey.t;
    (* constants *)
    validators_pubkeys : Bls.PublicKeySet.t;
    two_f_plus_one : int;
    (* communication *)
    send : Types.message Event.channel;
    recv : Types.message Event.channel;
    (* storage *)
    mutable digest_to_block : Types.SignedBlock.t DigestMap.t;
    mutable pending_transactions : Types.Transaction.t list;
    (* round state *)
    mutable round_state : round_state;
  }
  (** contains the state of the mempool *)

  (* initialization functions *)

  (** create a new fresh round state given a `round` *)
  let new_round_state round =
    {
      received_certificates = Types.CertificateSet.empty;
      received_signatures = Bls.PublicKeyMap.empty;
      round;
      phase = Types.ReadyForNewRound;
    }

  (** creates a new mempool state from a signing key, a list of validator public keys,
        and two channels to communicate with the network simulator
    *)
  let new_mempool signing_key validators_pubkeys ~send ~recv : t =
    let num_validators = Bls.PublicKeySet.cardinal validators_pubkeys in
    let two_f_plus_one = (2 * ((num_validators - 1) / 3)) + 1 in
    let round_state = new_round_state 0 in
    {
      signing_key;
      public_key = Bls.SigningKey.to_public signing_key;
      send;
      recv;
      digest_to_block = DigestMap.empty;
      pending_transactions = [];
      validators_pubkeys;
      two_f_plus_one;
      round_state;
    }

  (* logging *)

  let log t msg =
    let source = Bls.PublicKey.log t.public_key in
    Format.printf "[0x%s] %s\n" source msg;
    print_newline ()

  (* communication *)

  let broadcast t ~label data =
    Event.sync
      (Event.send t.send
         Types.{ from = t.public_key; destination = None; label; data })

  let send t ~public_key ~label data =
    Event.sync
      (Event.send t.send
         Types.
           { from = t.public_key; destination = Some public_key; label; data })

  let recv t : Types.message = Event.sync (Event.receive t.recv)

  (* storage *)

  (** retrieve `num` transactions from the mempool *)
  let get_pending_transactions num =
    List.init num (fun _ -> Types.Transaction.for_test ())

  let store_and_sign_block t digest block =
    t.digest_to_block <- DigestMap.add digest block t.digest_to_block;
    Bls.SigningKey.sign t.signing_key digest

  (** store a received signature on our block *)
  let store_signature t public_key signature : bool =
    let received_signatures =
      Bls.PublicKeyMap.add public_key signature
        t.round_state.received_signatures
    in
    t.round_state <- { t.round_state with received_signatures };
    Bls.PublicKeyMap.cardinal t.round_state.received_signatures
    >= t.two_f_plus_one

  (** store a received certificate and potentially update to a new round *)
  let store_certificate t certificate =
    (* add the certificate to the state *)
    let received_certificates =
      Types.CertificateSet.add certificate t.round_state.received_certificates
    in
    t.round_state <- { t.round_state with received_certificates };

    (* check if we have enough certificate to go to the next round *)
    if
      Types.CertificateSet.cardinal t.round_state.received_certificates
      = t.two_f_plus_one
    then t.round_state <- { t.round_state with phase = ReadyForNewRound }

  (* message processing *)
  let validate_block t (sblock : Types.SignedBlock.t) : bool =
    let pubkey = sblock.block.source in
    let signature = sblock.signature in
    let digest = Types.SignedBlock.digest sblock in
    let block = sblock.block in
    (* valid signature *)
    if not (Bls.PublicKey.verify pubkey digest signature) then (
      print_endline "invalid sig";
      false (* from current round *))
    else if not (block.round = t.round_state.round) then (
      Format.printf "block: %d, us: %d\n" block.round t.round_state.round;
      print_endline "invalid round";
      false (* not enough certificates for non-genesis block *))
    else if
      block.round <> 0 && List.length block.certificates < t.two_f_plus_one
    then (
      print_endline "not enough certs";
      false (* author has already proposed in this round *))
    else if DigestMap.mem digest t.digest_to_block then (
      print_endline "author has already proposed";
      false)
    else true

  (** receive a block *)
  let process_block t ~from:public_key bytes =
    (* deserialize block *)
    let signed_block = Types.SignedBlock.of_bytes bytes in

    (* validate block *)
    if not (validate_block t signed_block) then failwith "invalid block"
    else
      (* store and sign the block *)
      let digest = Types.SignedBlock.digest signed_block in
      let signature = store_and_sign_block t digest signed_block in

      (* send signature back *)
      let serialized_signature = Bls.Signature.to_bytes signature in
      send t ~public_key ~label:"signature" serialized_signature

  (** create a certificate from 2f+1 signatures *)
  let create_and_broadcast_cert t =
    let signatures = t.round_state.received_signatures in
    assert (Bls.PublicKeyMap.cardinal signatures >= t.two_f_plus_one);

    (* get block digest *)
    let block_digest =
      match t.round_state.phase with
      | Proposed block_digest -> block_digest
      | WaitForCerts | ReadyForNewRound -> failwith "forbidden state transition"
    in

    (* create certificate & serialized *)
    let certificate = Types.{ block_digest; signatures } in
    let serialized_certificate = Types.Certificate.to_bytes certificate in

    (* update the state *)
    t.round_state <- { t.round_state with phase = WaitForCerts };

    (* store the certificate (and potentially move to new round) *)
    store_certificate t certificate;

    (* broadcast the certificate *)
    broadcast t ~label:"certificate" serialized_certificate

  (** receive a signature *)
  let process_signature t ~from:public_key bytes =
    (* get the current proposed block digest *)
    match t.round_state.phase with
    | WaitForCerts -> ()
    | ReadyForNewRound -> failwith "forbidden state transition"
    | Proposed digest ->
        (* deserialize signature *)
        let signature = Bls.Signature.of_bytes bytes in

        (* validate and store it *)
        if
          Bls.PublicKey.verify public_key digest signature
          && store_signature t public_key signature
        then
          (* create a certificate if we have enough signatures *)
          create_and_broadcast_cert t

  let process_certificate t bytes =
    (* deserialize it *)
    let certificate = Types.Certificate.of_bytes bytes in

    (* make sure there's 2f+1 signatures inside of it *)
    if Bls.PublicKeyMap.cardinal certificate.signatures < t.two_f_plus_one then
      ()
    else
      (* verify the signature *)
      let verify_signature public_key signature =
        (* check if the public key is part of the validator set *)
        Bls.PublicKeySet.mem public_key t.validators_pubkeys
        && (* check if the signature is correct *)
        Bls.PublicKey.verify public_key certificate.block_digest signature
      in
      if Bls.PublicKeyMap.for_all verify_signature certificate.signatures then
        (* TODO: ask for the block if we don't have it *)

        (* store and check if we can advance to new round *)
        store_certificate t certificate

  let process_msg t Types.{ from; label; data; _ } =
    match label with
    | "signed_block" -> process_block t ~from data
    | "signature" -> process_signature t ~from data
    | "certificate" -> process_certificate t data
    | _ -> failwith "unimplemented"

  (* main loops *)

  (** after starting a round, this is the loop *)
  let rec listen_for_messages t =
    (match t.round_state.phase with
    | ReadyForNewRound -> failwith "did not expected that"
    | WaitForCerts | Proposed _ -> ());

    (* receive & process the next message *)
    let msg = recv t in
    process_msg t msg;

    (* check if it's time to go to the next round *)
    match t.round_state.phase with
    | ReadyForNewRound -> start_round t
    | WaitForCerts | Proposed _ -> listen_for_messages t

  (** start a new round (relies on previous round state) *)
  and start_round ?genesis t =
    assert (t.round_state.phase = ReadyForNewRound);

    (* create a block building on the 2f+1 certificates
       although for genesis, there will be no certificate list
    *)
    let transactions = get_pending_transactions 10 in
    let round = t.round_state.round in
    let certificates =
      Types.CertificateSet.elements t.round_state.received_certificates
    in
    let sblock =
      Types.SignedBlock.create ~privkey:t.signing_key ~round ~certificates
        transactions
    in
    let serialized_sblock = Marshal.(to_bytes sblock [ No_sharing ]) in
    let block_digest = Types.SignedBlock.digest sblock in

    (* re-init the round state with new round (unless genesis) *)
    let round_state =
      match genesis with
      | Some true -> t.round_state
      | Some false | None ->
          let new_round = t.round_state.round + 1 in
          new_round_state new_round
    in

    (* init the round state for the new round *)
    t.round_state <- { round_state with phase = Types.Proposed block_digest };

    (* send it to everyone *)
    broadcast t ~label:"signed_block" serialized_sblock;

    (* wait to receive a certificate *)
    listen_for_messages t

  (** the main function *)
  let run_consensus t =
    (* log t "starting"; *)
    start_round ~genesis:true t
end

(** A validator is just a mempool for now. *)
module Validator = struct
  type t = { mempool : Mempool.t }

  let new_validator signing_key validators_pubkeys ~send ~recv =
    { mempool = Mempool.new_mempool signing_key validators_pubkeys ~send ~recv }

  let run t =
    (* run consensus *)
    Mempool.run_consensus t.mempool
end

module Mempool = struct
  type round_state = {
    received_certificates : Types.CertificateSet.t;
    received_signatures : Bls.SignatureSet.t;
    round : int;
    phase : Types.phase;
    already_proposed : Bls.PublicKeySet.t;
  }

  type t = {
    (* about *)
    signing_key : Bls.SigningKey.t;
    public_key : Bls.PublicKey.t;
    (* constants *)
    validator_pubkeys : Bls.PublicKey.t list;
    two_f_plus_one : int;
    (* communication *)
    send : Types.message Event.channel;
    recv : Types.message Event.channel;
    (* storage *)
    round_to_blocks : int -> Types.Block.t list;
    mutable pending_transactions : Types.Transaction.t list;
    (* round state *)
    mutable round_state : round_state;
  }

  (* new *)

  let new_mempool signing_key validator_pubkeys ~send ~recv : t =
    let two_f_plus_one = (2 * ((List.length validator_pubkeys - 1) / 3)) + 1 in
    let round_state =
      {
        received_certificates = Types.CertificateSet.empty;
        received_signatures = Bls.SignatureSet.empty;
        round = 0;
        phase = Types.ReadyForNewRound;
        already_proposed = Bls.PublicKeySet.empty;
      }
    in
    {
      signing_key;
      public_key = Bls.SigningKey.to_public signing_key;
      send;
      recv;
      round_to_blocks = (fun _ -> []);
      pending_transactions = [];
      validator_pubkeys;
      two_f_plus_one;
      round_state;
    }

  (*logging *)

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

  let write digest block =
    let _ = digest in
    let _ = block in
    ()

  let valid ~digest ~certificate : bool =
    let _ = digest in
    let _ = certificate in
    failwith "unimplemented"

  let read ~digest : Types.Block.t =
    let _ = digest in
    failwith "unimplemented"

  let get_pending_transactions num =
    List.init num (fun _ -> Types.Transaction.for_test ())

  (** store signature *)
  let store_signature t signature : bool =
    let received_signatures =
      Bls.SignatureSet.add signature t.round_state.received_signatures
    in
    t.round_state <- { t.round_state with received_signatures };
    Bls.SignatureSet.cardinal t.round_state.received_signatures
    = t.two_f_plus_one

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
  let validate_block (state : t) (sblock : Types.SignedBlock.t) : bool =
    let pubkey = sblock.block.source in
    let signature = sblock.signature in
    let digest = Types.SignedBlock.digest sblock in
    let block = sblock.block in
    (* valid signature *)
    if not (Bls.PublicKey.verify pubkey digest signature) then false
      (* from current round *)
    else if not (block.round = state.round_state.round) then false
      (* not enough certificates for non-genesis block *)
    else if
      block.round <> 0 && List.length block.certificates < state.two_f_plus_one
    then false (* author has already proposed in this round *)
    else if Bls.PublicKeySet.mem block.source state.round_state.already_proposed
    then false
    else true

  (** receive a block *)
  let process_block t ~from:public_key bytes =
    (* deserialize block *)
    let signed_block : Types.SignedBlock.t = Marshal.from_bytes bytes 0 in

    (* validate block *)
    if not (validate_block t signed_block) then failwith "invalid block"
    else
      (* store it *)
      let digest = Types.SignedBlock.digest signed_block in
      write digest signed_block;

      (* sign it *)
      let signature = Bls.SigningKey.sign t.signing_key digest in
      let signature = Bls.Signature.to_bytes signature in

      (* send signature back *)
      send t ~public_key ~label:"signature" signature

  (** create a certificate from 2f+1 signatures *)
  let create_and_broadcast_cert t =
    assert (
      Types.CertificateSet.cardinal t.round_state.received_certificates
      >= t.two_f_plus_one);

    (* get block digest *)
    let block_digest =
      match t.round_state.phase with
      | Proposed block_digest -> block_digest
      | WaitForCerts | ReadyForNewRound -> failwith "forbidden state transition"
    in

    (* create certificate & serialized *)
    let certificate =
      Types.{ block_digest; signatures = t.round_state.received_signatures }
    in
    let serialized_certificate = Types.Certificate.to_bytes certificate in

    (* update the state *)
    t.round_state <- { t.round_state with phase = WaitForCerts };

    (* store the certificate (and potentially move to new round) *)
    store_certificate t certificate;

    (* broadcast the certificate *)
    broadcast t ~label:"certificate" serialized_certificate

  (** receive a signature *)
  let process_signature t ~from bytes =
    (* get the current proposed block digest *)
    match t.round_state.phase with
    | WaitForCerts -> ()
    | ReadyForNewRound -> failwith "forbidden state transition"
    | Proposed digest ->
        (* deserialize signature *)
        let signature = Bls.Signature.of_bytes bytes in

        (* validate and store it *)
        if
          Bls.PublicKey.verify from digest signature
          && store_signature t signature
        then
          (* create a certificate if we have enough signatures *)
          create_and_broadcast_cert t

  let process_msg t Types.{ from; label; data; _ } =
    match label with
    | "signed_block" -> process_block t ~from data
    | "signature" -> process_signature t ~from data
    | "certificate" -> failwith "unimplemented"
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

  (** start a round with this function *)
  and start_round t =
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

    (* update the state *)
    let digest = Types.SignedBlock.digest sblock in
    t.round_state <- { t.round_state with phase = Types.Proposed digest };

    (* send it to everyone *)
    broadcast t ~label:"signed_block" serialized_sblock;

    (* wait to receive a certificate *)
    listen_for_messages t

  (** the main function *)
  let run_consensus t =
    (* log t "starting"; *)
    start_round t
end

(** A validator is just a mempool for now. *)
module Validator = struct
  type t = { mempool : Mempool.t }

  let new_validator signing_key validator_pubkeys ~send ~recv =
    { mempool = Mempool.new_mempool signing_key validator_pubkeys ~send ~recv }

  let run t =
    (* run consensus *)
    Mempool.run_consensus t.mempool
end

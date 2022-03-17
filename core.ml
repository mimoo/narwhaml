module Mempool = struct
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
    received_certificates : Types.CertificateSet.t;
    received_signatures : Types.SignatureSet.t;
    round : int;
    mutable phase : Types.phase;
    already_proposed : Types.PublicKeySet.t;
  }

  (* new *)

  let new_mempool signing_key validator_pubkeys ~send ~recv : t =
    let two_f_plus_one = List.length validator_pubkeys in
    {
      phase = Types.ReadyForNewRound;
      signing_key;
      public_key = Bls.SigningKey.to_public signing_key;
      send;
      recv;
      round = 0;
      round_to_blocks = (fun _ -> []);
      pending_transactions = [];
      received_certificates = Types.CertificateSet.empty;
      validator_pubkeys;
      two_f_plus_one;
      already_proposed = Types.PublicKeySet.empty;
      received_signatures = Types.SignatureSet.empty;
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

  (* message processing *)

  let validate_block (state : t) (sblock : Types.SignedBlock.t) : bool =
    let pubkey = sblock.block.source in
    let signature = sblock.signature in
    let digest = Types.SignedBlock.digest sblock in
    let block = sblock.block in
    (* valid signature *)
    if not (Bls.PublicKey.verify pubkey digest signature) then false
      (* from current round *)
    else if not (block.round = state.round) then false
      (* not enough certificates for non-genesis block *)
    else if
      block.round <> 0 && List.length block.certificates < state.two_f_plus_one
    then false (* author has already proposed in this round *)
    else if Types.PublicKeySet.mem block.source state.already_proposed then
      false
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

  (** store signature *)
  let store_signature _t _signature = ()

  (** receive a signature *)
  let process_signature t ~from bytes =
    (* deserialize signature *)
    let signature = Bls.Signature.of_bytes bytes in

    (* get the current proposed block digest *)
    let digest =
      match t.phase with
      | Proposed digest -> digest
      | ReadyForNewRound ->
          failwith "did not expected this in this serial logic"
    in

    (* validate it *)
    if Bls.PublicKey.verify from digest signature then
      store_signature t signature
    else ()

  let process_msg t Types.{ from; label; data; _ } =
    match label with
    | "signed_block" -> process_block t ~from data
    | "signature" -> process_signature t ~from data
    | _ -> failwith "unimplemented"

  (* main loops *)

  (** after starting a round, this is the loop *)
  let rec in_round t =
    let msg = recv t in
    process_msg t msg;

    match t.phase with
    | ReadyForNewRound -> failwith "oops"
    | Proposed _ -> in_round t

  (** start a round with this function *)
  let start_round t =
    (* create a block *)
    let transactions = get_pending_transactions 10 in
    let round = t.round in
    let certificates = Types.CertificateSet.elements t.received_certificates in
    let sblock =
      Types.SignedBlock.create ~privkey:t.signing_key ~round ~certificates
        transactions
    in
    let serialized_sblock = Marshal.(to_bytes sblock [ No_sharing ]) in

    (* update the state *)
    let digest = Types.SignedBlock.digest sblock in
    t.phase <- Types.Proposed digest;

    (* send it to everyone *)
    broadcast t ~label:"block" serialized_sblock;

    (* wait to receive a certificate *)
    in_round t

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

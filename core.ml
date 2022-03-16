module Mempool = struct
  type t = {
    mutex : Mutex.t;
    signing_key : Bls.SigningKey.t;
    public_key : Bls.PublicKey.t;
    send : Types.message Event.channel;
    recv : Types.message Event.channel;
    round : int;
    round_to_blocks : int -> Types.Block.t list;
    mutable pending_transactions : Types.Transaction.t list;
    validator_pubkeys : Bls.PublicKey.t list;
    two_f_plus_one : int;
    mutable round_to_authors : int -> Types.RoundToAuthors.t;
    mutable round_to_certificates : int -> Types.Certificate.t list;
  }

  module Network = struct
    let broadcast ~from ~label _bytes =
      let _ = from in
      let _ = label in
      ()

    let send ~from ~public_key ~label _bytes =
      let _ = from in
      let _ = public_key in
      let _ = label in
      ()
  end

  let new_mempool signing_key validator_pubkeys ~send ~recv : t =
    {
      mutex = Mutex.create ();
      signing_key;
      public_key = Bls.SigningKey.to_public signing_key;
      send;
      recv;
      round = 0;
      round_to_blocks = (fun _ -> []);
      pending_transactions = [];
      validator_pubkeys;
      two_f_plus_one = 0;
      round_to_authors = (fun _ -> Types.RoundToAuthors.empty);
      round_to_certificates = (fun _ -> []);
    }

  let get_current_certificates (state : t) round =
    state.round_to_certificates round

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
    else if
      Types.RoundToAuthors.mem block.source (state.round_to_authors block.round)
    then false
    else true

  let start_round t =
    (* create a block *)
    let transactions = get_pending_transactions 10 in
    let round = t.round in
    let certificates = get_current_certificates t round in
    let sblock =
      Types.SignedBlock.create ~privkey:t.signing_key ~round ~certificates
        transactions
    in
    let serialized_sblock = Marshal.(to_bytes sblock [ No_sharing ]) in

    (* send it to everyone *)
    Network.broadcast ~from:t.public_key ~label:"block" serialized_sblock;

    (* wait to receive a certificate *)
    ()

  let receive_block t ~from bytes =
    (* deserialize block *)
    let sblock : Types.SignedBlock.t = Marshal.from_bytes bytes 0 in

    (* validate block *)
    if not (validate_block t sblock) then failwith "invalid block"
    else
      (* store it *)
      let digest = Types.SignedBlock.digest sblock in
      write digest sblock;

      (* sign it *)
      let signature = Bls.SigningKey.sign t.signing_key digest in
      let signature = Bls.Signature.to_bytes signature in

      (* send signature back *)
      Network.send ~from ~public_key:from ~label:"signature" signature

  let receive_data t ~(from : Bls.PublicKey.t) ~(label : string) (data : bytes)
      =
    Format.printf "%s received a message" (Bls.PublicKey.to_hex t.public_key);
    match label with
    | "block" -> receive_block t ~from data
    | _ -> failwith "unimplemented"

  let run_consensus t =
    Format.printf "starting validator %s\n" (Bls.PublicKey.to_hex t.public_key);

    (* propose block *)
    start_round t;

    (* wait for messages and process them *)
    ()
end

module Validator = struct
  type t = { mempool : Mempool.t }

  let new_validator signing_key validator_pubkeys ~send ~recv =
    { mempool = Mempool.new_mempool signing_key validator_pubkeys ~send ~recv }

  let run t =
    (* send a msg for testing *)
    Format.printf "%s sending a test msg\n"
      (Bls.PublicKey.to_hex t.mempool.public_key);
    Event.sync
      (Event.send t.mempool.send
         Types.
           {
             from = t.mempool.public_key;
             destination = t.mempool.public_key Option;
             label = "test";
             data = Bytes.of_string "hey";
           });
    (* run consensus *)
    Mempool.run_consensus t.mempool
end

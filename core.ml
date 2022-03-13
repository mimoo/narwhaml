module rec Mempool : sig
  type t = {
    signing_key : Bls.SigningKey.t;
    public_key : Bls.PublicKey.t;
    round : int;
    round_to_blocks : int -> Types.Block.t list;
    pending_transactions : Types.Transaction.t list;
    validators : Bls.PublicKey.t list;
    two_f_plus_one : int;
    round_to_authors : int -> Types.RoundToAuthors.t;
    round_to_certificates : int -> Types.Certificate.t list;
  }

  module Network : sig
    val broadcast : from:Bls.PublicKey.t -> label:string -> bytes -> unit

    val send :
      from:Bls.PublicKey.t ->
      public_key:Bls.PublicKey.t ->
      label:string ->
      bytes ->
      unit
  end

  val new_mempool :
    signing_key:Bls.SigningKey.t -> validators:Bls.PublicKey.t list -> t

  val get_current_certificates : t -> int -> Types.Certificate.t list

  val write : int -> int -> unit

  val valid : digest:bytes -> certificate:Types.Certificate.t -> bool

  val read : digest:bytes -> Types.Block.t

  val get_pending_transactions : int -> Types.Transaction.t list

  val validate_block : t -> Types.SignedBlock.t -> bool

  val start_round : privkey:Bls.SigningKey.t -> t -> unit

  val receive_block : t -> from:Bls.PublicKey.t -> bytes -> unit

  val receive_data : t -> from:Bls.PublicKey.t -> label:string -> bytes -> unit
end = struct
  type t = {
    signing_key : Bls.SigningKey.t;
    public_key : Bls.PublicKey.t;
    round : int;
    round_to_blocks : int -> Types.Block.t list;
    pending_transactions : Types.Transaction.t list;
    validators : Bls.PublicKey.t list;
    two_f_plus_one : int;
    round_to_authors : int -> Types.RoundToAuthors.t;
    round_to_certificates : int -> Types.Certificate.t list;
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

  let new_mempool ~signing_key ~validators : t =
    {
      signing_key;
      public_key = Bls.SigningKey.to_public signing_key;
      round = 0;
      round_to_blocks = (fun _ -> []);
      pending_transactions = [];
      validators;
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

  let start_round ~privkey t =
    (* create a block *)
    let transactions = get_pending_transactions 10 in
    let round = t.round in
    let certificates = get_current_certificates t round in
    let sblock =
      Types.SignedBlock.create ~privkey ~round ~certificates transactions
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
    match label with
    | "block" -> receive_block t ~from data
    | _ -> failwith "unimplemented"
end

and Validator : sig
  type t = { mempool : Mempool.t }
end = struct
  type t = { mempool : Mempool.t }
end
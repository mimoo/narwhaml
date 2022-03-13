
module RoundToAuthors = Set.Make (Bls.PublicKey)

module Mempool = struct
  type t = {
    round : int;
    round_to_blocks : int -> Types.Block.t list;
    pending_transactions : Types.Transaction.t list;
    two_f_plus_one : int;
    round_to_authors : int -> RoundToAuthors.t;
    round_to_certificates : int -> Types.Certificate.t list;
  }

  let new_mempool _ : t =
    {
      round = 0;
      round_to_blocks = (fun _ -> []);
      pending_transactions = [];
      two_f_plus_one = 0;
      round_to_authors = (fun _ -> RoundToAuthors.empty);
      round_to_certificates = (fun _ -> []);
    }

  let get_current_certificates (state : t) round =
    state.round_to_certificates round

  let write ~digest ~block =
    let _ = digest in
    let _ = block in
    failwith "unimplemented"

  let valid ~digest ~certificate : bool =
    let _ = digest in
    let _ = certificate in
    failwith "unimplemented"

  let read ~digest : Types.Block.t =
    let _ = digest in
    failwith "unimplemented"

  let get_pending_transactions num =
    List.init num (fun _ -> Types.Transaction.for_test ())

  let validate_block (state : t) ~(sblock : Types.SignedBlock.t) : bool =
    let pubkey = sblock.block.source in
    let signature = sblock.signature in
    let digest = Types.Digest.to_bytes @@ Types.SignedBlock.digest sblock in
    let block = sblock.block in
    (* valid signature *)
    if not (Bls.PublicKey.verify pubkey digest signature) then false
      (* from current round *)
    else if not (block.round = state.round) then false
      (* not enough certificates for non-genesis block *)
    else if
      block.round <> 0 && List.length block.certificates < state.two_f_plus_one
    then false (* author has already proposed in this round *)
    else if RoundToAuthors.mem block.source (state.round_to_authors block.round)
    then false
    else true

  (** The consensus function *)
  let run_consensus ~privkey (state : t) =
    (* create a block *)
    let transactions = get_pending_transactions 10 in
    let round = state.round in
    let certificates = get_current_certificates state round in
    let block = Types.SignedBlock.create ~privkey ~round ~certificates transactions in
    let _ = block in
    failwith "unimplemented"
end

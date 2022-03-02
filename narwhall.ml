(** This is an educational implementation of the Narwhall protocol. *)

module ReliableBroadcast = struct
  let broadcast _msg = failwith "unimplemented"
end

module Transaction = struct
  type t = int

  let for_test _ : t = Random.int 10000
end

(** isn't it weird that there is no error here?
    shouldn't this require that t is hashable or something?
    
    worse, it thinks it's the same every time!!!
    *)
module Oracle (SomeModule : Hashtbl.HashedType) = struct
  let counter = ref 0

  module Hashtbl = Hashtbl.Make (SomeModule)

  let memory : int Hashtbl.t = Hashtbl.create 5

  let to_bytes (t : SomeModule.t) : string =
    Format.printf "using %d\n" (SomeModule.hash t);
    match Hashtbl.find_opt memory t with
    | Some d -> string_of_int d
    | None ->
        counter := !counter + 1;
        Hashtbl.add memory t !counter;
        string_of_int !counter
end

module Digest = struct
  type t = int

  let counter = ref 0

  let fake : (string, int) Hashtbl.t = Hashtbl.create 5

  let hash data =
    match Hashtbl.find_opt fake data with
    | Some d -> d
    | None ->
        counter := !counter + 1;
        Hashtbl.add fake data !counter;
        !counter

  let%test_unit "counter" =
    assert (hash "hey" = 1);
    assert (hash "hey" = 1);
    assert (hash "heyo" = 2)
end

module Ed25519 = struct
  module Signature = struct
    type t = int
  end

  module Pubkey = struct
    type t = int

    let verify ~(pubkey : t) ~(signature : Signature.t) (msg : Digest.t) : bool
        =
      let _ = pubkey in
      let _ = signature in
      let _ = msg in
      true

    let compare = Int.compare
  end

  module Privkey = struct
    type t = int

    let generate _ : t = Random.int 10000

    let to_pubkey t : Pubkey.t = t + 1

    let sign ~(privkey : t) (msg : Digest.t) : Signature.t =
      let _ = privkey in
      let _ = msg in
      failwith "unimplemented"
  end
end

module rec Block : sig
  type t = {
    source : Ed25519.Pubkey.t;
    round : int;
    transactions : Transaction.t list;
    certificates : Certificate.t list;
  }

  val to_bytes : t -> string

  val digest : t -> Digest.t
end = struct
  type t = {
    source : Ed25519.Pubkey.t;
    round : int;
    transactions : Transaction.t list;
    certificates : Certificate.t list;
  }

  let to_bytes block =
    let module B = Oracle (struct
      type nonrec t = t

      let hash = Hashtbl.hash

      let equal = Stdlib.( = )
    end) in
    B.to_bytes block

  let digest (block : Block.t) : Digest.t =
    let _ = block in
    failwith "digest"

  let%test_unit "to_bytes" =
    let privkey = Ed25519.Privkey.generate () in
    let source = Ed25519.Privkey.to_pubkey privkey in
    let round = 1 in
    let transactions = [ Transaction.for_test () ] in
    let certificates = [] in
    let block = { source; round; transactions; certificates } in
    let b1 = to_bytes block in
    let b2 = to_bytes block in
    assert (b1 = "1");
    assert (b1 = b2);
    let other_block = { block with round = 2 } in
    let b3 = to_bytes other_block in
    assert (b3 = "2")
end

and SignedBlock : sig
  type t = { block : Block.t; signature : Ed25519.Signature.t }

  val create :
    privkey:Ed25519.Privkey.t ->
    round:int ->
    certificates:Certificate.t list ->
    Transaction.t list ->
    t

  val digest : t -> Digest.t
end = struct
  type t = { block : Block.t; signature : Ed25519.Signature.t }

  let digest { block; _ } = Block.digest block

  let create ~privkey ~round ~certificates transactions =
    let source = Ed25519.Privkey.to_pubkey privkey in
    let block = Block.{ source; round; transactions; certificates } in
    let digest = Block.digest block in
    let signature = Ed25519.Privkey.sign ~privkey digest in
    { block; signature }
end

and Certificate : sig
  type t = { block : Block.t; signatures : Ed25519.Signature.t list }

  val create : Block.t -> t

  val add_signature : t -> Ed25519.Signature.t -> t
end = struct
  type t = { block : Block.t; signatures : Ed25519.Signature.t list }

  let create block : t = { block; signatures = [] }

  let add_signature t signature =
    { t with signatures = signature :: t.signatures }
end

module RoundToAuthors = Set.Make (Ed25519.Pubkey)

module Mempool = struct
  type t = {
    round : int;
    round_to_blocks : int -> Block.t list;
    pending_transactions : Transaction.t list;
    two_f_plus_one : int;
    round_to_authors : int -> RoundToAuthors.t;
    round_to_certificates : int -> Certificate.t list;
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

  let read ~digest : Block.t =
    let _ = digest in
    failwith "unimplemented"

  let get_pending_transactions num =
    List.init num (fun _ -> Transaction.for_test ())

  let validate_block (state : t) ~(sblock : SignedBlock.t) : bool =
    let pubkey = sblock.block.source in
    let signature = sblock.signature in
    let digest = SignedBlock.digest sblock in
    let block = sblock.block in
    (* valid signature *)
    if not (Ed25519.Pubkey.verify ~pubkey ~signature digest) then false
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
    let block = SignedBlock.create ~privkey ~round ~certificates transactions in
    let _ = block in
    failwith "unimplemented"
end

module Validator = struct
  type t = { privkey : Ed25519.Privkey.t; mempool : Mempool.t }

  let new_validator _ =
    { privkey = Ed25519.Privkey.generate (); mempool = Mempool.new_mempool () }
end

module Network = struct
  let test_network ~num_validators =
    let _validators =
      List.init num_validators (fun _ -> Validator.new_validator ())
    in
    ()
end

let%test_unit "thing" = print_endline "hello world"

module DigestToBlock = Map.Make (Bytes)

module Storage = struct
  type t = { mutex : Mutex.t; mutable blocks : Types.Block.t DigestToBlock.t }

  let store t block =
    let digest = Types.Block.digest block in
    Mutex.lock t.mutex;
    t.blocks <- DigestToBlock.add digest block t.blocks;
    Mutex.unlock t.mutex
end

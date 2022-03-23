type t = { mutex : Mutex.t }

let t = { mutex = Mutex.create () }

let log msg =
  Mutex.lock t.mutex;
  Format.printf "%s\n" msg;
  Mutex.unlock t.mutex

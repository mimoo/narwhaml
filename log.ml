type t = { mutex : Mutex.t }

let t = { mutex = Mutex.create () }

let log msg =
  Mutex.lock t.mutex;
  Format.printf "%s" msg;
  print_newline ();
  Mutex.unlock t.mutex

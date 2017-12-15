open Lwt
open OUnit2

let test_lwt name f =
  let test test_ctxt =
    Lwt_main.run (f ())
  in
  name >:: test

let send_msgs socket =
  Lwt_list.iter_s (Nanomsg.send socket)

let expect_msgs socket =
  let expect msg =
    Nanomsg.recv socket
    >>= fun actual_msg ->
    Printf.printf "XOXOX received = %s\n%!" actual_msg;
    assert (actual_msg = msg);
    return ()
  in
  Lwt_list.iter_s expect

let exchange producer consumer messages = Lwt.join [
    send_msgs producer messages;
    expect_msgs consumer messages;
  ]

let test_push_pull channel messages =
  let name = "push/pull over "^channel in
  let test () =
    let producer = Nanomsg.socket Nanomsg.Push in
    let consumer = Nanomsg.socket Nanomsg.Pull in
    let _ = Nanomsg.bind producer channel in
    let _ = Nanomsg.connect consumer channel in
    exchange producer consumer messages
  in
  test_lwt name test

let message_samples = ["foo"; "bar"; "xoxox"]

let suite = "tests">:::[
  test_push_pull "ipc:///tmp/foo" message_samples;
  test_push_pull "inproc://foo" message_samples;
  test_push_pull "tcp://127.0.0.1:5555" message_samples;
]

let main () =
  run_test_tt_main suite

let () =
  Lwt_engine.set (new Lwt_engine.libev ());
  Lwt_unix.with_async_detach main

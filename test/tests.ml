open Lwt
open OUnit2

module Msg = Nanomsg.Payload

let test_apply name f x y =
  let test test_ctxt =
    assert_equal y (f x)
  in
  name >:: test

let test_inverse name f x g =
  let test test_ctxt =
    assert_equal x (g (f x))
  in
  name >:: test

let test_lwt name f =
  let test test_ctxt =
    Lwt_main.run (f ())
  in
  name >:: test

let test_lwt_fail name exn f =
  let test test_ctxt =
    assert_raises exn (fun () ->
      Lwt_main.run (f ())
    )
  in
  name >:: test

let send_msgs socket =
  Lwt_list.iter_s (fun msg -> Nanomsg.send socket (Msg.of_string msg))

let expect_msgs socket =
  let expect msg =
    Nanomsg.recv socket
    >>= fun actual_msg ->
    assert_equal (Msg.to_string actual_msg) msg;
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
(*
    >>= fun () ->
    Nanomsg.close producer
    >>= fun () ->
    Nanomsg.close consumer
*)
  in
  test_lwt name test

let test_send_over_recv_only =
  let name = "send over PULL" in
  let exn = Unix.Unix_error (Unix.EINVAL, "Nanomsg.send", "receive-only socket") in
  let test () =
    let socket = Nanomsg.socket Nanomsg.Pull in
    Nanomsg.send socket (Msg.of_string "msg")
  in
  test_lwt_fail name exn test

let test_recv_over_send_only =
  let name = "recv from PUSH" in
  let exn = Unix.Unix_error (Unix.EINVAL, "Nanomsg.recv", "send-only socket") in
  let test () =
    let socket = Nanomsg.socket Nanomsg.Push in
    Nanomsg.recv socket
  in
  test_lwt_fail name exn test

let test_garbage_collection =
  let name = "garbage collection" in
  let test () =
    let channel = "ipc:///tmp/pair-pair" in
    let message = Bytes.create 1000 in
    let producer = Nanomsg.socket Nanomsg.Pair in
    let consumer = Nanomsg.socket Nanomsg.Pair in
    let _ = Nanomsg.bind producer channel in
    let _ = Nanomsg.connect consumer channel in
    let rec loop n =
      if n = 0 then Lwt.return_unit
      else if n mod 10 = 0 then (
        Gc.compact ();
        loop (n-1)
      ) else (
        Nanomsg.send producer (Msg.of_string message)
        >>= fun () ->
        Nanomsg.recv consumer
        >>= fun payload ->
        assert_equal message (Msg.to_string payload);
        loop (n-1)
      )
    in
    loop 1000
  in
  test_lwt name test

let test_bad_protocol =
  let name = "connecting PUSH PAIR" in
  let exn = Unix.Unix_error (Unix.EINVAL, "Nanomsg.recv", "send-only socket") in
  let test () =
    let channel = "inproc://push-pair" in
    let push = Nanomsg.socket Nanomsg.Push in
    let pair = Nanomsg.socket Nanomsg.Pair in
    let _ = Nanomsg.bind push channel in
    let _ = Nanomsg.connect pair channel in
    Nanomsg.send push (Msg.of_string "msg")
    >>= fun () ->
    Nanomsg.recv pair
  in
  test_lwt_fail name exn test

let message_samples = ["foo"; "bar"; "xoxox"]

let suite = "tests">:::[
  test_push_pull "ipc:///tmp/foo" message_samples;
  test_push_pull "inproc://foo" message_samples;
  test_push_pull "tcp://127.0.0.1:5555" message_samples;
  test_send_over_recv_only;
  test_recv_over_send_only;
  test_garbage_collection;
  (* test_bad_protocol; *)
]

let main () =
  run_test_tt_main suite

let () =
  Lwt_engine.set (new Lwt_engine.libev ());
  Lwt_unix.with_async_detach main

open Lwt
open OUnit2

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
  Lwt_list.iter_s (Nanomsg.send socket)

let expect_msgs socket =
  let expect msg =
    Nanomsg.recv socket
    >>= fun actual_msg ->
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
    >>= fun () ->
    Nanomsg.close producer
    >>= fun () ->
    Nanomsg.close consumer
  in
  test_lwt name test

let test_send_over_recv_only =
  let name = "send over PULL" in
  let exn = Unix.Unix_error (Unix.EINVAL, "Nanomsg.send", "receive-only socket") in
  let test () =
    let socket = Nanomsg.socket Nanomsg.Pull in
    Nanomsg.send socket "msg"
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

let test_bad_protocol =
  let name = "connecting PUSH PAIR" in
  let exn = Unix.Unix_error (Unix.EINVAL, "Nanomsg.recv", "send-only socket") in
  let test () =
    let channel = "inproc://push-pair" in
    let push = Nanomsg.socket Nanomsg.Push in
    let pair = Nanomsg.socket Nanomsg.Pair in
    let _ = Nanomsg.bind push channel in
    let _ = Nanomsg.connect pair channel in
    Nanomsg.send push "msg"
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
  (* test_bad_protocol; *)
]

let main () =
  run_test_tt_main suite

let () =
  Lwt_engine.set (new Lwt_engine.libev ());
  Lwt_unix.with_async_detach main

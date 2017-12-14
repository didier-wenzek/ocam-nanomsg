open Lwt

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
    
let () =
  let producer = Nanomsg.socket Nanomsg.Push in
  let consumer = Nanomsg.socket Nanomsg.Pull in

(*
  let _ = Nanomsg.bind producer "inproc://foo" in
  let _ = Nanomsg.connect consumer "inproc://foo" in
*)
(*
  let _ = Nanomsg.bind producer "ipc:///tmp/foo" in
  let _ = Nanomsg.connect consumer "ipc:///tmp/foo" in
*)
(*
  let _ = Nanomsg.connect producer "tcp://127.0.0.1:5560" in
  let _ = Nanomsg.bind consumer "tcp://127.0.0.1:5560" in
*)
  let _ = Nanomsg.bind producer "ipc:///tmp/foo" in
  let _ = Nanomsg.connect consumer "ipc:///tmp/foo" in

  let msg_list = ["foo"; "bar"; "xoxox"] in

  Lwt.join [
    send_msgs producer msg_list;
    expect_msgs consumer msg_list;
  ] |> Lwt_main.run

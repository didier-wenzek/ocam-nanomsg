open Lwt

(* A socket is wrapped with its RECVFD and SNDFD file descriptors.

  RECVFD is readable when a message can be received from the socket.
  SNDFD is writable when a message can be sent to the socket.

  These file descriptors are used by send/recv functions,
  which use [Lwt_unix.wrap_syscall]
  to wait until there is actually something to write/read.
*)
type socket_id = int
type socket = Socket of socket_id * Lwt_unix.file_descr * Lwt_unix.file_descr
type address = string
type endpoint_id = int
type endpoint = Endpoint of socket_id * endpoint_id

type domain = AF_SP | AF_SP_RAW
type protocol = Pair | Pub | Sub | Req | Rep | Push | Pull | Surveyor | Respondent | Bus
type nn_sockopt =
  | NN_LINGER
  | NN_SNDBUF | NN_RCVBUF
  | NN_RECONNECT_IVL | NN_RECONNECT_IVL_MAX
  | NN_SNDPRIO | NN_RCVPRIO
  | NN_IPV4ONLY
  | NN_SNDFD | NN_RCVFD
  | NN_TCP_NODELAY 
  | NN_REQ_RESEND_IVL
  | NN_SUB_SUBSCRIBE | NN_SUB_UNSUBSCRIBE
  | NN_SURVEYOR_DEADLINE
type nn_level =
  | NN_SOL_SOCKET
  | NN_TCP
  | NN_REQ | NN_SUB | NN_SURVEYOR

type _ sockopt =
  | IntOpt : nn_level * nn_sockopt -> int sockopt
  | BoolOpt : nn_level * nn_sockopt -> bool sockopt

let linger = IntOpt (NN_SOL_SOCKET, NN_LINGER)
let sndbuf = IntOpt (NN_SOL_SOCKET, NN_SNDBUF)
let rcvbuf = IntOpt (NN_SOL_SOCKET, NN_RCVBUF)
let reconnect_ivl = IntOpt (NN_SOL_SOCKET, NN_RECONNECT_IVL)
let reconnect_ivl_max = IntOpt (NN_SOL_SOCKET, NN_RECONNECT_IVL_MAX)
let sndprio = IntOpt (NN_SOL_SOCKET, NN_SNDPRIO)
let rcvprio = IntOpt (NN_SOL_SOCKET, NN_RCVPRIO)
let ipv4only = BoolOpt (NN_SOL_SOCKET, NN_IPV4ONLY)
let tcp_nodelay = BoolOpt (NN_TCP, NN_TCP_NODELAY)
let req_resend_ivl = IntOpt (NN_REQ, NN_REQ_RESEND_IVL)
let surveyor_deadline = IntOpt (NN_SURVEYOR, NN_SURVEYOR_DEADLINE)

external nn_getsockopt_int : socket_id -> nn_level -> nn_sockopt -> int = "ocaml_nanomsg_getsockopt_int"
external nn_getsockopt_fd : socket_id -> nn_level -> nn_sockopt -> Unix.file_descr = "ocaml_nanomsg_getsockopt_int"
external nn_setsockopt_int : socket_id -> nn_level -> nn_sockopt -> int -> unit = "ocaml_nanomsg_setsockopt_int"
external nn_setsockopt_str : socket_id -> nn_level -> nn_sockopt -> string -> unit = "ocaml_nanomsg_setsockopt_str"

(* Wraps the RECVFD or SNDFD file descriptor of socket into a Lwt_unix.file_descr

   In case this file descriptor doesn't exist
   (because the socket is either not readable or not writable),
   a dummy file descriptor is used
   (this is harmless because it will not be used).
*)
let get_socket_fd socket_id io_event =
  let sockopt, default_fd = match io_event with
    | Lwt_unix.Read -> (NN_RCVFD, Lwt_unix.stdin)
    | Lwt_unix.Write -> (NN_SNDFD, Lwt_unix.stdout)
  in try
    let unix_fd = nn_getsockopt_fd socket_id NN_SOL_SOCKET sockopt in
    Lwt_unix.of_unix_file_descr ~blocking:false unix_fd
  with Unix.Unix_error (Unix.ENOPROTOOPT,_,_) -> default_fd

external nn_socket : domain -> protocol -> socket_id = "ocaml_nanomsg_socket"
let socket ?(domain = AF_SP) protocol =
  let socket_id = nn_socket domain protocol in
  let recv_fd = get_socket_fd socket_id Lwt_unix.Read in
  let send_fd = get_socket_fd socket_id Lwt_unix.Write in
  Socket(socket_id, recv_fd, send_fd)

external nn_bind : socket_id -> address -> endpoint_id = "ocaml_nanomsg_bind"
let bind (Socket(socket,_,_)) address = Endpoint (socket, nn_bind socket address)

external nn_connect : socket_id -> address -> endpoint_id = "ocaml_nanomsg_connect"
let connect (Socket(socket,_,_)) address = Endpoint (socket, nn_connect socket address)

external nn_shutdown : socket_id -> endpoint_id -> unit = "ocaml_nanomsg_shutdown"
let shutdown (Endpoint(socket,id)) = nn_shutdown socket id

let getsockopt : type a. socket -> a sockopt -> a = fun (Socket(socket,_,_)) -> function
  | IntOpt (level, sockopt) -> nn_getsockopt_int socket level sockopt
  | BoolOpt (level, sockopt) -> let value = nn_getsockopt_int socket level sockopt in value = 1

let setsockopt : type a. socket -> a sockopt -> a -> unit = fun (Socket(socket,_,_)) sockopt value -> match sockopt with
  | IntOpt (level, int_sockopt) -> nn_setsockopt_int socket level int_sockopt value
  | BoolOpt (level, int_sockopt) -> nn_setsockopt_int socket level int_sockopt (if value then 1 else 0)

external nn_send : socket_id -> string -> unit = "ocaml_nanomsg_send"
let send (Socket(socket, _, send_fd)) msg = 
  let action () =
    try nn_send socket msg;                 None
    with
    | Unix.Unix_error(Unix.EAGAIN, _, _) -> raise Lwt_unix.Retry_write
    | error                              -> Some error
  in
  Lwt_unix.wrap_syscall Lwt_unix.Write send_fd action
  >>= function
  | None -> return_unit
  | Some error -> fail error

external nn_recv : socket_id -> string = "ocaml_nanomsg_recv"
let recv (Socket(socket, recv_fd, _)) =
  let action () =
    try (None, nn_recv socket)
    with
    | Unix.Unix_error(Unix.EAGAIN, _, _) -> raise Lwt_unix.Retry_read
    | error                              -> (Some error, "")
  in
  Lwt_unix.wrap_syscall Lwt_unix.Read recv_fd action
  >>= function
  | (None, msg) -> return msg
  | (Some error, _) -> fail error

external nn_close_job : socket_id -> unit Lwt_unix.job = "ocaml_nanomsg_close_job"
let close (Socket(socket,_,_)) = Lwt_unix.run_job (nn_close_job socket)

external term : unit -> unit = "ocaml_nanomsg_term"

let subscribe (Socket(socket,_,_)) prefix = nn_setsockopt_str socket NN_SUB NN_SUB_SUBSCRIBE prefix
let unsubscribe (Socket(socket,_,_)) prefix = nn_setsockopt_str socket NN_SUB NN_SUB_UNSUBSCRIBE prefix

(** The type of a socket *)
type socket

(** The type of a socket domain *)
type domain = AF_SP | AF_SP_RAW

(** The type of protocols which define the behavior of a socket *)
type protocol = Pair | Pub | Sub | Req | Rep | Push | Pull | Surveyor | Respondent | Bus

type address = string
type endpoint

(** A value of type ['a sockopt] names an option with value of type ['a]. *)
type 'a sockopt

(** Creates a socket with specified domain and protocol

    The default domain is AF_SP.
*)
val socket : ?domain:domain -> protocol -> socket

(** Adds a local endpoint to the socket. *)
val bind : socket -> address -> endpoint

(** Adds a remote endpoint to the socket. *)
val connect : socket -> address -> endpoint

(** Options.

  No timeout option is provided:
  all send/recv operations are performed in non-blocking mode and use the NN_DONTWAIT flag.

  No send/recv fd is provided:
  there are used internally to signal send/recv event to Lwt.
*)
val linger : int sockopt
val sndbuf : int sockopt
val rcvbuf : int sockopt
val reconnect_ivl : int sockopt
val reconnect_ivl_max : int sockopt
val sndprio : int sockopt
val rcvprio : int sockopt
val ipv4only : bool sockopt
val tcp_nodelay : bool sockopt
val req_resend_ivl : int sockopt
val surveyor_deadline : int sockopt

val getsockopt : socket -> 'a sockopt -> 'a
val setsockopt : socket -> 'a sockopt -> 'a -> unit

(** Sends a message to the socket.

   Which of the peers the message will be sent to is determined by the particular socket type.

   (!) Warning,
   the size of the send buffer (sndbuf option) must be large enough
   to contain the message otherwise the operation blocks !
*)
val send : socket -> string -> unit Lwt.t

(** Receives a message from the socket. *)
val recv : socket -> string Lwt.t

(** Removes an endpoint from a socket.

  [shutdown ()] returns immediately, however,
  the library will try to deliver any outstanding outbound messages to the endpoint
  for the time specified by NN_LINGER socket option.
*)
val shutdown : endpoint -> unit

(** Closes the socket.
 
  Any buffered inbound messages that were not yet received by the application will be discarded.
  The library will try to deliver any outstanding outbound messages for the time specified by NN_LINGER socket option.
*)
val close : socket -> unit Lwt.t

(** Informs all the open sockets that process termination is underway. *)
val term : unit -> unit

(** Subcribes the socket to all messages published with the given prefix. *)
val subscribe : socket -> string -> unit

(** Unsubcribes the socket to messages published with the given prefix. *)
val unsubscribe : socket -> string -> unit

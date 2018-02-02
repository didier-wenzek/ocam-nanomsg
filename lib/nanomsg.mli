(** The type of a socket *)
type socket

(** The type of a socket domain *)
type domain = AF_SP | AF_SP_RAW

(** The type of protocols which define the behavior of a socket *)
type protocol = Pair | Pub | Sub | Req | Rep | Push | Pull | Surveyor | Respondent | Bus

(** The type of a socket address: a string made of two parts ["transport://address"] *)
type address = string

(** The type of a binding of a socket with some local or remote addresse. *)
type endpoint

(** A value of type ['a sockopt] names an option with value of type ['a]. *)
type 'a sockopt

module Payload : sig
  (** Message to be sent. *)
  type send

  (** Received message. *)
  type recv

  (** Buffer type based on Bigarray *)
  type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t 

  (** Make a message of a value. *)
  val of_string: string -> send

  (** Extract the content of a received message. *)
  val to_string: recv -> string

  (** Encode a value in a message to be sent. *)
  val of_value: sizer:('a -> int) -> writer:(buffer -> 'a -> unit) -> 'a -> send

  (** Decode a value of a received message. *)
  val to_value: reader:(buffer -> 'a) -> recv -> 'a
end

(** Creates a socket with a specified domain and protocol

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
  the socket file descriptor is used internally to signal send/recv events to Lwt.
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
val send : socket -> Payload.send -> unit Lwt.t

(** Receives a message from the socket. *)
val recv : socket -> Payload.recv Lwt.t

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

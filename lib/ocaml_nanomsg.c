#include <string.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <lwt_unix.h>
#include <nanomsg/nn.h>
#include <nanomsg/pair.h>
#include <nanomsg/pubsub.h>
#include <nanomsg/reqrep.h>
#include <nanomsg/pipeline.h>
#include <nanomsg/survey.h>
#include <nanomsg/bus.h>
#include <nanomsg/tcp.h>

/* Must be synchronized with Nanomsg.domain. */
static int const DOMAIN[] = {
  AF_SP,
  AF_SP_RAW
};

/* Must be synchronized with Nanomsg.protocol. */
static int const PROTOCOL[] = {
  NN_PAIR,
  NN_PUB,
  NN_SUB,
  NN_REQ,
  NN_REP,
  NN_PUSH,
  NN_PULL,
  NN_SURVEYOR,
  NN_RESPONDENT,
  NN_BUS
};

/* Must be synchronized with Nanomsg.nn_sockopt. */
static int const SOCKOPT[] = {
  NN_LINGER,
  NN_SNDBUF,
  NN_RCVBUF,
  NN_RECONNECT_IVL,
  NN_RECONNECT_IVL_MAX,
  NN_SNDPRIO,
  NN_RCVPRIO,
  NN_IPV4ONLY,
  NN_SNDFD,
  NN_RCVFD,
  NN_TCP_NODELAY,
  NN_REQ_RESEND_IVL,
  NN_SUB_SUBSCRIBE,
  NN_SUB_UNSUBSCRIBE,
  NN_SURVEYOR_DEADLINE
};

/* Must be synchronized with Nanomsg.nn_level. */
static int const LEVEL[] = {
  NN_SOL_SOCKET,
  NN_TCP,
  NN_REQ,
  NN_SUB,
  NN_SURVEYOR
};

extern CAMLprim
value ocaml_nanomsg_socket(value caml_domain, value caml_protocol)
{
  CAMLparam2(caml_domain, caml_protocol);
  CAMLlocal1(caml_socket);

  int domain = DOMAIN[Int_val(caml_domain)];
  int protocol = PROTOCOL[Int_val(caml_protocol)];
  int socket = nn_socket(domain, protocol);

  if (socket == -1) {
    unix_error(errno, "Nanomsg.socket", Nothing);
  } else {
    caml_socket = Val_int(socket);
    CAMLreturn(caml_socket);
  }
}

extern CAMLprim
value ocaml_nanomsg_getsockopt_int(value caml_socket, value caml_level, value caml_sockopt)
{
  CAMLparam3(caml_socket, caml_level, caml_sockopt);
  CAMLlocal1(caml_option);

  int socket = Int_val(caml_socket);
  int level = LEVEL[Int_val(caml_level)];
  int sockopt = SOCKOPT[Int_val(caml_sockopt)];
  int option;
  size_t size = sizeof(option);
  int error = nn_getsockopt(socket, level, sockopt, &option, &size);

  if (error) {
    unix_error(errno, "Nanomsg.getsockopt", Nothing);
  } else {
    caml_option = Val_int(option);
    CAMLreturn(caml_option);
  }
}

extern CAMLprim
value ocaml_nanomsg_setsockopt_int(value caml_socket, value caml_level, value caml_sockopt, value caml_option)
{
  CAMLparam4(caml_socket, caml_level, caml_sockopt, caml_option);

  int socket = Int_val(caml_socket);
  int level = LEVEL[Int_val(caml_level)];
  int sockopt = SOCKOPT[Int_val(caml_sockopt)];
  int option = Int_val(caml_option);
  size_t size = sizeof(option);
  int error = nn_setsockopt(socket, level, sockopt, &option, size);

  if (error) {
    unix_error(errno, "Nanomsg.setsockopt", Nothing);
  } else {
    CAMLreturn(Val_unit);
  }
}

extern CAMLprim
value ocaml_nanomsg_setsockopt_str(value caml_socket, value caml_level, value caml_sockopt, value caml_option)
{
  CAMLparam4(caml_socket, caml_level, caml_sockopt, caml_option);

  int socket = Int_val(caml_socket);
  int level = LEVEL[Int_val(caml_level)];
  int sockopt = SOCKOPT[Int_val(caml_sockopt)];
  const char* option = String_val(caml_option);
  size_t size = caml_string_length(caml_option);
  int error = nn_setsockopt(socket, level, sockopt, option, size);

  if (error) {
    unix_error(errno, "Nanomsg.setsockopt", Nothing);
  } else {
    CAMLreturn(Val_unit);
  }
}

extern CAMLprim
value ocaml_nanomsg_bind(value caml_socket, value caml_address)
{
  CAMLparam2(caml_socket, caml_address);
  CAMLlocal1(caml_endpoint);

  int socket = Int_val(caml_socket);
  const char *address = String_val(caml_address);
  int endpoint = nn_bind(socket, address);

  if (endpoint == -1) {
    unix_error(errno, "Nanomsg.bind", Nothing);
  } else {
    caml_endpoint = Val_int(endpoint);
    CAMLreturn(caml_endpoint);
  }
}

extern CAMLprim
value ocaml_nanomsg_connect(value caml_socket, value caml_address)
{
  CAMLparam2(caml_socket, caml_address);
  CAMLlocal1(caml_endpoint);

  int socket = Int_val(caml_socket);
  const char *address = String_val(caml_address);
  int endpoint = nn_connect(socket, address);

  if (endpoint == -1) {
    unix_error(errno, "Nanomsg.connect", Nothing);
  } else {
    caml_endpoint = Val_int(endpoint);
    CAMLreturn(caml_endpoint);
  }
}

extern CAMLprim
value ocaml_nanomsg_shutdown(value caml_socket, value caml_endpoint)
{
  CAMLparam2(caml_socket, caml_endpoint);

  int socket = Int_val(caml_socket);
  int endpoint = Int_val(caml_endpoint);
  int error = nn_shutdown(socket, endpoint);

  if (error) {
    unix_error(errno, "Nanomsg.shutdown", Nothing);
  } else {
    CAMLreturn(Val_unit);
  }
}

extern CAMLprim
value ocaml_nanomsg_term(value caml_unit)
{
  CAMLparam1(caml_unit);

  nn_term();

  CAMLreturn(Val_unit);
}

struct job_close {
  struct lwt_unix_job job;
  
  int socket;
  int error;
};

#include <stdio.h>

static void worker_close(struct job_close* job)
{
  if (nn_close(job->socket)) job->error = errno;
  else                       job->error = 0;
}

static value result_close(struct job_close* job)
{
  CAMLparam0();

  int error = job->error;
  lwt_unix_free_job(&job->job);

  if (error) {
    unix_error(error, "Nanomsg.close", Nothing);
  } else {
    CAMLreturn(Val_unit);
  }
}

extern CAMLprim
value ocaml_nanomsg_close_job(value caml_socket)
{
  LWT_UNIX_INIT_JOB(job, close, 0);

  job->socket = Int_val(caml_socket);

  return lwt_unix_alloc_job(&job->job);
}

extern CAMLprim
value ocaml_nanomsg_send(value caml_socket, value caml_msg)
{
  CAMLparam2(caml_socket, caml_msg);

  int socket = Int_val(caml_socket);
  void* buf = String_val(caml_msg);
  size_t len = caml_string_length(caml_msg);

  int sent = nn_send(socket, buf, len, NN_DONTWAIT);
  
  if (sent == -1) {
    unix_error(errno, "Nanomsg.send", Nothing);
  } else {
    CAMLreturn(Val_unit);
  }
}

extern CAMLprim
value ocaml_nanomsg_recv(value caml_socket)
{
  CAMLparam1(caml_socket);
  CAMLlocal1(caml_msg);

  int socket = Int_val(caml_socket);
  void *buf;
  int len = nn_recv(socket, &buf, NN_MSG, NN_DONTWAIT);

  if (len == -1) {
    unix_error(errno, "Nanomsg.recv", Nothing);
  } else {
    caml_msg = caml_alloc_string(len);
    memcpy(String_val(caml_msg), buf, len);

    nn_freemsg(buf);
    CAMLreturn(caml_msg);
  }
}


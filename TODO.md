* [ ]  Fix memory issues with received payload.
    * [X] Add a `recv_string` helper function
    * [X] Add a `send_string` helper function
    * [X] Add a `recv_value` helper function of type `reader:(bigstring -> 'a) -> socket -> 'a Lwt.t`
    * [X] Add a `send_value` helper function of type `sizer:('a -> int) -> writer:(bigstring -> 'a -> unit) -> socket -> 'a -> unit Lwt.t`
    * [X] Wrap the message payloads into a custom block with a finalizer
    * [X] Add converter function `payload -> string`
    * [X] Add converter function `string -> payload`
    * [X] Add converter function `sizer:('a -> int) -> writer:(bigstring -> 'a -> unit) -> 'a -> payload`.
    * [X] Add converter function `reader:(bigstring -> 'a) -> payload -> 'a`.
* [ ] Manage timeout
    * [ ] Add timeout options
    * [ ] Add a blocking version of recv (returning None if nothing before timeout).
    * [ ] Add a non blocking version of recv (returning None if currently nothing).

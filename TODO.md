* [ ] Fix memory issues with received payload.
      * [ ] Implement a recv_string function
      * [ ] Wrap the message payloads into a custom block with a finalizer
      * [ ] Add converter function `(payload -> string)`
      * [ ] Add converter function `(payload -> bigstring)` (! with warning about memory).
      * [ ] Add converter function `(payload -> (bigstring -> 'a) -> 'a)`
      * [ ] Add a create function `size -> payload`
      * [ ] Add a create function `string -> payload`
      * [ ] Add a write function `(bigstring -> int -> 'a -> int) -> payload -> int -> 'a -> int
      * [ ] Add a write function `(bigstring -> 'a -> unit) -> payload -> 'a -> unit
      * [ ] Add a create function `('a -> int) -> (bigstring -> int -> 'a -> int) -> 'a -> payload
* [ ] Add timeout options
* [ ] Add a blocking version of recv (returning None if nothing before timeout).
* [ ] Add a non blocking version of recv (returning None if currently nothing).

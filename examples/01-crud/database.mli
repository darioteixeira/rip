(********************************************************************************)
(*  Database.mli
    Copyright (c) 2016 Dario Teixeira <dario.teixeira@nleyten.com>
*)
(********************************************************************************)

type t


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val init: unit -> t
val get_one: t -> int -> string option
val get_all: t -> (int * string) list
val add: t -> string -> int
val update: t -> int -> string -> bool
val delete_one: t -> int -> bool
val delete_all: t -> unit


(********************************************************************************)
(*  Database.mli
    Copyright (c) 2016 Dario Teixeira <dario.teixeira@nleyten.com>
*)
(********************************************************************************)

(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type t


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val init: unit -> t Lwt.t
val get_one: t -> int -> string option Lwt.t
val get_all: t -> (int * string) list Lwt.t
val add: t -> string -> int Lwt.t
val update: t -> int -> string -> bool Lwt.t
val delete_one: t -> int -> bool Lwt.t
val delete_all: t -> unit Lwt.t


(********************************************************************************)
(*  Rip_server.mli
    Copyright (c) 2016 Dario Teixeira <dario.teixeira@nleyten.com>
*)
(********************************************************************************)


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val run: unit Lwt.t -> int -> unit Lwt.t


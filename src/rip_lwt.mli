(********************************************************************************)
(*  Rip_lwt.mli
    Copyright (c) 2016 Dario Teixeira <dario.teixeira@nleyten.com>
*)
(********************************************************************************)

include Rip.S with
    type 'a io = 'a Lwt.t and
    type body = Cohttp_lwt.Body.t and
    type conn = Cohttp_lwt_unix.Server.IO.conn * Cohttp.Connection.t


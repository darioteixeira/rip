(********************************************************************************)
(*  Rip_lwt.ml
    Copyright (c) 2016 Dario Teixeira <dario.teixeira@nleyten.com>
*)
(********************************************************************************)

include Rip.Make
(struct
    type 'a io = 'a Lwt.t
    type body = Cohttp_lwt.Body.t
    type conn = Cohttp_lwt_unix.Server.IO.conn * Cohttp.Connection.t

    let return = Lwt.return
    let (>>=) = Lwt.(>>=)

    let body_of_string_option = function
        | None   -> Lwt.return `Empty
        | Some x -> Lwt.return (`String x)

    let string_option_of_body = function
        | `Empty         -> Lwt.return_none
        | `String x      -> Lwt.return_some x
        | `Strings xs    -> Lwt.return_some (String.concat "" xs)
        | `Stream _ as x -> Cohttp_lwt.Body.to_string x >>= fun str -> Lwt.return_some str
end)


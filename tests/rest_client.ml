open Cohttp
open Lwt


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type reply =
    {
    status: Code.status_code [@printer fun fmt status -> fprintf fmt "%s" (Code.string_of_status status)];
    media: string option;
    body: string;
    } [@@deriving show]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let reply =
    let module M =
    struct
        type t = reply
        let pp = pp_reply
        let equal = (=)
    end in
    (module M: Alcotest.TESTABLE with type t = M.t)

let run ?credential ?accept ?(payload = None) port meth path =
    let (content_type, body) = match payload with
        | Some (c, b) -> (Some c, b)
        | None        -> (None, "") in
    let add f v h = match v with
        | Some v -> f h v
        | None   -> h in
    let headers =
        Header.init () |>
        add Header.add_authorization credential |>
        add (fun h v -> Header.add h "Accept" v) accept |>
        add (fun h v -> Header.add h "Content-Type" v) content_type in
    let body = Cohttp_lwt.Body.of_string body in
    let uri = Uri.make ~scheme:"http" ~host:Config.host ~port ~path () in
    Cohttp_lwt_unix.Client.call ~headers ~body meth uri >>= fun (response, body) ->
    let status = Response.status response in
    let media = response |> Response.headers |> Header.get_media_type in
    Cohttp_lwt.Body.to_string body >>= fun body ->
    Lwt.return {status; media; body}


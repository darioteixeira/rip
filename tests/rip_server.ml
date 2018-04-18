(********************************************************************************)
(*  Rip_server.ml
    Copyright (c) 2016 Dario Teixeira <dario.teixeira@nleyten.com>
*)
(********************************************************************************)

open Result
open Lwt
open Rip


(********************************************************************************)
(** {1 Private functions and values}                                            *)
(********************************************************************************)

(********************************************************************************)
(** {2 Miscelaneous values}                                                     *)
(********************************************************************************)

let users = Resource.make Path.(const "users")

let user = Resource.make Path.("users" /> Arg.uint)

let mime = ("text", "plain")

let db = Database.init ()


(********************************************************************************)
(** {2 CRUD functions for collection of users}                                  *)
(********************************************************************************)

let users_get db _req () =
    Database.get_all db >>= fun names ->
    let body =
        List.map (fun (k, v) -> Printf.sprintf "(%d: %s)" k v) names |>
        String.concat "; " |>
        Printf.sprintf "[%s]" in
    Lwt.return (Outcome.ok (Some body))

let users_post db req body () = match body with
    | Some name ->
        Database.add db name >>= fun uid ->
        let uri = Resource.apply user uid in
        Lwt.return (Outcome.created uri)
    | None ->
        Lwt.return (Outcome.bad_request None)

let users_delete db _req () =
    Database.delete_all db >>= fun () ->
    Lwt.return Outcome.no_content


(********************************************************************************)
(** {2 CRUD functions for individual users}                                     *)
(********************************************************************************)

let user_get db _req uid =
    match%lwt Database.get_one db uid with
        | Some name -> Lwt.return (Outcome.ok (Some (Printf.sprintf "(%d: %s)" uid name)))
        | None      -> Lwt.return (Outcome.not_found None)

let user_put db _req body uid = match body with
    | Some name ->
        begin match%lwt Database.update db uid name with
            | true  -> Lwt.return Outcome.no_content
            | false -> Lwt.return (Outcome.not_found None)
        end
    | None ->
        Lwt.return (Outcome.bad_request None)

let user_delete db _req uid =
    match%lwt Database.delete_one db uid with
        | true  -> Lwt.return Outcome.no_content
        | false -> Lwt.return (Outcome.not_found None)


(********************************************************************************)
(** {2 Authorization}                                                           *)
(********************************************************************************)

let authorize = function
    | Some (`Basic (u, p)) when u = Config.username && p = Config.good_password -> Lwt.return @@ Ok ()
    | _                                                                         -> Lwt.return @@ Error None


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let run stop port =
    Database.init () >>= fun db -> 
    let callback = Rip_lwt.make_callback
        [
        Rip_lwt.service
            ~get:[(mime, users_get db)]
            ~post:[(mime, users_post db)]
            ~delete:(users_delete db)
            ~authorize
            users;
        Rip_lwt.service
            ~get:[(mime, user_get db)]
            ~put:[(mime, user_put db)]
            ~delete:(user_delete db)
            ~authorize
            user;
        ] in
    Cohttp_lwt_unix.Server.create ~stop ~mode:(`TCP (`Port port)) (Cohttp_lwt_unix.Server.make ~callback ())


open Lwt
open Rip
open Rip_lwt


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let authors = Resource.make Path.(const "authors")

let author = Resource.make Path.("authors" /> Arg.uint)

let mime = ("text", "plain")

let db = Database.init ()


(********************************************************************************)
(** {2 CRUD functions for collection of authors}                                *)
(********************************************************************************)

let authors_get _req () =
    let body =
        Database.get_all db |>
        List.map (fun (k, v) -> Printf.sprintf "(%d: %s)" k v) |>
        String.concat "; " |>
        Printf.sprintf "[%s]" in
    Lwt.return (Outcome.ok (Some body))

let authors_post req body () = match body with
    | Some name ->
        let uid = Database.add db name in
        let uri = Resource.apply author uid in
        Lwt.return (Outcome.created uri)
    | None ->
        Lwt.return (Outcome.bad_request None)

let authors_delete _req () =
    Database.delete_all db;
    Lwt.return Outcome.no_content


(********************************************************************************)
(** {2 CRUD functions for individual authors}                                   *)
(********************************************************************************)

let author_get _req uid =
    match Database.get_one db uid with
        | Some name -> Lwt.return (Outcome.ok (Some (Printf.sprintf "(%d: %s)" uid name)))
        | None      -> Lwt.return (Outcome.not_found None)

let author_put _req body uid = match body with
    | Some name ->
        begin match Database.update db uid name with
            | true  -> Lwt.return Outcome.no_content
            | false -> Lwt.return (Outcome.not_found None)
        end
    | None ->
        Lwt.return (Outcome.bad_request None)

let author_delete _req uid =
    match Database.delete_one db uid with
        | true  -> Lwt.return Outcome.no_content
        | false -> Lwt.return (Outcome.not_found None)


(********************************************************************************)
(** {2 Main}                                                                    *)
(********************************************************************************)

let () =
    let open Cohttp_lwt_unix in
    Lwt_main.run
    begin
        let callback = Rip_lwt.make_callback
            [
            Rip_lwt.service
                ~get:[(mime, authors_get)]
                ~post:[(mime, authors_post)]
                ~delete:authors_delete
                authors;
            Rip_lwt.service
                ~get:[(mime, author_get)]
                ~put:[(mime, author_put)]
                ~delete:author_delete
                author;
            ] in
        Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())
    end


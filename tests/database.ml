(********************************************************************************)
(*  Database.ml
    Copyright (c) 2016 Dario Teixeira <dario.teixeira@nleyten.com>
*)
(********************************************************************************)


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type t =
    {
    dict: (int, string) Hashtbl.t;
    mutable counter: int;
    }


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let init () = Lwt.return
    {
    dict = Hashtbl.create 10;
    counter = 0;
    }

let get_one db id =
    try Lwt.return_some (Hashtbl.find db.dict id)
    with Not_found -> Lwt.return_none

let get_all db =
    Lwt.return @@ Hashtbl.fold (fun k v accum -> (k, v) :: accum) db.dict []

let add db data =
    db.counter <- db.counter + 1;
    Hashtbl.add db.dict db.counter data;
    Lwt.return db.counter

let update db id data =
    if Hashtbl.mem db.dict id
    then (Hashtbl.replace db.dict id data; Lwt.return_true)
    else Lwt.return_false

let delete_one db id =
    if Hashtbl.mem db.dict id
    then (Hashtbl.remove db.dict id; Lwt.return_true)
    else Lwt.return_false

let delete_all db =
    Hashtbl.clear db.dict;
    Lwt.return_unit


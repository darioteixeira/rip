(********************************************************************************)
(*  Rip.ml
    Copyright (c) 2016 Dario Teixeira <dario.teixeira@nleyten.com>
*)
(********************************************************************************)

open Result
open Cohttp


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type mime = string * string


(********************************************************************************)
(** {1 Public modules}                                                          *)
(********************************************************************************)

module Arg =
struct
    type 'a t = 'a Tyre.t

    let identity x = x
    let try_ f x = match f x with v -> Some v | exception _ -> None
    let digits = Re.(rep1 digit)
    let neg_digits = Re.(seq [opt (char '-'); digits])

    let int = Tyre.int
    let int32 = Tyre.conv_fail ~name:"int32" (try_ Int32.of_string) (Int32.to_string) (Tyre.regex neg_digits)
    let int64 = Tyre.conv_fail ~name:"int64" (try_ Int64.of_string) (Int64.to_string) (Tyre.regex neg_digits)
    let uint = Tyre.pos_int
    let uint32 = Tyre.conv_fail ~name:"uint32" (try_ Int32.of_string) (Int32.to_string) (Tyre.regex digits)
    let uint64 = Tyre.conv_fail ~name:"uint64" (try_ Int64.of_string) (Int64.to_string) (Tyre.regex digits)
    let alnum = Tyre.conv_fail ~name:"alnum" (try_ identity) identity (Tyre.regex Re.(rep1 @@ alt [rg 'a' 'z'; rg 'A' 'Z'; rg '0' '9']))
    let oid = Tyre.conv_fail ~name:"oid" (try_ identity) identity (Tyre.regex Re.(seq [digits; rep (seq [char '.'; digits])]))
    let any = Tyre.conv_fail ~name:"any" (try_ identity) identity (Tyre.regex Re.(rep1 any))
end

module Path =
struct
    type _ t =
        | Const: string -> unit t
        | Prefix: string * 'a Arg.t -> 'a t
        | Suffix: 'a t * string -> 'a t
        | Seq: 'a t * 'b Arg.t -> ('a * 'b) t

    let const str = Const str
    let (/>) str arg = Prefix (str, arg)
    let (</) path str = Suffix (path, str)
    let (</>) path arg = Seq (path, arg)

    let rec to_tyre: type a. a t -> a Tyre.t = function
        | Const str          -> Tyre.conv_fail ~name:"unit" (function _ -> Some ()) (fun () -> "/" ^ str) (Tyre.regex @@ Re.str ("/" ^ str))
        | Prefix (str, arg)  -> Tyre.prefix (Tyre.str (Printf.sprintf "/%s/" str)) arg
        | Suffix (path, str) -> Tyre.suffix (to_tyre path) (Tyre.str ("/" ^ str))
        | Seq (path, arg)    -> Tyre.seq (Tyre.suffix (to_tyre path) (Tyre.str "/")) arg
end

module Resource =
struct
    type 'a t =
        {
        orig: 'a Tyre.t;
        comp: 'a Tyre.re;
        }

    let make path =
        let orig = Path.to_tyre path in
        let comp = Tyre.compile (Tyre.whole_string orig) in
        {orig; comp}

    let apply {orig; _} arg =
        Uri.of_string (Tyre.eval orig arg)
end

module Outcome =
struct
    open Cohttp

    type t = Response.t * string option

    let ok body = (Response.make ~status:`OK (), body)
    let created uri = (Response.make ~status:`Created (), Some (Uri.to_string uri))
    let no_content = (Response.make ~status:`No_content (), None)
    let bad_request body = (Response.make ~status:`Bad_request (), body)
    let unauthorized body = (Response.make ~status:`Unauthorized (), body)
    let forbidden body = (Response.make ~status:`Forbidden (), body)
    let not_found body = (Response.make ~status:`Not_found (), body)
    let method_not_allowed body = (Response.make ~status:`Method_not_allowed (), body)
    let not_acceptable body = (Response.make ~status:`Not_acceptable (), body)
    let conflict body = (Response.make ~status:`Conflict (), body)
    let gone body = (Response.make ~status:`Gone (), body)
    let unsupported_media_type body = (Response.make ~status:`Unsupported_media_type (), body)
    let too_many_requests body = (Response.make ~status:`Too_many_requests (), body)
    let internal_server_error body = (Response.make ~status:`Internal_server_error (), body)
    let not_implemented body = (Response.make ~status:`Not_implemented (), body)
end


(********************************************************************************)
(** {1 Public signatures}                                                       *)
(********************************************************************************)

module type BACKEND =
sig
    type 'a io
    type body
    type conn

    val return: 'a -> 'a io
    val (>>=): 'a io -> ('a -> 'b io) -> 'b io

    val body_of_string_option: string option -> body io
    val string_option_of_body: body -> string option io
end

module type S =
sig
    type 'a io
    type body
    type conn

    type 'a getter = mime * (Cohttp.Request.t -> 'a -> Outcome.t io)
    type 'a setter = mime * (Cohttp.Request.t -> string option -> 'a -> Outcome.t io)
    type 'a action = Cohttp.Request.t -> 'a -> Outcome.t io

    type service

    val service:
        ?get:'a getter list ->
        ?put:'a setter list ->
        ?post:'a setter list ->
        ?patch:'a setter list ->
        ?delete:'a action ->
        ?authorize:(Cohttp.Auth.credential option -> (unit, string option) result io) ->
        'a Resource.t ->
        service

    val make_callback: ?logger:(string -> unit io) -> service list -> (conn -> Cohttp.Request.t -> body -> (Cohttp.Response.t * body) io)
end


(********************************************************************************)
(** {1 Public functors}                                                         *)
(********************************************************************************)

module Make (Backend: BACKEND): S with
    type 'a io = 'a Backend.io and
    type body = Backend.body and
    type conn = Backend.conn =
struct
    open Backend

    type 'a io = 'a Backend.io
    type body = Backend.body
    type conn = Backend.conn

    type 'a getter = mime * (Cohttp.Request.t -> 'a -> Outcome.t io)
    type 'a setter = mime * (Cohttp.Request.t -> string option -> 'a -> Outcome.t io)
    type 'a action = Cohttp.Request.t -> 'a -> Outcome.t io

    type 'a unwrapped_service =
        {
        get: 'a getter list option;
        put: 'a setter list option;
        post: 'a setter list option;
        patch: 'a setter list option;
        delete: 'a action option;
        authorize: (Cohttp.Auth.credential option -> (unit, string option) result io) option;
        resource: 'a Resource.t;
        }

    type service = Wrapped: 'a unwrapped_service -> service

    let invoke_mime_getter req arg handlers =
        let open Accept in
        let add_media (sup, sub) handler =
            handler req arg >>= fun (response, body) ->
            let headers = Header.add_unless_exists (Response.headers response) "Content-Type" (Printf.sprintf "%s/%s" sup sub) in
            let response = {response with headers} in
            Backend.return (response, body) in
        let rec loop_media_ranges = function
            | (_, (media_range, _)) :: tl ->
                let rec loop_handlers = function
                    | [] ->
                        loop_media_ranges tl
                    | (mime, handler) :: tl -> match (media_range, mime) with
                        | (MediaType (a, b), (x, y)) when a = x && b = y -> add_media mime handler
                        | (AnyMediaSubtype a, (x, _)) when a = x         -> add_media mime handler
                        | (AnyMedia, _)                                  -> add_media mime handler
                        | _                                              -> loop_handlers tl in
                loop_handlers handlers
            | [] ->
                Backend.return @@ Outcome.not_acceptable None in
        req |>
        Request.headers |>
        Header.get_acceptable_media_ranges |>
        Accept.qsort |>
        loop_media_ranges

    let invoke_mime_setter req body arg handlers =
        let open Accept in
        let rec loop media_range = function
            | [] ->
                Backend.return @@ Outcome.unsupported_media_type None
            | hd :: tl -> match (media_range, hd) with
                | (MediaType (a, b), ((x, y), handler)) when a = x && b = y -> handler req body arg
                | (AnyMediaSubtype a, ((x, _), handler)) when a = x         -> handler req body arg
                | (AnyMedia, (_, handler))                                  -> handler req body arg
                | _                                                         -> loop media_range tl in
        match req |> Request.headers |> Header.get_media_type |> Accept.media_ranges with
            | [(_, (media_range, _))] -> loop media_range handlers
            | _                       -> assert false   (* This should never happen *)

    let invoke svc req body arg = match (Request.meth req, svc) with
        | (`GET, {get = Some handlers; _}) ->
            invoke_mime_getter req arg handlers
        | (`PUT, {put = Some handlers; _}) ->
            invoke_mime_setter req body arg handlers
        | (`POST, {post = Some handlers; _}) ->
            invoke_mime_setter req body arg handlers
        | (`PATCH, {patch = Some handlers; _}) ->
            invoke_mime_setter req body arg handlers
        | (`DELETE, {delete = Some handler; _}) ->
            handler req arg
        | (`OPTIONS, svc) ->
            let add name meth accum = match meth with Some _ -> name :: accum | None -> accum in
            let allowed =
                ["OPTIONS"] |>
                add "GET" svc.get |>
                add "PUT" svc.put |>
                add "POST" svc.post |>
                add "PATCH" svc.patch |>
                add "DELETE" svc.delete |>
                String.concat ", " in
            let headers = Header.init_with "Allow" allowed in
            Backend.return @@ (Response.make ~status:`OK ~headers (), None)
        | _ ->
            Backend.return @@ Outcome.method_not_allowed None

    let service ?get ?put ?post ?patch ?delete ?authorize resource =
        Wrapped {get; put; post; patch; delete; authorize; resource}

    let make_callback ?(logger = fun _str -> Backend.return ()) svcs = fun _conn req body ->
        let path = req |> Request.uri |> Uri.path in
        let rec loop = function
            | [] ->
                logger (Printf.sprintf "Unable to handle resource '%s'" path) >>= fun () ->
                Backend.return @@ Outcome.not_found None
            | Wrapped hd :: tl -> match Tyre.exec hd.resource.comp path with
                | Ok arg ->
                    begin
                        begin match (Request.meth req, hd.authorize) with
                            | (`OPTIONS, _) | (_, None) -> Backend.return @@ Ok ()
                            | (_, Some f)               -> req |> Request.headers |> Header.get_authorization |> f
                        end >>= function
                            | Ok () ->
                                Backend.string_option_of_body body >>= fun body ->
                                invoke hd req body arg
                            | Error body ->
                                Backend.return @@ Outcome.unauthorized body
                    end
                | Error _ ->
                    loop tl in
        loop svcs >>= fun (response, body) ->
        Backend.body_of_string_option body >>= fun body ->
        Backend.return (response, body)
end


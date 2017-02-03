(********************************************************************************)
(*  Rip.mli
    Copyright (c) 2016 Dario Teixeira <dario.teixeira@nleyten.com>
*)
(********************************************************************************)

open Result


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type mime = string * string


(********************************************************************************)
(** {1 Public modules}                                                          *)
(********************************************************************************)

module Arg:
sig
    type 'a t

    val int: int t
    val int32: int32 t
    val int64: int64 t
    val uint: int t
    val uint32: int32 t
    val uint64: int64 t
    val alnum: string t
end

module Path:
sig
    type 'a t

    val const: string -> unit t
    val (/>): string -> 'a Arg.t -> 'a t
    val (</): 'a t -> string -> 'a t
    val (</>): 'a t -> 'b Arg.t -> ('a * 'b) t
end

module Resource:
sig
    type 'a t

    val make: 'a Path.t -> 'a t
    val apply: 'a t -> 'a -> Uri.t
end

module Outcome:
sig
    type t = Cohttp.Response.t * string option

    val ok: string option -> t
    (** Code is [200 OK]. The request was accepted and successful, and the response
        contains the result. Since this is a general purpose response code, it can
        be returned from any request. For GET requests, return the requested resource
        or data in the response body. It's also commonly used for PUT or DELETE
        requests when there is information about the result to be conveyed in the
        response body. *)

    val created: Uri.t -> t
    (** Code is [201 CREATED]. Use this function to indicate that a PUT or POST method
        successfully created a new resource.  It is customary for the body to consist
        solely of the URI of the newly created resource. *)

    val no_content: t
    (** Code is [204 NO CONTENT]. The request was successful but that there is nothing
        to return. Used for instance with DELETE requests. *)

    val bad_request: string option -> t
    (** Code in [400 BAD REQUEST]. The request was not valid. Returned, for instance,
        when the data submitted in a PUT or POST request is not properly formatted. *)

    val unauthorized: string option -> t
    (** Code is [401 UNAUTHORIZED]. Returned when authorization is required but not
        provided or invalid. *)

    val forbidden: string option -> t
    (** Code is [403 FORBIDDEN]. Indicates that the client does not have permission
        to perform the given method on this resource.  It should only be returned to
        clients which have otherwise valid authorization. *)

    val not_found: string option -> t
    (** Code is [404 NOT FOUND]. Indicates that the resource does not exist. *)

    val method_not_allowed: string option -> t
    (** Code is [405 METHOD NOT ALLOWED]. Indicates that the resource does not support
        the requested method. *)

    val not_acceptable: string option -> t
    (** Code is [406 NOT ACCEPTABLE]. Indicates that the server does not know how to
        return data in any of formats requested by the client via an [Accept] header. *)

    val conflict: string option -> t
    (** Code is [409 CONFLICT]. Indicates that a conflict occurred during the attempt
        to modify the resource. The response body should contain further information. *)

    val gone: string option -> t
    (** Code is [410 GONE]. Indicates that the resource is no longer available. Useful,
        for instance, as a response to clients expecting an old and no longer supported
        version of the API. *)

    val unsupported_media_type: string option -> t
    (** Code is [415 UNSUPPORTED MEDIA TYPE]. Indicates that the server does not know
        how to handle the format specified by the client in the [Content-Type] header. *)

    val too_many_requests: string option -> t
    (** Code is [429 TOO MANY REQUESTS]. The request was rejected due to rate limiting. *)

    val internal_server_error: string option -> t
    (** Code is [500 INTERNAL SERVER ERROR]. The server experienced an internal error.
        More information is expected in the reponse body. *)

    val not_implemented: string option -> t
    (** Code is [501 NOT IMPLEMENTED]. The server cannot carry out the request, possibly
        because the functionality is not yet implemented. *)
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

module Make: functor (Backend: BACKEND) -> S with
    type 'a io = 'a Backend.io and
    type body = Backend.body and
    type conn = Backend.conn


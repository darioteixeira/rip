open Cohttp


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type reply =
    {
    status: Code.status_code;
    media: string option;
    body: string;
    }


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val reply: reply Alcotest.testable

val run: ?credential:Auth.credential -> ?accept:string -> ?payload:(string * string) option -> int -> Code.meth -> string -> reply Lwt.t


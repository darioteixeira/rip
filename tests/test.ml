(********************************************************************************)
(*  Test_with_lwt.ml
    Copyright (c) 2016 Dario Teixeira <dario.teixeira@nleyten.com>
*)
(********************************************************************************)

open Lwt


(********************************************************************************)
(** {1 Tests}                                                                   *)
(********************************************************************************)

let good_credential = `Basic (Config.username, Config.good_password)

let bad_credential = `Basic (Config.username, Config.bad_password)

let accept = "*/*"

let credential = good_credential

let test_authorization port =
    Rest_client.run ~accept port `GET "/users" >>= fun rep0 ->
    Alcotest.(check Rest_client.reply) "0" {status = `Unauthorized; media = None; body = ""} rep0;
    Rest_client.run ~credential:bad_credential ~accept port `GET "/users" >>= fun rep1 ->
    Alcotest.(check Rest_client.reply) "1" {status = `Unauthorized; media = None; body = ""} rep1;
    Rest_client.run ~credential:good_credential ~accept port `GET "/users" >>= fun rep2 ->
    Alcotest.(check Rest_client.reply) "2" {status = `OK; media = Some "text/plain"; body = "[]"} rep2;
    Lwt.return ()
    
let test_get port =
    Rest_client.run ~credential ~accept port `GET "/users" >>= fun rep0 ->
    Alcotest.(check Rest_client.reply) "0" {status = `OK; media = Some "text/plain"; body = "[]"} rep0;
    Lwt.return_unit

let test_post port =
    Rest_client.run ~credential ~accept ~payload:(Some ("text/plain", "test1")) port `POST "/users" >>= fun rep0 ->
    Alcotest.(check Rest_client.reply) "0" {status = `Created; media = None; body = "/users/1"} rep0;
    Rest_client.run ~credential ~accept port `GET "/users" >>= fun rep1 ->
    Alcotest.(check Rest_client.reply) "1" {status = `OK; media = Some "text/plain"; body = "[(1: test1)]"} rep1;
    Rest_client.run ~credential ~accept port `GET "/users/1" >>= fun rep2 ->
    Alcotest.(check Rest_client.reply) "2" {status = `OK; media = Some "text/plain"; body = "(1: test1)"} rep2;
    Lwt.return_unit

let test_put port =
    Rest_client.run ~credential ~accept ~payload:(Some ("text/plain", "test1a")) port `POST "/users" >>= fun rep0 ->
    Alcotest.(check Rest_client.reply) "0" {status = `Created; media = None; body = "/users/1"} rep0;
    Rest_client.run ~credential ~accept ~payload:(Some ("text/plain", "test1b")) port `PUT "/users/1" >>= fun rep1 ->
    Alcotest.(check Rest_client.reply) "1" {status = `No_content; media = None; body = ""} rep1;
    Rest_client.run ~credential ~accept port `GET "/users/1" >>= fun rep2 ->
    Alcotest.(check Rest_client.reply) "2" {status = `OK; media = Some "text/plain"; body = "(1: test1b)"} rep2;
    Lwt.return_unit

let test_delete port =
    Rest_client.run ~credential ~accept ~payload:(Some ("text/plain", "test1")) port `POST "/users" >>= fun rep0 ->
    Alcotest.(check Rest_client.reply) "0" {status = `Created; media = None; body = "/users/1"} rep0;
    Rest_client.run ~credential ~accept port `DELETE "/users/1" >>= fun rep1 ->
    Alcotest.(check Rest_client.reply) "1" {status = `No_content; media = None; body = ""} rep1;
    Rest_client.run ~credential ~accept port `GET "/users/1" >>= fun rep2 ->
    Alcotest.(check Rest_client.reply) "2" {status = `Not_found; media = Some "text/plain"; body = ""} rep2;
    Rest_client.run ~credential ~accept ~payload:(Some ("text/plain", "test2")) port `POST "/users" >>= fun rep3 ->
    Alcotest.(check Rest_client.reply) "3" {status = `Created; media = None; body = "/users/2"} rep3;
    Rest_client.run ~credential ~accept ~payload:(Some ("text/plain", "test3")) port `POST "/users" >>= fun rep4 ->
    Alcotest.(check Rest_client.reply) "4" {status = `Created; media = None; body = "/users/3"} rep4;
    Rest_client.run ~credential ~accept port `GET "/users" >>= fun rep5 ->
    Alcotest.(check Rest_client.reply) "5" {status = `OK; media = Some "text/plain"; body = "[(3: test3); (2: test2)]"} rep5;
    Rest_client.run ~credential ~accept port `DELETE "/users" >>= fun rep6 ->
    Alcotest.(check Rest_client.reply) "6" {status = `No_content; media = None; body = ""} rep6;
    Rest_client.run ~credential ~accept port `GET "/users" >>= fun rep7 ->
    Alcotest.(check Rest_client.reply) "7" {status = `OK; media = Some "text/plain"; body = "[]"} rep7;
    Lwt.return_unit

let test_accept port =
    Rest_client.run ~credential ~accept:"text/plain" port `GET "/users" >>= fun rep0 ->
    Alcotest.(check Rest_client.reply) "0" {status = `OK; media = Some "text/plain"; body = "[]"} rep0;
    Rest_client.run ~credential ~accept:"text/*" port `GET "/users" >>= fun rep1 ->
    Alcotest.(check Rest_client.reply) "1" {status = `OK; media = Some "text/plain"; body = "[]"} rep1;
    Rest_client.run ~credential ~accept:"application/json" port `GET "/users" >>= fun rep2 ->
    Alcotest.(check Rest_client.reply) "2" {status = `Not_acceptable; media = None; body = ""} rep2;
    Rest_client.run ~credential ~accept:"application/*" port `GET "/users" >>= fun rep3 ->
    Alcotest.(check Rest_client.reply) "3" {status = `Not_acceptable; media = None; body = ""} rep3;
    Lwt.return_unit

let test_content port =
    Rest_client.run ~credential ~accept ~payload:(Some ("text/plain", "test1")) port `POST "/users" >>= fun rep0 ->
    Alcotest.(check Rest_client.reply) "0" {status = `Created; media = None; body = "/users/1"} rep0;
    Rest_client.run ~credential ~accept ~payload:(Some ("application/json", "test1")) port `POST "/users" >>= fun rep0 ->
    Alcotest.(check Rest_client.reply) "0" {status = `Unsupported_media_type; media = None; body = ""} rep0;
    Lwt.return_unit


(********************************************************************************)
(** {1 Main}                                                                    *)
(********************************************************************************)

let run offset test () =
    let port = Config.port + offset in
    Lwt_main.run
    begin
        let (stop, stopper) = Lwt.wait () in
        let _ = Rip_server.run stop port in
        test port >>= fun () ->
        Lwt.wakeup stopper ();
        Lwt.return_unit
    end

let testset =
    [
    ("Authorization", `Quick, run 0 test_authorization);
    ("Get", `Quick, run 1 test_get);
    ("Post", `Quick, run 2 test_post);
    ("Put", `Quick, run 3 test_put);
    ("Delete", `Quick, run 4 test_delete);
    ("Mime types in Accept header", `Quick, run 5 test_accept);
    ("Mime types in Content-type header", `Quick, run 6 test_content);
    ]

let () =
    Alcotest.run "Rip with Lwt" [("Standard testset", testset)]


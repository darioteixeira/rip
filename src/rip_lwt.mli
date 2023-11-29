include Rip.S with
    type 'a io = 'a Lwt.t and
    type body = Cohttp_lwt.Body.t and
    type conn = Cohttp_lwt_unix.Server.IO.conn * Cohttp.Connection.t


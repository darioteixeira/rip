RIP (REST in Peace)
===================

RIP is a simple framework for building RESTful servers in OCaml.  It's basically a thin
wrapper on top of Cohttp that takes care of some of the boilerplate involved in writing
a RESTful server.  It makes sure the proper response codes are sent when various error
conditions occur, and automatically routes requests based on content-type and accept
headers.  Another notable feature is the support for type-safe routing to resources.

RIP is developed by Dario Teixeira <`dario.teixeira@nleyten.com`> and is licensed under
the terms of the [LGPL 2.1](http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html)
(with the OCaml linking exception).


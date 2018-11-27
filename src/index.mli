type imandraSyntax = Reason | OCaml

type imandraOptions =
  < syntax : imandraSyntax
  ; debug : bool
  > Js.t

type imandraProcess

val waitForPrompt : imandraProcess -> string -> unit Js.Promise.t

val start : imandraOptions -> imandraProcess Js.Promise.t

(* val stop : imandraProcess -> unit Js.Promise.t *)


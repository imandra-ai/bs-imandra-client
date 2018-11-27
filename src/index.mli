type imandraSyntax

type imandraOptions

type imandraProcess

(* val wait_for_prompt : imandraProcess -> unit Js.Promise.t *)

val start : imandraOptions -> imandraProcess Js.Promise.t

(* val stop : imandraProcess -> unit Js.Promise.t *)


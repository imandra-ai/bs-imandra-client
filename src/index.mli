type imandraSyntax = Reason | OCaml

type imandraOptions =
  { syntax : imandraSyntax
  ; debug : bool
  } [@@bs.deriving abstract]

type imandraProcess =
  { nodeProcess : Node.Child_process.spawnResult
  } [@@bs.deriving abstract]

val waitForPrompt : imandraProcess -> string -> 'a Js.undefined Js.Promise.t

val start : imandraOptions -> imandraProcess Js.Promise.t

(* val stop : imandraProcess -> unit Js.Promise.t *)


type imandraOptions =
  { syntax : string
  ; debug : bool [@bs.optional]
  ; serverCmd : string [@bs.optional]
  } [@@bs.deriving abstract]

type imandraProcess =
  { nodeProcess : Node.Child_process.spawnResult
  ; port : int
  } [@@bs.deriving abstract]

val start : imandraOptions -> imandraProcess Js.Promise.t

val stop : imandraProcess -> unit Js.Promise.t


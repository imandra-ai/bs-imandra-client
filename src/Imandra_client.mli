type imandraOptions =
  { syntax : string
  ; debug : bool [@bs.optional]
  ; serverCmd : string [@bs.optional]
  } [@@bs.deriving abstract]

type imandraProcess =
  { nodeProcess : Node.Child_process.spawnResult
  ; port : int
  ; baseUrl : string
  } [@@bs.deriving abstract]

val start : imandraOptions -> imandraProcess Js.Promise.t

val stop : imandraProcess -> unit Js.Promise.t

type model =
  { language : string
  ; src : string
  }

type refutedCounterexample =
  { model : model }

type unknownResult =
  { unknownReason: string }

type refutedResult =
  { refutedCounterexample: refutedCounterexample }

type verifyResult =
  | Proved
  | Unknown of unknownResult
  | Refuted of refutedResult

val verify : imandraProcess -> src:string -> Js.Json.t Js.Promise.t

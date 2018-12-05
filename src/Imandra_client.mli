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

module Verify : sig
  type model =
    { language : string
    ; src : string
    }

  type counterexample =
    { model : model }

  type unknownResult =
    { reason: string }

  type refutedResult =
    { counterexample: counterexample }

  type verifyResult =
    | Proved
    | Unknown of unknownResult
    | Refuted of refutedResult

  module Decode : sig
    val verifyResult : Js.Json.t -> verifyResult
  end

  val by_src : imandraProcess -> src:string -> Js.Json.t Js.Promise.t
  val by_name : imandraProcess -> name:string -> Js.Json.t Js.Promise.t
end

module Eval : sig
  val by_src : imandraProcess -> src:string -> Js.Json.t Js.Promise.t
end

module Instance : sig
  type model =
    { language : string
    ; src : string
    }

  type example =
    { model : model }

  type unknownResult =
    { reason: string }

  type satResult =
    { example: example }

  type instanceResult =
    | Sat of satResult
    | Unknown of unknownResult
    | Unsat

  module Decode : sig
    val instanceResult : Js.Json.t -> instanceResult
  end

  val by_src : imandraProcess -> src:string -> Js.Json.t Js.Promise.t
  val by_name : imandraProcess -> name:string -> Js.Json.t Js.Promise.t
end

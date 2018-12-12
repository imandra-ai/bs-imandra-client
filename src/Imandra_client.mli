val function_name: 'a -> string

type imandraOptions =
  { debug : bool [@bs.optional]
  ; serverCmd : string [@bs.optional]
  } [@@bs.deriving abstract]

type imandraServerInfo =
  { port : int
  ; baseUrl : string
  } [@@bs.deriving abstract]

val start : imandraOptions -> (Node.Child_process.spawnResult * imandraServerInfo) Js.Promise.t

val stop : Node.Child_process.spawnResult -> unit Js.Promise.t

type 'a with_json =
  ('a * Js.Json.t)

type error = string

type syntax =
  | OCaml
  | Reason

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

  val by_src : ?syntax:syntax -> src:string -> imandraServerInfo -> (verifyResult with_json, error with_json ) Belt.Result.t Js.Promise.t
  val by_name : name:string -> imandraServerInfo -> (verifyResult with_json, error with_json ) Belt.Result.t Js.Promise.t
end

module Eval : sig
  val by_src : ?syntax:syntax -> src:string -> imandraServerInfo -> (unit with_json, error with_json) Belt.Result.t Js.Promise.t
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

  val by_src : ?syntax:syntax -> src:string -> imandraServerInfo -> (instanceResult with_json, error with_json) Belt.Result.t Js.Promise.t
  val by_name : name:string -> imandraServerInfo -> (instanceResult with_json, error with_json) Belt.Result.t Js.Promise.t
end

val reset : imandraServerInfo -> (unit with_json, error with_json) Belt.Result.t Js.Promise.t

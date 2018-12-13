val function_name: 'a -> string

type imandraOptions =
  { debug : bool [@bs.optional]
  ; serverCmd : string [@bs.optional]
  } [@@bs.deriving abstract]

module ServerInfo : sig
  type t =
    { port : int
    ; baseUrl : string
    }

  module Encode : sig
    val t : t -> Js.Json.t
  end

  module Decode : sig
    val t : Js.Json.t -> t
  end

  val to_file : ?filename:string -> t -> unit
  val from_file : ?filename:string -> unit -> t
  val cleanup : ?filename:string -> unit -> unit

end

val start : imandraOptions -> (Node.Child_process.spawnResult * ServerInfo.t) Js.Promise.t

val stop : Node.Child_process.spawnResult -> unit Js.Promise.t

type 'a with_json =
  ('a * Js.Json.t)

type error = string

module Syntax : sig
  type t =
    | OCaml
    | Reason
end

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

  val by_src : syntax:Syntax.t -> src:string -> ServerInfo.t -> (verifyResult with_json, error with_json ) Belt.Result.t Js.Promise.t
  val by_name : name:string -> ServerInfo.t -> (verifyResult with_json, error with_json ) Belt.Result.t Js.Promise.t
end

module Eval : sig
  val by_src : syntax:Syntax.t -> src:string -> ServerInfo.t -> (unit with_json, error with_json) Belt.Result.t Js.Promise.t
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

  val by_src : syntax:Syntax.t -> src:string -> ServerInfo.t -> (instanceResult with_json, error with_json) Belt.Result.t Js.Promise.t
  val by_name : name:string -> ServerInfo.t -> (instanceResult with_json, error with_json) Belt.Result.t Js.Promise.t
end

val reset : ServerInfo.t -> (unit with_json, error with_json) Belt.Result.t Js.Promise.t

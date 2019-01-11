val function_name: 'a -> string

type imandraOptions =
  { debug : bool [@bs.optional]
  ; serverCmd : string [@bs.optional]
  } [@@bs.deriving abstract]

module PrinterDetails : sig
  type t =
    { name : string
    ; cx_var_name : string
    }
end

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

  val toFile : ?filename:string -> t -> unit
  val fromFile : ?filename:string -> unit -> t
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

module Model : sig
  type t =
    { syntax : Syntax.t
    ; src : string
    }
end

module Response : sig
  type instance =
    { model : Model.t
    ; type_ : string
    ; printed : string option
    }
end

module Verify : sig
  type unknownResult =
    { reason : string }

  type refutedResult =
    { instance: Response.instance }

  type verifyResult =
    | Proved
    | Unknown of unknownResult
    | Refuted of refutedResult

  val bySrc : ?instancePrinter:PrinterDetails.t -> syntax:Syntax.t -> src:string -> ServerInfo.t -> (verifyResult with_json, error with_json ) Belt.Result.t Js.Promise.t
  val byName : ?instancePrinter:PrinterDetails.t -> name:string -> ServerInfo.t -> (verifyResult with_json, error with_json ) Belt.Result.t Js.Promise.t
end

module Eval : sig
  val bySrc : syntax:Syntax.t -> src:string -> ServerInfo.t -> (unit with_json, error with_json) Belt.Result.t Js.Promise.t
end

module Instance : sig
  type unknownResult =
    { reason: string }

  type satResult =
    { instance: Response.instance }

  type instanceResult =
    | Sat of satResult
    | Unknown of unknownResult
    | Unsat

  val bySrc : ?instancePrinter:PrinterDetails.t -> syntax:Syntax.t -> src:string -> ServerInfo.t -> (instanceResult with_json, error with_json) Belt.Result.t Js.Promise.t
  val byName : ?instancePrinter:PrinterDetails.t -> name:string -> ServerInfo.t -> (instanceResult with_json, error with_json) Belt.Result.t Js.Promise.t
end

val reset : ServerInfo.t -> (unit with_json, error with_json) Belt.Result.t Js.Promise.t

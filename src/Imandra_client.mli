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
    val t : t Decoders_bs.Encode.encoder
  end

  module Decode : sig
    val t : t Decoders_bs.Decode.decoder
  end

  val toFile : ?filename:string -> t -> unit
  val fromFile : ?filename:string -> unit -> (t, string) Belt.Result.t
  val cleanup : ?filename:string -> unit -> unit

end

val start : imandraOptions -> (Node.Child_process.spawnResult * ServerInfo.t) Js.Promise.t

val stop : Node.Child_process.spawnResult -> unit Js.Promise.t

module Error : sig
  type t =
    | Decoder_error of Decoders_bs.Decode.error
    | Imandra_error of Api.Response.error

  val pp : Format.formatter -> t -> unit
end


module Verify : sig
  val bySrc
    : ?instancePrinter:Api.Request.printer_details
    -> ?hints:Api.Request.Hints.t
    -> syntax:Api.src_syntax -> src:string
    -> ServerInfo.t
    -> (Api.Response.verify_result, Error.t) Belt.Result.t Js.Promise.t

  val byName
    : ?instancePrinter:Api.Request.printer_details
    -> ?hints:Api.Request.Hints.t
    -> name:string
    -> ServerInfo.t
    -> (Api.Response.verify_result, Error.t) Belt.Result.t Js.Promise.t
end

module Eval : sig
  val bySrc
    : syntax:Api.src_syntax
    -> src:string
    -> ServerInfo.t
    -> (unit, Error.t) Belt.Result.t Js.Promise.t
end

module Instance : sig
  val bySrc
    : ?instancePrinter:Api.Request.printer_details
    -> syntax:Api.src_syntax
    -> src:string
    -> ServerInfo.t
    -> (Api.Response.instance_result, Error.t) Belt.Result.t Js.Promise.t

  val byName
    : ?instancePrinter:Api.Request.printer_details
    -> name:string
    -> ServerInfo.t
    -> (Api.Response.instance_result, Error.t) Belt.Result.t Js.Promise.t
end

val reset
  : ServerInfo.t
  -> (unit, Error.t) Belt.Result.t Js.Promise.t

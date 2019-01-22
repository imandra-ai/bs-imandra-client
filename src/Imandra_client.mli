val function_name: 'a -> string

module Api = Imandra_client_api

type imandra_options =
  { debug : bool [@bs.optional]
  ; server_cmd : string [@bs.optional]
  } [@@bs.deriving abstract]

module Server_info : sig
  type t =
    { port : int
    ; base_url : string
    }

  module Encode : sig
    val t : t Decoders_bs.Encode.encoder
  end

  module Decode : sig
    val t : t Decoders_bs.Decode.decoder
  end

  val to_file : ?filename:string -> t -> unit
  val from_file : ?filename:string -> unit -> (t, string) Belt.Result.t
  val cleanup : ?filename:string -> unit -> unit

end

val start : imandra_options -> (Node.Child_process.spawnResult * Server_info.t) Js.Promise.t

val stop : Node.Child_process.spawnResult -> unit Js.Promise.t

module Error : sig
  type t =
    | Decoder_error of Decoders_bs.Decode.error
    | Imandra_error of Api.Response.error

  val pp : Format.formatter -> t -> unit

  val pp_str : t -> string
end

val to_base64 : string -> string

val from_base64 : string -> string

module Verify : sig
  val by_src
    : ?instance_printer:Api.Request.printer_details
    -> ?hints:Api.Request.Hints.t
    -> syntax:Api.src_syntax -> src:string
    -> Server_info.t
    -> (Api.Response.verify_result, Error.t) Belt.Result.t Js.Promise.t

  val by_name
    : ?instance_printer:Api.Request.printer_details
    -> ?hints:Api.Request.Hints.t
    -> name:string
    -> Server_info.t
    -> (Api.Response.verify_result, Error.t) Belt.Result.t Js.Promise.t
end

module Eval : sig
  val by_src
    : syntax:Api.src_syntax
    -> src:string
    -> Server_info.t
    -> (unit, Error.t) Belt.Result.t Js.Promise.t
end

module Instance : sig
  val by_src
    : ?instance_printer:Api.Request.printer_details
    -> syntax:Api.src_syntax
    -> src:string
    -> Server_info.t
    -> (Api.Response.instance_result, Error.t) Belt.Result.t Js.Promise.t

  val by_name
    : ?instance_printer:Api.Request.printer_details
    -> name:string
    -> Server_info.t
    -> (Api.Response.instance_result, Error.t) Belt.Result.t Js.Promise.t
end

val reset
  : Server_info.t
  -> (unit, Error.t) Belt.Result.t Js.Promise.t

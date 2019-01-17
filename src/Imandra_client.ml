[%raw "require('isomorphic-fetch')"]
external spawn : string -> string array -> Node.Child_process.spawnResult = "" [@@bs.module "child_process"]
external getPort : unit -> int Js.Promise.t = "getPortPromise" [@@bs.module "portfinder"]

module D = Api.Decoders(Decoders_bs.Decode)
module E = Api.Encoders(Decoders_bs.Encode)

let function_name = [%raw fun f -> "{return f.name}"]

external spawnOn
  : Node.Child_process.spawnResult
    -> ([ `close of int -> unit ] [@bs.string])
    -> Node.Child_process.spawnResult = "on" [@@bs.send]

external spawnOff
  : Node.Child_process.spawnResult
    -> ([ `close of int -> unit ] [@bs.string])
    -> Node.Child_process.spawnResult = "off" [@@bs.send]

external bufferOn
  : Node.string_buffer
    -> ([ `data of Node.Buffer.t -> unit] [@bs.string])
    -> Node.string_buffer = "on" [@@bs.send]

external bufferOff
  : Node.string_buffer
    -> ([ `data of Node.Buffer.t -> unit] [@bs.string])
    -> Node.string_buffer = "off" [@@bs.send]

type imandraOptions =
  { debug : bool [@bs.optional]
  ; serverCmd : string [@bs.optional]
  } [@@bs.deriving abstract]

type imandraOptionsWithDefaults =
  { debug : bool
  ; serverCmd : string
  }

module ServerInfo = struct
  type t =
    { port : int
    ; baseUrl : string
    }

  module Encode = struct
    open Decoders_bs.Encode
    let t t =
      obj [ ("port", int t.port)
          ; ("baseUrl", string t.baseUrl)]
  end

  module Decode = struct
    open Decoders_bs.Decode
    let t : t decoder =
      field "port" int >>= fun port ->
      field "baseUrl" string >>= fun baseUrl ->
      succeed { port; baseUrl }
  end

  let toFile ?(filename=".imandra-server-info") (t : t) =
    let j_str = Decoders_bs.Encode.encode_string Encode.t t in
    Node.Fs.writeFileSync filename j_str `utf8

  let fromFile ?(filename=".imandra-server-info") () =
    Node.Fs.readFileSync filename `utf8
    |> Decoders_bs.Decode.decode_string Decode.t
    |> Decoders_util.My_result.map_err (Format.asprintf "%a" Decoders_bs.Decode.pp_error)

  let cleanup ?(filename=".imandra-server-info") () =
    Node.Fs.unlinkSync filename;
end

external bufferToStringWithEncoding : Node.Buffer.t ->
  ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary  | `hex ] [@bs.string]) ->
  string = "toString" [@@bs.send]

let to_base64 (s : string) =
  let b = Node.Buffer.fromString s in
  (bufferToStringWithEncoding b `base64)

module Error = struct
  type t =
    | Decoder_error of Decoders_bs.Decode.error
    | Imandra_error of Api.Response.error

  let pp fmt = function
    | Decoder_error e -> Format.fprintf fmt "Decoder_error: %a" Decoders_bs.Decode.pp_error e
    | Imandra_error e -> Format.fprintf fmt "Imandra_error: %s" e.error
end

let printStreamsDebug (np : Node.Child_process.spawnResult) =
  let props = Node.Child_process.readAs np in
  let so = props##stdout |> Js.Null.getExn in
  let se = props##stderr |> Js.Null.getExn in

  ignore
    (so |. bufferOn (`data (fun b ->
         let s = Node.Buffer.toString b in
         Js.Console.log (Printf.sprintf "STDOUT: %s" s)
       )));

  ignore
    ( se |. bufferOn (`data (fun b ->
          let s = Node.Buffer.toString b in
          Js.Console.log (Printf.sprintf "STDERR: %s" s)
        )))

let makeBaseUrl (port : int) =
  Printf.sprintf "http://localhost:%d" port

let timeout ms : unit Js.Promise.t =
  Js.Promise.make (fun ~resolve ~reject:_ ->
      let id = ref None in
      let theId = Js.Global.setTimeout (fun () ->
          resolve !id [@bs];
        ) ms
      in
      id := Some theId;
      ()
    )
  |> Js.Promise.then_ (fun _ ->
      Js.Promise.resolve ()
    )

let waitForServer (port : int) : unit Js.Promise.t =
  Js.Promise.make (fun ~resolve ~reject:_ ->
      let rec checkStatus () =
        Fetch.fetch (Printf.sprintf "%s/status" (makeBaseUrl port))
        |> Js.Promise.then_ (fun res ->
            resolve res [@bs];
            Js.Promise.resolve ();
          )
        |> Js.Promise.catch (fun _ ->
            (timeout 1000)
            |> Js.Promise.then_ checkStatus
          )

      in ignore (checkStatus ())
    )
  |> Js.Promise.then_ (fun _ ->
      Js.Promise.resolve ()
    )

let withDefaults (opts : imandraOptions) : imandraOptionsWithDefaults =
  { debug = (match (opts |. debugGet) with | None -> false | Some d -> d)
  ; serverCmd = (match (opts |. serverCmdGet) with | None -> "imandra-http-server" | Some s -> s)
  }

let start (passedOpts : imandraOptions) : (Node.Child_process.spawnResult * ServerInfo.t) Js.Promise.t =

  let opts = withDefaults passedOpts in

  let startupExitOutputCb = ref (fun _ -> ()) in
  let startupExitSpawnCb = ref (fun _ -> ()) in

  Js.Promise.make (fun ~resolve ~reject ->
      let listenForStartupClose (np : Node.Child_process.spawnResult) =
        let props = Node.Child_process.readAs np in
        let so = props##stdout |> Js.Null.getExn in
        let seText = ref "" in
        startupExitOutputCb :=
          (fun b ->
             let s = Node.Buffer.toString b in
             seText := Js.String.concat !seText s;
          );

        startupExitSpawnCb :=
          (fun code ->
             Js.Console.error !seText;
             reject (Failure (Printf.sprintf "Imandra process exited during startup (code: %d)." code)) [@bs]);

        (so |. bufferOn (`data !startupExitOutputCb) |> ignore);
        (np |. spawnOn (`close !startupExitSpawnCb) |> ignore);
      in

      let unlistenForStartupClose (np : Node.Child_process.spawnResult) =
        let props = Node.Child_process.readAs np in
        let so = props##stdout |> Js.Null.getExn in
        so |. bufferOff (`data !startupExitOutputCb) |> ignore;
        np |. spawnOff (`close !startupExitSpawnCb) |> ignore;
      in

      getPort ()
      |> Js.Promise.then_ (fun port ->
          (* Always set reason to load the reason parser. Syntax is specified per-call *)
          let args = [|"--non-interactive"; "-reason"; "-port"; (string_of_int port)|] in
          let np = spawn opts.serverCmd args in

          listenForStartupClose np;

          if (opts.debug) then
            printStreamsDebug np
          else
            ();

          waitForServer port
          |> Js.Promise.then_ (fun () ->
              unlistenForStartupClose np;
              resolve (np, ServerInfo.{ port = port;  baseUrl=("http://localhost:" ^ (string_of_int port)) }) [@bs];
              Js.Promise.resolve ();
            )
        )
      |> ignore
    )

external spawnKill : Node.Child_process.spawnResult -> int -> unit = "kill" [@@bs.send]

let stop (np : Node.Child_process.spawnResult) : unit Js.Promise.t =
  Js.Promise.make (fun ~resolve ~reject:_ ->
      let handler = ref (fun _ -> ()) in
      handler := (fun code ->
          np |. spawnOff (`close !handler) |> ignore;
          resolve code [@bs];
        );
      np |. spawnOn (`close !handler) |> ignore;
      np |. spawnKill 2 |> ignore;
    )
  |> Js.Promise.then_ (fun _ -> Js.Promise.resolve ())

let handle_response decoder res =
  let status = Fetch.Response.status res in
  Fetch.Response.json res
  |> Js.Promise.then_ (fun json ->
      if status = 200 then
        (Decoders_bs.Decode.decode_value decoder json)
        |> Decoders_util.My_result.map_err (fun e -> Error.Decoder_error e)
        |> Js.Promise.resolve
      else
        (Decoders_bs.Decode.decode_value D.Response.error json)
        |> Decoders_util.My_result.map_err (fun e -> Error.Decoder_error e)
        |> (fun r -> Belt.Result.flatMap r (fun decoded -> Error (Error.Imandra_error decoded)))
        |> Js.Promise.resolve
    )

module Verify = struct
  let bySrc
      ?(instancePrinter : Api.Request.printer_details option)
      ?(hints : Api.Request.Hints.t option)
      ~(syntax: Api.src_syntax)
      ~(src : string)
      (p : ServerInfo.t)
    : (Api.Response.verify_result, Error.t) Belt.Result.t Js.Promise.t =

    let req : Api.Request.verify_req_src = { syntax; src_base64 = (to_base64 src); instance_printer = instancePrinter; hints } in
    let body = Fetch.BodyInit.make (Decoders_bs.Encode.encode_string E.Request.verify_req_src req) in
    Fetch.fetchWithRequestInit
      (Fetch.Request.make (p.baseUrl ^ "/verify/by-src"))
      (Fetch.RequestInit.make ~method_:Post ~body ())
    |> Js.Promise.then_ (handle_response D.Response.verify_result)

  let byName
      ?(instancePrinter: Api.Request.printer_details option)
      ?(hints: Api.Request.Hints.t option)
      ~(name : string)
      (p : ServerInfo.t)
    : (Api.Response.verify_result, Error.t) Belt.Result.t Js.Promise.t =
    let req : Api.Request.verify_req_name = { name; instance_printer = instancePrinter; hints } in
    let body = Fetch.BodyInit.make (Decoders_bs.Encode.encode_string E.Request.verify_req_name req) in
    Fetch.fetchWithRequestInit
      (Fetch.Request.make (p.baseUrl ^ "/verify/by-name"))
      (Fetch.RequestInit.make ~method_:Post ~body ())
    |> Js.Promise.then_ (handle_response D.Response.verify_result)
end

module Eval = struct
  let bySrc
      ~(syntax: Api.src_syntax)
      ~(src : string)
      (p : ServerInfo.t)
    : (unit, Error.t) Belt.Result.t Js.Promise.t =

    let req : Api.Request.eval_req_src = { syntax; src_base64 = (to_base64 src) } in
    let body = Fetch.BodyInit.make (Decoders_bs.Encode.encode_string E.Request.eval_req_src req) in
    Fetch.fetchWithRequestInit
      (Fetch.Request.make (p.baseUrl ^ "/eval/by-src"))
      (Fetch.RequestInit.make ~method_:Post ~body ())
    |> Js.Promise.then_ (handle_response (Decoders_bs.Decode.succeed ()))
end

module Instance = struct
  let bySrc
      ?(instancePrinter: Api.Request.printer_details option)
      ~(syntax: Api.src_syntax)
      ~(src : string)
      (p : ServerInfo.t)
    : (Api.Response.instance_result, Error.t) Belt.Result.t Js.Promise.t =

    let req : Api.Request.instance_req_src = { instance_printer = instancePrinter; syntax; src_base64 = (to_base64 src) } in
    let body = Fetch.BodyInit.make (Decoders_bs.Encode.encode_string E.Request.instance_req_src req) in
    Fetch.fetchWithRequestInit
      (Fetch.Request.make (p.baseUrl ^ "/instance/by-src"))
      (Fetch.RequestInit.make ~method_:Post ~body ())
    |> Js.Promise.then_ (handle_response D.Response.instance_result)

  let byName
      ?(instancePrinter: Api.Request.printer_details option)
      ~(name : string)
      (p : ServerInfo.t)
    : (Api.Response.instance_result, Error.t) Belt.Result.t Js.Promise.t =

    let req : Api.Request.instance_req_name = { name; instance_printer = instancePrinter } in
    let body = Fetch.BodyInit.make (Decoders_bs.Encode.encode_string E.Request.instance_req_name req) in
    Fetch.fetchWithRequestInit
      (Fetch.Request.make (p.baseUrl ^ "/instance/by-name"))
      (Fetch.RequestInit.make ~method_:Post ~body ())
    |> Js.Promise.then_ (handle_response D.Response.instance_result)
end

let reset (p : ServerInfo.t) : (unit, Error.t) Belt.Result.t Js.Promise.t =
    Fetch.fetchWithRequestInit
      (Fetch.Request.make (p.baseUrl ^ "/reset"))
      (Fetch.RequestInit.make ~method_:Post ())
    |> Js.Promise.then_ (handle_response (Decoders_bs.Decode.succeed ()))

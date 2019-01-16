[%raw "require('isomorphic-fetch')"]
external spawn : string -> string array -> Node.Child_process.spawnResult = "" [@@bs.module "child_process"]
external getPort : unit -> int Js.Promise.t = "getPortPromise" [@@bs.module "portfinder"]

module Api = Api.Types(Decoders_bs.Decode)(Decoders_bs.Encode)

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
    let t t =
      Js.Dict.fromList
        [ ("port", Js.Json.number (float_of_int t.port))
        ; ("baseUrl", Js.Json.string (t.baseUrl))
        ]
      |> Js.Json.object_
  end

  module Decode = struct
    let t json =
      Json.Decode.(
        { port = (field "port" int json)
        ; baseUrl = (field "baseUrl" string json)
        }
      )
  end

  let toFile ?(filename=".imandra-server-info") (t : t) =
    let j_str =
      Encode.t t
      |> Js.Json.stringify
    in
    Node.Fs.writeFileSync filename j_str `utf8

  let fromFile ?(filename=".imandra-server-info") () : t =
    Node.Fs.readFileSync filename `utf8
    |> Js.Json.parseExn
    |> Decode.t

  let cleanup ?(filename=".imandra-server-info") () =
    Node.Fs.unlinkSync filename;

end

module PrinterDetails = struct
end

module Syntax = struct
  type t =
    | OCaml
    | Reason
end

external bufferToStringWithEncoding : Node.Buffer.t ->
  ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary  | `hex ] [@bs.string]) ->
  string = "toString" [@@bs.send]

let to_base64 (s : string) =
  let b = Node.Buffer.fromString s in
  (bufferToStringWithEncoding b `base64)

module Request = struct
  type reqSrc =
    { instancePrinter : PrinterDetails.t option
    ; syntax : Syntax.t
    ; src : string
    }

  type reqName =
    { name : string
    ; instancePrinter : PrinterDetails.t option
    }

  let append_opt_key k f opt xs =
    match opt with
    | None -> xs
    | Some x -> xs @ [(k, f x)]

  module Encode = struct

    let reqSrc (t : reqSrc) : Js.Json.t =
      let b = Node.Buffer.fromString t.src in
      let encodedSrc = (bufferToStringWithEncoding b `base64) in
      Js.Dict.fromList
        ([ ("src_base64", Js.Json.string encodedSrc)
         ; ("syntax", Js.Json.string (match t.syntax with | OCaml -> "ocaml" | Reason -> "reason") )
         ]
         |> append_opt_key "instance_printer" PrinterDetails.Encode.t t.instancePrinter
        )
      |> Js.Json.object_

    let reqName (t : reqName) : Js.Json.t =
      Js.Dict.fromList
        ([ ("name", Js.Json.string t.name)
         ]
         |> append_opt_key "instance_printer" PrinterDetails.Encode.t t.instancePrinter
        )
      |> Js.Json.object_
  end
end

module Response = struct
  type instance =
    { model : Model.t
    ; type_ : string
    ; printed : string option
    }

  module Decode = struct
    let instance json =
      Json.Decode.(
        { model = field "model" Model.Decode.t json
        ; type_ = field "type" string json
        ; printed = field "printed" (optional string) json
        }
      )
  end
end

type 'a with_json =
  ('a * Js.Json.t)

type error = string

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


let errorOr decoder json : ('a with_json, error with_json) Belt.Result.t =
  Json.Decode.(
    match optional (field "error" string) json with
    | Some error ->
      Error (error, json)
    | None ->
      Ok (decoder json, json)
  )

module Verify = struct
  type unknownResult =
    { reason: string }

  type refutedResult =
    { instance: Response.instance }

  type verifyResult =
    | Proved
    | Unknown of unknownResult
    | Refuted of refutedResult

  module Decode = struct
    let verifyResult json =
      Json.Decode.(
        let r = (field "result" string json) in
        match (field "result" string json) with
        | "proved" -> Proved
        | "unknown" -> Unknown { reason = field "unknown_reason" string json }
        | "refuted" -> Refuted { instance = field "instance" Response.Decode.instance json }
        | _ -> failwith (Printf.sprintf "unknown verify result: %s" r)
      )
  end


  let bySrc ?(instancePrinter: PrinterDetails.t option) ~(syntax: Syntax.t) ~(src : string) (p : ServerInfo.t) : (verifyResult with_json, error with_json) Belt.Result.t Js.Promise.t =
    let body = Fetch.BodyInit.make ((Request.Encode.reqSrc { syntax; src ; instancePrinter }) |> Js.Json.stringify) in
    Fetch.fetchWithRequestInit
      (Fetch.Request.make (p.baseUrl ^ "/verify/by-src"))
      (Fetch.RequestInit.make ~method_:Post ~body ())
    |> Js.Promise.then_ (fun res ->
        Fetch.Response.json res
      )
    |> Js.Promise.then_ (fun json ->
        Js.Promise.resolve (errorOr Decode.verifyResult json)
      )

  let byName ?(instancePrinter: PrinterDetails.t option) ~(name : string) (p : ServerInfo.t) : (verifyResult with_json, error with_json) Belt.Result.t Js.Promise.t =
    let body = Fetch.BodyInit.make ((Request.Encode.reqName { name; instancePrinter }) |> Js.Json.stringify) in
    Fetch.fetchWithRequestInit
      (Fetch.Request.make (p.baseUrl ^ "/verify/by-name"))
      (Fetch.RequestInit.make ~method_:Post ~body ())
    |> Js.Promise.then_ (fun res ->
        Fetch.Response.json res
      )
    |> Js.Promise.then_ (fun json ->
        Js.Promise.resolve (errorOr Decode.verifyResult json)
      )
end

module Eval = struct
  module Decode = struct
    let evalResult _json =
      ()
  end

  let bySrc ~(syntax: Syntax.t) ~(src : string) (p : ServerInfo.t) : (unit with_json, error with_json) Belt.Result.t Js.Promise.t =
    let body = Fetch.BodyInit.make ((Request.Encode.reqSrc { syntax; src ; instancePrinter = None }) |> Js.Json.stringify) in
    Fetch.fetchWithRequestInit
      (Fetch.Request.make (p.baseUrl ^ "/eval/by-src"))
      (Fetch.RequestInit.make ~method_:Post ~body ())
    |> Js.Promise.then_ (fun res ->
        Fetch.Response.json res
      )
    |> Js.Promise.then_ (fun json ->
        Js.Promise.resolve (errorOr Decode.evalResult json)
      )
end

module Instance = struct
  type unknownResult =
    { reason: string
    }

  type satResult =
    { instance : Response.instance
    }

  type instanceResult =
    | Sat of satResult
    | Unknown of unknownResult
    | Unsat

  module Decode = struct
    let instanceResult json =
      Json.Decode.(
        let r = (field "result" string json) in
        match (field "result" string json) with
        | "unsat" -> Unsat
        | "unknown" -> Unknown { reason = field "unknown_reason" string json }
        | "sat" -> Sat { instance = field "instance" Response.Decode.instance json }
        | _ -> failwith (Printf.sprintf "unknown verify result: %s" r)
      )
  end

  let bySrc ?(instancePrinter: PrinterDetails.t option) ~(syntax: Syntax.t) ~(src : string) (p : ServerInfo.t) : (instanceResult with_json, error with_json) Belt.Result.t Js.Promise.t =
    let body = Fetch.BodyInit.make ((Request.Encode.reqSrc { syntax; src; instancePrinter }) |> Js.Json.stringify) in
    Fetch.fetchWithRequestInit
      (Fetch.Request.make (p.baseUrl ^ "/instance/by-src"))
      (Fetch.RequestInit.make ~method_:Post ~body ())
    |> Js.Promise.then_ (fun res ->
        Fetch.Response.json res
      )
    |> Js.Promise.then_ (fun json ->
        Js.Promise.resolve (errorOr Decode.instanceResult json)
      )

  let byName ?(instancePrinter: PrinterDetails.t option) ~(name : string) (p : ServerInfo.t) : (instanceResult with_json, error with_json) Belt.Result.t Js.Promise.t =
    let body = Fetch.BodyInit.make ((Request.Encode.reqName { name; instancePrinter }) |> Js.Json.stringify) in
    Fetch.fetchWithRequestInit
      (Fetch.Request.make (p.baseUrl ^ "/instance/by-name"))
      (Fetch.RequestInit.make ~method_:Post ~body ())
    |> Js.Promise.then_ (fun res ->
        Fetch.Response.json res
      )
    |> Js.Promise.then_ (fun json ->
        Js.Promise.resolve (errorOr Decode.instanceResult json)
      )
end

module Decode = struct
  let resetResult _json =
    ()
end

let reset (p : ServerInfo.t) : (unit with_json, error with_json) Belt.Result.t Js.Promise.t =
    Fetch.fetchWithRequestInit
      (Fetch.Request.make (p.baseUrl ^ "/reset"))
      (Fetch.RequestInit.make ~method_:Post ())
    |> Js.Promise.then_ (fun res ->
        Fetch.Response.json res
      )
    |> Js.Promise.then_ (fun json ->
        Js.Promise.resolve (errorOr Decode.resetResult json)
      )

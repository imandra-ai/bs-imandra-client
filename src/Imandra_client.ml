[%raw "require('isomorphic-fetch')"]
external spawn : string -> string array -> Node.Child_process.spawnResult = "" [@@bs.module "child_process"]
external getPort : unit -> int Js.Promise.t = "getPortPromise" [@@bs.module "portfinder"]

external spawnOn
  : Node.Child_process.spawnResult
    -> ([ `exit of int -> unit ] [@bs.string])
    -> Node.Child_process.spawnResult = "on" [@@bs.send]

external spawnOff
  : Node.Child_process.spawnResult
    -> ([ `exit of int -> unit ] [@bs.string])
    -> Node.Child_process.spawnResult = "off" [@@bs.send]

external bufferOn
  : Node.string_buffer
    -> ([ `data of Node.Buffer.t -> unit] [@bs.string])
    -> Node.string_buffer = "on" [@@bs.send]

external bufferOff
  : Node.string_buffer
    -> ([ `data of Node.Buffer.t -> unit] [@bs.string])
    -> Node.string_buffer = "off" [@@bs.send]

type imandraSyntax = Reason | OCaml

type imandraOptions =
  { syntax : string
  ; debug : bool [@bs.optional]
  ; serverCmd : string [@bs.optional]
  } [@@bs.deriving abstract]

type imandraOptionsWithDefaults =
  { syntax : imandraSyntax
  ; debug : bool
  ; serverCmd : string
  }

type imandraProcess =
  { nodeProcess : Node.Child_process.spawnResult
  ; port : int
  ; baseUrl : string
  } [@@bs.deriving abstract]

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
  { syntax = if (opts |. syntaxGet) = "reason" then Reason else OCaml
  ; debug = (match (opts |. debugGet) with | None -> false | Some d -> d)
  ; serverCmd = (match (opts |. serverCmdGet) with | None -> "imandra-http-server" | Some s -> s)
  }

let start (passedOpts : imandraOptions) : imandraProcess Js.Promise.t =

  let opts = withDefaults passedOpts in

  let startupExitOutputCb = ref (fun _ -> ()) in
  let startupExitSpawnCb = ref (fun _ -> ()) in

  let listenForStartupExit (np : Node.Child_process.spawnResult) =
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
         raise (Js.Exn.raiseError (Printf.sprintf "Imandra process exited during startup (code: %d)." code) ));

    (so |. bufferOn (`data !startupExitOutputCb) |> ignore);
    (np |. spawnOn (`exit !startupExitSpawnCb) |> ignore);
  in

  let unlistenForStartupExit (np : Node.Child_process.spawnResult) =
    let props = Node.Child_process.readAs np in
    let so = props##stdout |> Js.Null.getExn in
    so |. bufferOff (`data !startupExitOutputCb) |> ignore;
    np |. spawnOff (`exit !startupExitSpawnCb) |> ignore;
  in

  Js.Promise.make (fun ~resolve ~reject:_ ->
      getPort ()
      |> Js.Promise.then_ (fun port ->
          let syntaxArg = if opts.syntax = Reason then [|"-reason"|] else [||] in
          let args = (Array.append [|"--non-interactive"; "-port"; (string_of_int port)|] syntaxArg) in
          let np = spawn opts.serverCmd args in

          listenForStartupExit np;

          if (opts.debug) then
            printStreamsDebug np
          else
            ();

          waitForServer port
          |> Js.Promise.then_ (fun () ->
              unlistenForStartupExit np;
              resolve (imandraProcess ~nodeProcess:np ~port ~baseUrl:("http://localhost:" ^ (string_of_int port))) [@bs];
              Js.Promise.resolve ();
            )
        )
      |> ignore
    )

external spawnKill : Node.Child_process.spawnResult -> int -> unit = "kill" [@@bs.send]
external spawnUnref : Node.Child_process.spawnResult -> unit -> unit = "unref" [@@bs.send]

let stop (p : imandraProcess) : unit Js.Promise.t =
  Js.Promise.make (fun ~resolve ~reject:_ ->
      let np = (p |. nodeProcessGet) in
      let rec handler = ref (fun _ -> ()) in
      handler := (fun code ->
          np |. spawnOff (`exit !handler) |> ignore;
          np |. spawnUnref () |> ignore;
          resolve code [@bs];
        );
      np |. spawnOn (`exit !handler) |> ignore;
      np |. spawnKill 2 |> ignore;

    )
  |> Js.Promise.then_ (fun _ -> Js.Promise.resolve ())

external bufferToStringWithEncoding : Node.Buffer.t ->
  ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary  | `hex ] [@bs.string]) ->
  string = "toString" [@@bs.send]

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

module Decode = struct
  let modelDecoder json =
    Json.Decode.(
      { language = (field "language" string json)
      ; src = Node.Buffer.fromStringWithEncoding (field "src_base64" string json) `base64 |> Node.Buffer.toString
      }
    )

  let cxDecoder json =
    Json.Decode.(
      { model = field "model" modelDecoder json
      }
    )

  let verifyResult json =
    Json.Decode.(
      let r = (field "result" string json) in
      match (field "result" string json) with
      | "proved" -> Proved
      | "unknown" -> Unknown { unknownReason = field "unknown_reason" string json }
      | "refuted" -> Refuted { refutedCounterexample = field "refuted_counterexample" cxDecoder json }
      | _ -> failwith (Printf.sprintf "unknown verify result: %s" r)
    )
end

let verify (p : imandraProcess) ~(src : string) : Js.Json.t Js.Promise.t =
  let b = Node.Buffer.fromString src in
  let encodedSrc = (bufferToStringWithEncoding b `base64) in
  let req = "{ \"src_base64\": \"" ^ encodedSrc ^ "\" }" in
  let body = Fetch.BodyInit.make req in
  Fetch.fetchWithRequestInit
    (Fetch.Request.make ((p |. baseUrlGet) ^ "/verify/by-src"))
    (Fetch.RequestInit.make ~method_:Post ~body ())
  |> Js.Promise.then_ (fun res ->
      Fetch.Response.json res
    )

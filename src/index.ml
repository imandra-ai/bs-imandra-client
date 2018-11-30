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

let baseUrl (port : int) =
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
        Fetch.fetch (Printf.sprintf "%s/status" (baseUrl port))
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

  let startupExitStderrCb = ref (fun _ -> ()) in
  let startupExitSpawnCb = ref (fun _ -> ()) in

  let listenForStartupExit (np : Node.Child_process.spawnResult) =
    let props = Node.Child_process.readAs np in
    let se = props##stderr |> Js.Null.getExn in
    let seText = ref "" in
    startupExitStderrCb := (fun b ->
           let s = Node.Buffer.toString b in
           seText := Js.String.concat !seText s;
         );


    startupExitSpawnCb := (fun code ->
      Js.Console.error !seText;
      raise (Js.Exn.raiseError (Printf.sprintf "Imandra process exited during startup (code: %d)." code) ));

    (se |. bufferOn (`data !startupExitStderrCb) |> ignore);
    (np |. spawnOn (`exit !startupExitSpawnCb) |> ignore);
  in

  let unlistenForStartupExit (np : Node.Child_process.spawnResult) =
    let props = Node.Child_process.readAs np in
    let se = props##stderr |> Js.Null.getExn in
    se |. bufferOff (`data !startupExitStderrCb) |> ignore;
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
              resolve (imandraProcess ~nodeProcess:np) [@bs];
              Js.Promise.resolve ();
            )
        )
      |> ignore
    )

external spawnKill
  : Node.Child_process.spawnResult
    -> string
    -> unit = "kill" [@@bs.send]

let stop (p : imandraProcess) : unit Js.Promise.t =
  Js.Promise.make (fun ~resolve ~reject:_ ->
      let np = (p |. nodeProcessGet) in
      np |. spawnKill "SIGKILL";
      np |. spawnOn (`exit (fun code ->
          resolve code [@bs]
          ;
        ))
      |> ignore

    )
  |> Js.Promise.then_ (fun _ -> Js.Promise.resolve ())


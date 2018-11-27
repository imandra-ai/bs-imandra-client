external spawn : string -> string array -> Node.Child_process.spawnResult = "" [@@bs.module "child_process"]

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

type imandraSyntax = Reason | OCaml

type imandraOptions =
  { syntax : imandraSyntax
  ; debug : bool
  } [@@bs.deriving abstract]

type imandraProcess =
  { nodeProcess : Node.Child_process.spawnResult
  } [@@bs.deriving abstract]

let waitForPrompt (process : imandraProcess) (promptLine : string) : unit Js.Promise.t =
  Js.Promise.make (fun ~resolve ~reject:_ ->
      let np = Node.Child_process.readAs (process |. nodeProcessGet) in
      let so = np##stdout |> Js.Null.getExn in
      let rec handleStdout b =
        let s = Node.Buffer.toString b in
        let lines = s |> Js.String.split "\n" in
        if (lines |> Js.Array.includes promptLine) then
          begin
            ignore (so |. bufferOff (`data handleStdout));
            resolve "-" [@bs]
          end
        else
          ();
      in
      ignore (so |. bufferOn (`data handleStdout))
    )
  |> Js.Promise.then_ (fun _ -> Js.Promise.resolve ())

let printStreamsDebug (p : imandraProcess) =
  let np = (p |. nodeProcessGet) in
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


let start (opts : imandraOptions) : imandraProcess Js.Promise.t =

  let makeHandleCloseDuringStart (p : imandraProcess) =
    let np = (p |. nodeProcessGet) in
    let props = Node.Child_process.readAs np in
    let se = props##stderr |> Js.Null.getExn in
    let seText = ref "" in
    ignore
      (se |. bufferOn (`data (fun b ->
           let s = Node.Buffer.toString b in
           seText := Js.String.concat !seText s;
         )));
    fun code ->
      Js.Console.error !seText;
      raise (Js.Exn.raiseError (Printf.sprintf "Imandra process exited during startup (code: %d)." code) )
  in

  Js.Promise.make (fun ~resolve ~reject:_ ->
      let np = spawn "imandra-repl-dev" [|"--non-interactive"; "-raw"; "-require"; "cohttp.lwt"|] in
      let ip = imandraProcess ~nodeProcess:np in
      let handleCloseDuringStart = makeHandleCloseDuringStart ip in

      ignore (np |. spawnOn (`close handleCloseDuringStart));

      if (opts |. debugGet) then
        printStreamsDebug ip
      else
        ();

      ignore
        (waitForPrompt ip "# "
         |> Js.Promise.then_
            (fun _ ->
               ignore (np |. spawnOff (`close handleCloseDuringStart));
               resolve ip [@bs];
               Js.Promise.resolve ();
            )
        ))

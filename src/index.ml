external spawn : string -> string list -> Node.Child_process.spawnResult = "" [@@bs.module "child_process"]

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
  < syntax : imandraSyntax
  ; debug : bool
  > Js.t

type imandraProcess =
  { nodeProcess : Node.Child_process.spawnResult
  }

let waitForPrompt (process : imandraProcess) (promptLine : string) : unit Js.Promise.t =
  Js.Promise.make (fun ~resolve ~reject:_ ->
      let np = Node.Child_process.readAs process.nodeProcess in
      let so = np##stdout |> Js.Null.getExn in
      let rec handleStdout b =
        let s = Node.Buffer.toString b in
        let lines = s |. Js.String.split "\n" in
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

let start (opts : imandraOptions) : imandraProcess Js.Promise.t =

  let handleCloseDuringStart code =
    if code = 3 then
      raise (Js.Exn.raiseError "imandra-repl requires login. Start imandra-repl manually to login." )
  in

  Js.Promise.make (fun ~resolve ~reject:_ ->
      let np = spawn "imandra-repl-dev" ["-require"; "cohttp.lwt"] in
      let props = Node.Child_process.readAs np in
      let ip = { nodeProcess = np } in

      ignore (np |. spawnOn (`close handleCloseDuringStart));

      if opts##debug then
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
              )));
      else
        ();

      ignore
        (Js.Promise.then_
           (fun _ ->
              ignore (np |. spawnOff (`close handleCloseDuringStart));
              resolve { nodeProcess = np } [@bs];
              Js.Promise.resolve ();
           )
           (waitForPrompt ip "# "))
    )

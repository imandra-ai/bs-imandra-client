[%raw "require('isomorphic-fetch')"]
external spawn : string -> string array -> Node.Child_process.spawnResult = "" [@@bs.module "child_process"]
external get_port : unit -> int Js.Promise.t = "getPortPromise" [@@bs.module "portfinder"]

module Api = Imandra_client_api

module D = Api.Decoders(Decoders_bs.Decode)
module E = Api.Encoders(Decoders_bs.Encode)

let function_name = [%raw fun f -> "{return f.name}"]

external spawn_on
  : Node.Child_process.spawnResult
    -> ([ `close of int -> unit ] [@bs.string])
    -> Node.Child_process.spawnResult = "on" [@@bs.send]

external spawn_off
  : Node.Child_process.spawnResult
    -> ([ `close of int -> unit ] [@bs.string])
    -> Node.Child_process.spawnResult = "off" [@@bs.send]

external buffer_on
  : Node.string_buffer
    -> ([ `data of Node.Buffer.t -> unit] [@bs.string])
    -> Node.string_buffer = "on" [@@bs.send]

external buffer_off
  : Node.string_buffer
    -> ([ `data of Node.Buffer.t -> unit] [@bs.string])
    -> Node.string_buffer = "off" [@@bs.send]

type imandra_options =
  { debug : bool [@bs.optional]
  ; server_cmd : string [@bs.optional]
  } [@@bs.deriving abstract]

type imandraOptionsWithDefaults =
  { debug : bool
  ; server_cmd : string
  }

module Server_info = struct
  type t =
    { port : int
    ; base_url : string
    }

  module Encode = struct
    open Decoders_bs.Encode
    let t t =
      obj [ ("port", int t.port)
          ; ("base_url", string t.base_url)]
  end

  module Decode = struct
    open Decoders_bs.Decode
    let t : t decoder =
      field "port" int >>= fun port ->
      field "base_url" string >>= fun base_url ->
      succeed { port; base_url }
  end

  let to_file ?(filename=".imandra-server-info") (t : t) =
    let j_str = Decoders_bs.Encode.encode_string Encode.t t in
    Node.Fs.writeFileSync filename j_str `utf8

  let from_file ?(filename=".imandra-server-info") () =
    Node.Fs.readFileSync filename `utf8
    |> Decoders_bs.Decode.decode_string Decode.t
    |> Decoders_util.My_result.map_err (Format.asprintf "%a" Decoders_bs.Decode.pp_error)

  let cleanup ?(filename=".imandra-server-info") () =
    Node.Fs.unlinkSync filename;
end

external buffer_to_string_with_encoding : Node.Buffer.t ->
  ([ `ascii  | `utf8  | `utf16le  | `usc2  | `base64  | `latin1 | `binary  | `hex ] [@bs.string]) ->
  string = "toString" [@@bs.send]

let to_base64 (s : string) =
  let b = Node.Buffer.fromString s in
  (buffer_to_string_with_encoding b `base64)

let from_base64 (s : string) =
  Node.Buffer.fromStringWithEncoding s `base64
  |> Node.Buffer.toString

module Error = struct
  type t =
    | Decoder_error of Decoders_bs.Decode.error
    | Imandra_error of Api.Response.error

  let pp fmt = function
    | Decoder_error e -> Format.fprintf fmt "Decoder_error: %a" Decoders_bs.Decode.pp_error e
    | Imandra_error e -> Format.fprintf fmt "Imandra_error: %s" e.error

  let pp_str e =
    Format.asprintf "%a" pp e
end

let print_streams_debug (np : Node.Child_process.spawnResult) =
  let props = Node.Child_process.readAs np in
  let so = props##stdout |> Js.Null.getExn in
  let se = props##stderr |> Js.Null.getExn in

  ignore
    (so |. buffer_on (`data (fun b ->
         let s = Node.Buffer.toString b in
         Js.Console.log (Printf.sprintf "STDOUT: %s" s)
       )));

  ignore
    ( se |. buffer_on (`data (fun b ->
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

let wait_for_server (port : int) : unit Js.Promise.t =
  Js.Promise.make (fun ~resolve ~reject:_ ->
      let rec check_status () =
        Fetch.fetch (Printf.sprintf "%s/status" (makeBaseUrl port))
        |> Js.Promise.then_ (fun res ->
            resolve res [@bs];
            Js.Promise.resolve ();
          )
        |> Js.Promise.catch (fun _ ->
            (timeout 1000)
            |> Js.Promise.then_ check_status
          )

      in ignore (check_status ())
    )
  |> Js.Promise.then_ (fun _ ->
      Js.Promise.resolve ()
    )

let with_defaults (opts : imandra_options) : imandraOptionsWithDefaults =
  { debug = (match (opts |. debugGet) with | None -> false | Some d -> d)
  ; server_cmd = (match (opts |. server_cmdGet) with | None -> "imandra-http-server" | Some s -> s)
  }

let start (passed_opts : imandra_options) : (Node.Child_process.spawnResult * Server_info.t) Js.Promise.t =

  let opts = with_defaults passed_opts in

  let startup_exit_output_cb = ref (fun _ -> ()) in
  let startup_exit_spawn_cb = ref (fun _ -> ()) in

  Js.Promise.make (fun ~resolve ~reject ->
      let listen_for_startup_close (np : Node.Child_process.spawnResult) =
        let props = Node.Child_process.readAs np in
        let so = props##stdout |> Js.Null.getExn in
        let seText = ref "" in
        startup_exit_output_cb :=
          (fun b ->
             let s = Node.Buffer.toString b in
             seText := Js.String.concat !seText s;
          );

        startup_exit_spawn_cb :=
          (fun code ->
             Js.Console.error !seText;
             reject (Failure (Printf.sprintf "Imandra process exited during startup (code: %d)." code)) [@bs]);

        (so |. buffer_on (`data !startup_exit_output_cb) |> ignore);
        (np |. spawn_on (`close !startup_exit_spawn_cb) |> ignore);
      in

      let unlisten_for_startup_close (np : Node.Child_process.spawnResult) =
        let props = Node.Child_process.readAs np in
        let so = props##stdout |> Js.Null.getExn in
        so |. buffer_off (`data !startup_exit_output_cb) |> ignore;
        np |. spawn_off (`close !startup_exit_spawn_cb) |> ignore;
      in

      get_port ()
      |> Js.Promise.then_ (fun port ->
          (* Always set reason to load the reason parser. Syntax is specified per-call *)
          let args = [|"--non-interactive"; "-reason"; "-port"; (string_of_int port)|] in
          let np = spawn opts.server_cmd args in

          listen_for_startup_close np;

          if (opts.debug) then
            print_streams_debug np
          else
            ();

          wait_for_server port
          |> Js.Promise.then_ (fun () ->
              unlisten_for_startup_close np;
              resolve (np, Server_info.{ port = port;  base_url=("http://localhost:" ^ (string_of_int port)) }) [@bs];
              Js.Promise.resolve ();
            )
        )
      |> ignore
    )

external spawn_kill : Node.Child_process.spawnResult -> int -> unit = "kill" [@@bs.send]

let stop (np : Node.Child_process.spawnResult) : unit Js.Promise.t =
  Js.Promise.make (fun ~resolve ~reject:_ ->
      let handler = ref (fun _ -> ()) in
      handler := (fun code ->
          np |. spawn_off (`close !handler) |> ignore;
          resolve code [@bs];
        );
      np |. spawn_on (`close !handler) |> ignore;
      np |. spawn_kill 2 |> ignore;
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
  let by_src
      ?(instance_printer : Api.Request.printer_details option)
      ?(hints : Api.Request.Hints.t option)
      ~(syntax: Api.src_syntax)
      ~(src : string)
      (p : Server_info.t)
    : (Api.Response.verify_result, Error.t) Belt.Result.t Js.Promise.t =

    let req : Api.Request.verify_req_src = { syntax; src_base64 = (to_base64 src); instance_printer = instance_printer; hints } in
    let body = Fetch.BodyInit.make (Decoders_bs.Encode.encode_string E.Request.verify_req_src req) in
    Fetch.fetchWithRequestInit
      (Fetch.Request.make (p.base_url ^ "/verify/by-src"))
      (Fetch.RequestInit.make ~method_:Post ~body ())
    |> Js.Promise.then_ (handle_response D.Response.verify_result)

  let by_name
      ?(instance_printer: Api.Request.printer_details option)
      ?(hints: Api.Request.Hints.t option)
      ~(name : string)
      (p : Server_info.t)
    : (Api.Response.verify_result, Error.t) Belt.Result.t Js.Promise.t =
    let req : Api.Request.verify_req_name = { name; instance_printer = instance_printer; hints } in
    let body = Fetch.BodyInit.make (Decoders_bs.Encode.encode_string E.Request.verify_req_name req) in
    Fetch.fetchWithRequestInit
      (Fetch.Request.make (p.base_url ^ "/verify/by-name"))
      (Fetch.RequestInit.make ~method_:Post ~body ())
    |> Js.Promise.then_ (handle_response D.Response.verify_result)
end

module Eval = struct
  let by_src
      ~(syntax: Api.src_syntax)
      ~(src : string)
      (p : Server_info.t)
    : (unit, Error.t) Belt.Result.t Js.Promise.t =

    let req : Api.Request.eval_req_src = { syntax; src_base64 = (to_base64 src) } in
    let body = Fetch.BodyInit.make (Decoders_bs.Encode.encode_string E.Request.eval_req_src req) in
    Fetch.fetchWithRequestInit
      (Fetch.Request.make (p.base_url ^ "/eval/by-src"))
      (Fetch.RequestInit.make ~method_:Post ~body ())
    |> Js.Promise.then_ (handle_response (Decoders_bs.Decode.succeed ()))
end

module Instance = struct
  let by_src
      ?(instance_printer: Api.Request.printer_details option)
      ~(syntax: Api.src_syntax)
      ~(src : string)
      (p : Server_info.t)
    : (Api.Response.instance_result, Error.t) Belt.Result.t Js.Promise.t =

    let req : Api.Request.instance_req_src = { instance_printer = instance_printer; syntax; src_base64 = (to_base64 src) } in
    let body = Fetch.BodyInit.make (Decoders_bs.Encode.encode_string E.Request.instance_req_src req) in
    Fetch.fetchWithRequestInit
      (Fetch.Request.make (p.base_url ^ "/instance/by-src"))
      (Fetch.RequestInit.make ~method_:Post ~body ())
    |> Js.Promise.then_ (handle_response D.Response.instance_result)

  let by_name
      ?(instance_printer: Api.Request.printer_details option)
      ~(name : string)
      (p : Server_info.t)
    : (Api.Response.instance_result, Error.t) Belt.Result.t Js.Promise.t =

    let req : Api.Request.instance_req_name = { name; instance_printer = instance_printer } in
    let body = Fetch.BodyInit.make (Decoders_bs.Encode.encode_string E.Request.instance_req_name req) in
    Fetch.fetchWithRequestInit
      (Fetch.Request.make (p.base_url ^ "/instance/by-name"))
      (Fetch.RequestInit.make ~method_:Post ~body ())
    |> Js.Promise.then_ (handle_response D.Response.instance_result)
end

let reset (p : Server_info.t) : (unit, Error.t) Belt.Result.t Js.Promise.t =
    Fetch.fetchWithRequestInit
      (Fetch.Request.make (p.base_url ^ "/reset"))
      (Fetch.RequestInit.make ~method_:Post ())
    |> Js.Promise.then_ (handle_response (Decoders_bs.Decode.succeed ()))

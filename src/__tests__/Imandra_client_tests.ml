open Jest

let running_node_process = ref None
let running_imandra_server_info = ref None

module Api = Imandra_client.Api

let syntax = Api.Iml

let () =
  beforeAllPromise ~timeout:20000 (fun () ->
      let open Imandra_client in
      Imandra_client.start
        (imandra_options ~server_cmd:"imandra-http-server"
           ~debug:true ())
      |> Js.Promise.then_ (fun (np, isi) ->
          running_node_process := Some np;
          (* check serialisation works *)
          Imandra_client.Server_info.to_file isi;
          let from_disk = Imandra_client.Server_info.from_file () in
          running_imandra_server_info := Some from_disk;
          Js.Promise.resolve ()
        )
      |> Js.Promise.catch (fun e ->
          Js.Console.error e;
          Js.Promise.reject (Failure "Problem spawning Imandra process");
        )
    )

let () =
  testPromise ~timeout:20000 "verify refuted" (fun () ->
      let ip = !running_imandra_server_info |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Verify.by_src ip ~syntax ~src:"fun x -> x = 3"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok (Api.Response.V_refuted _) -> Js.Promise.resolve pass
          | Belt.Result.Ok _ -> Js.Promise.resolve (fail "wrong verify result")
          | Belt.Result.Error e -> Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
        )
    )

let () =
  testPromise ~timeout:20000 "verify by name proved" (fun () ->
      let ip = !running_imandra_server_info |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Eval.by_src ip ~syntax ~src:"let rev_rev x = 3 = 3"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok _ ->
            Imandra_client.Verify.by_name ip ~name:"rev_rev"
            |> Js.Promise.then_ (function
                | Belt.Result.Ok (Api.Response.V_proved) -> Js.Promise.resolve pass
                | Belt.Result.Ok _ -> Js.Promise.resolve (fail "wrong verify result")
                | Belt.Result.Error e -> Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
              )
          | Belt.Result.Error e -> Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
        )
    )

let () =
  testPromise ~timeout:20000 "instance" (fun () ->
      let ip = !running_imandra_server_info |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Instance.by_src ip ~syntax ~src:"fun x -> List.length x > 4"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok (Api.Response.I_sat _) -> Js.Promise.resolve pass
          | Belt.Result.Ok _ -> Js.Promise.resolve (fail "instance result not satisifed")
          | Belt.Result.Error e -> Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
        )
    )

let () =
  testPromise ~timeout:20000 "eval failure" (fun () ->
      let ip = !running_imandra_server_info |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Eval.by_src ip ~syntax ~src:"garbage"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok _ -> Js.Promise.resolve (fail "unexpected success")
          | Belt.Result.Error (Imandra_client.Error.Imandra_error e) ->
            Js.Promise.resolve (Expect.toEqual e.error (`Just "Unbound value garbage"))
          | Belt.Result.Error e -> Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
        )
    )

let () =
  testPromise ~timeout:20000 "eval failure for mod_use" (fun () ->
      let ip = !running_imandra_server_info |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Eval.by_src ip ~syntax ~src:"#mod_use \"lol_no_file.iml\""
      |> Js.Promise.then_ (function
          | Belt.Result.Ok _ -> Js.Promise.resolve (fail "unexpected success")
          | Belt.Result.Error (Imandra_client.Error.Imandra_error e) ->
            Js.Promise.resolve (Expect.toContainString "Cannot find file" (`Just e.error))
          | Belt.Result.Error e -> Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
        )
    )

let () =
  testPromise ~timeout:20000 "eval reason" (fun () ->
      let ip = !running_imandra_server_info |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Eval.by_src ip ~syntax:Reason ~src:"let myfn = (x) => x == 3;"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok _ ->
            Js.Promise.resolve (pass)
          | Belt.Result.Error e -> Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
        )
    )

let () =
  testPromise ~timeout:20000 "verify reason" (fun () ->
      let ip = !running_imandra_server_info |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Eval.by_src ip ~syntax:Reason ~src:"(x) => x == 3;"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok () ->
            Js.Promise.resolve (pass)
          | Belt.Result.Error e -> Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
        )
    )

let () =
  testPromise ~timeout:20000 "verify ocaml again" (fun () ->
      let ip = !running_imandra_server_info |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Verify.by_src ip ~syntax ~src:"fun x -> x = 3"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok (Api.Response.V_refuted _) ->
            Js.Promise.resolve (pass)
          | Belt.Result.Ok _ ->
            Js.Promise.resolve (fail "not refuted")
          | Belt.Result.Error e ->
            Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
        )
    )

let () =
  let src = {| type t = { x : Z.t };; let print_t a = (Printf.sprintf "x is %s" (Z.to_string a.x)) [@@program] |} in
  testPromise ~timeout:20000 "print instance" (fun () ->
      let ip = !running_imandra_server_info |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Eval.by_src ip ~syntax ~src
      |> Js.Promise.then_ (function
          | Belt.Result.Ok () ->
            let instance_printer = Api.Request.{ name = "print_t"; cx_var_name = "a" } in
            print_endline instance_printer.name;
            Imandra_client.Instance.by_src ip ~instance_printer ~syntax ~src:"fun a -> a.x + 97 = 100"
            |> Js.Promise.then_ (function
                | Belt.Result.Ok (Api.Response.I_sat { instance }) ->
                  Js.Promise.resolve (Expect.toEqual (Some "x is 3") (`Just instance.printed ))
                | Belt.Result.Ok _ -> Js.Promise.resolve (fail "instance result not satisifed")
                | Belt.Result.Error e -> Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
              )
          | Belt.Result.Error e -> Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
        )
    )

let () =
  testPromise ~timeout:20000 "reset" (fun () ->
      let ip = !running_imandra_server_info |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Eval.by_src ip ~syntax ~src:"let to_be_reset x = x = 3"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok () ->
            Imandra_client.Verify.by_name ip ~name:"to_be_reset"
            |> Js.Promise.then_ (function
                | Belt.Result.Ok _ ->
                  Imandra_client.reset ip
                  |> Js.Promise.then_ (function
                      | Belt.Result.Ok _ ->
                        Imandra_client.Verify.by_name ip ~name:"to_be_reset"
                        |> Js.Promise.then_ (function
                            | Belt.Result.Error (Imandra_client.Error.Imandra_error e) ->
                              Js.Promise.resolve (Expect.toContainString "Unknown verification goal" (`Just e.error))
                            | _ ->
                              Js.Promise.resolve (fail "unexpected result")
                          )
                      | Belt.Result.Error e -> Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
                    )
                | Belt.Result.Error e -> Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
              )
          | Belt.Result.Error e -> Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
        )
    )

let () =
  afterAllAsync ~timeout:20000 (fun finish ->
      match !running_node_process with
      | Some _np ->
        let ip = !running_imandra_server_info |> Belt.Option.getExn |> Belt.Result.getExn in
        Imandra_client.shutdown ip
        |> Js.Promise.then_ (fun _ ->
            running_node_process := None;
            running_imandra_server_info := None;
            Imandra_client.Server_info.cleanup ();
            finish ();
            Js.Promise.resolve ()
          )
        |> ignore

      | None ->
        (* assume failure during startup *)
        finish ();
    )

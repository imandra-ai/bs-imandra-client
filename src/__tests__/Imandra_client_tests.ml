open Jest

let runningNodeProcess = ref None
let runningImandraServerInfo = ref None

module Api = Imandra_client.Api

let syntax = Api.OCaml

let () =
  beforeAllPromise ~timeout:20000 (fun () ->
      let open Imandra_client in
      Imandra_client.start
        (imandraOptions
           ~debug:true ())
      |> Js.Promise.then_ (fun (np, isi) ->
          runningNodeProcess := Some np;
          (* check serialisation works *)
          Imandra_client.ServerInfo.toFile isi;
          let from_disk = Imandra_client.ServerInfo.fromFile () in
          runningImandraServerInfo := Some from_disk;
          Js.Promise.resolve ()
        )
      |> Js.Promise.catch (fun e ->
          Js.Console.error e;
          Js.Promise.reject (Failure "Problem spawning Imandra process");
        )
    )

let () =
  testPromise ~timeout:20000 "verify refuted" (fun () ->
      let ip = !runningImandraServerInfo |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Verify.bySrc ip ~syntax ~src:"fun x -> x = 3"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok (Api.Response.V_refuted _) -> Js.Promise.resolve pass
          | Belt.Result.Ok _ -> Js.Promise.resolve (fail "wrong verify result")
          | Belt.Result.Error e -> Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
        )
    )

let () =
  testPromise ~timeout:20000 "verify by name proved" (fun () ->
      let ip = !runningImandraServerInfo |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Eval.bySrc ip ~syntax ~src:"let rev_rev x = 3 = 3"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok _ ->
            Imandra_client.Verify.byName ip ~name:"rev_rev"
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
      let ip = !runningImandraServerInfo |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Instance.bySrc ip ~syntax ~src:"fun x -> List.length x > 4"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok (Api.Response.I_sat _) -> Js.Promise.resolve pass
          | Belt.Result.Ok _ -> Js.Promise.resolve (fail "instance result not satisifed")
          | Belt.Result.Error e -> Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
        )
    )

let () =
  testPromise ~timeout:20000 "eval failure" (fun () ->
      let ip = !runningImandraServerInfo |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Eval.bySrc ip ~syntax ~src:"garbage"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok _ -> Js.Promise.resolve (fail "unexpected success")
          | Belt.Result.Error (Imandra_client.Error.Imandra_error e) ->
            Js.Promise.resolve (Expect.toEqual e.error (`Just "Unbound value garbage"))
          | Belt.Result.Error e -> Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
        )
    )

let () =
  testPromise ~timeout:20000 "eval failure for mod_use" (fun () ->
      let ip = !runningImandraServerInfo |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Eval.bySrc ip ~syntax ~src:"#mod_use \"lol_no_file.iml\""
      |> Js.Promise.then_ (function
          | Belt.Result.Ok _ -> Js.Promise.resolve (fail "unexpected success")
          | Belt.Result.Error (Imandra_client.Error.Imandra_error e) ->
            Js.Promise.resolve (Expect.toContainString "Cannot find file" (`Just e.error))
          | Belt.Result.Error e -> Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
        )
    )

let () =
  testPromise ~timeout:20000 "eval reason" (fun () ->
      let ip = !runningImandraServerInfo |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Eval.bySrc ip ~syntax:Reason ~src:"let myfn = (x) => x == 3;"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok _ ->
            Js.Promise.resolve (pass)
          | Belt.Result.Error e -> Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
        )
    )

let () =
  testPromise ~timeout:20000 "verify reason" (fun () ->
      let ip = !runningImandraServerInfo |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Eval.bySrc ip ~syntax:Reason ~src:"(x) => x == 3;"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok () ->
            Js.Promise.resolve (pass)
          | Belt.Result.Error e -> Js.Promise.resolve (fail (Format.asprintf "%a" Imandra_client.Error.pp e))
        )
    )

let () =
  testPromise ~timeout:20000 "verify ocaml again" (fun () ->
      let ip = !runningImandraServerInfo |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Verify.bySrc ip ~syntax ~src:"fun x -> x = 3"
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
      let ip = !runningImandraServerInfo |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Eval.bySrc ip ~syntax ~src
      |> Js.Promise.then_ (function
          | Belt.Result.Ok () ->
            let instancePrinter = Api.Request.{ name = "print_t"; cx_var_name = "a" } in
            print_endline instancePrinter.name;
            Imandra_client.Instance.bySrc ip ~instancePrinter ~syntax ~src:"fun a -> a.x + 97 = 100"
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
      let ip = !runningImandraServerInfo |> Belt.Option.getExn |> Belt.Result.getExn in
      Imandra_client.Eval.bySrc ip ~syntax ~src:"let to_be_reset x = x = 3"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok () ->
            Imandra_client.Verify.byName ip ~name:"to_be_reset"
            |> Js.Promise.then_ (function
                | Belt.Result.Ok _ ->
                  Imandra_client.reset ip
                  |> Js.Promise.then_ (function
                      | Belt.Result.Ok _ ->
                        Imandra_client.Verify.byName ip ~name:"to_be_reset"
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
      match !runningNodeProcess with
      | Some np ->
        Imandra_client.stop np
        |> Js.Promise.then_ (fun _ ->
            runningNodeProcess := None;
            runningImandraServerInfo := None;
            Imandra_client.ServerInfo.cleanup ();
            finish ();
            Js.Promise.resolve ()
          )
        |> ignore

      | None ->
        (* assume failure during startup *)
        finish ();
    )

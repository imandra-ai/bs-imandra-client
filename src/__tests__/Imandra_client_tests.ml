open Jest

let runningNodeProcess = ref None
let runningImandraServerInfo = ref None

let syntax = Imandra_client.Syntax.OCaml

let () =
  beforeAllPromise ~timeout:20000 (fun () ->
      let open Imandra_client in
      Imandra_client.start
        (imandraOptions
           ~serverCmd:"imandra-http-server-dev"
           ~debug:true ())
      |> Js.Promise.then_ (fun (np, isi) ->
          runningNodeProcess := Some np;
          (* check serialisation works *)
          Imandra_client.ServerInfo.to_file isi;
          let from_disk = Imandra_client.ServerInfo.from_file () in
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
      let ip = !runningImandraServerInfo |> Belt.Option.getExn in
      Imandra_client.Verify.by_src ip ~syntax ~src:"fun x -> x = 3"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok (Imandra_client.Verify.Refuted _, _) -> Js.Promise.resolve pass
          | Belt.Result.Ok _ -> Js.Promise.resolve (fail "wrong verify result")
          | Belt.Result.Error (e, _) -> Js.Promise.resolve (fail (Printf.sprintf "error from imandra: %s" e))
        )
    )

let () =
  testPromise ~timeout:20000 "verify by name proved" (fun () ->
      let ip = !runningImandraServerInfo |> Belt.Option.getExn in
      Imandra_client.Eval.by_src ip ~syntax ~src:"let rev_rev x = 3 = 3"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok _ ->
            Imandra_client.Verify.by_name ip ~name:"rev_rev"
            |> Js.Promise.then_ (function
                | Belt.Result.Ok (Imandra_client.Verify.Proved, _) -> Js.Promise.resolve pass
                | Belt.Result.Ok _ -> Js.Promise.resolve (fail "wrong verify result")
                | Belt.Result.Error (e, _) -> Js.Promise.resolve (fail (Printf.sprintf "error from imandra: %s" e))
              )
          | Belt.Result.Error (e, _) -> Js.Promise.resolve (fail (Printf.sprintf "error from imandra: %s" e))
        )
    )

let () =
  testPromise ~timeout:20000 "instance" (fun () ->
      let ip = !runningImandraServerInfo |> Belt.Option.getExn in
      Imandra_client.Instance.by_src ip ~syntax ~src:"fun x -> List.length x > 4"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok (Imandra_client.Instance.Sat _, _) -> Js.Promise.resolve pass
          | Belt.Result.Ok (_, _) -> Js.Promise.resolve (fail "instance result not satisifed")
          | Belt.Result.Error (e, _) -> Js.Promise.resolve (fail (Printf.sprintf "error from imandra: %s" e))
        )
    )

let () =
  testPromise ~timeout:20000 "eval failure" (fun () ->
      let ip = !runningImandraServerInfo |> Belt.Option.getExn in
      Imandra_client.Eval.by_src ip ~syntax ~src:"garbage"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok (_, _) -> Js.Promise.resolve (fail "unexpected success")
          | Belt.Result.Error (e, _) ->
            Js.Promise.resolve (Expect.toEqual e (`Just "Unbound value garbage"))
        )
    )

let () =
  testPromise ~timeout:20000 "eval failure for mod_use" (fun () ->
      let ip = !runningImandraServerInfo |> Belt.Option.getExn in
      Imandra_client.Eval.by_src ip ~syntax ~src:"#mod_use \"lol_no_file.iml\""
      |> Js.Promise.then_ (function
          | Belt.Result.Ok (_, _) -> Js.Promise.resolve (fail "unexpected success")
          | Belt.Result.Error (e, _j) ->
            Js.Promise.resolve (Expect.toContainString "Cannot find file" (`Just e))
        )
    )

let () =
  testPromise ~timeout:20000 "eval reason" (fun () ->
      let ip = !runningImandraServerInfo |> Belt.Option.getExn in
      Imandra_client.Eval.by_src ip ~syntax:Reason ~src:"let myfn = (x) => x == 3;"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok (_, _) ->
            Js.Promise.resolve (pass)
          | Belt.Result.Error (e, _j) ->
            Js.Promise.resolve (fail (Printf.sprintf "error from imandra: %s" e))
        )
    )

let () =
  testPromise ~timeout:20000 "verify reason" (fun () ->
      let ip = !runningImandraServerInfo |> Belt.Option.getExn in
      Imandra_client.Eval.by_src ip ~syntax:Reason ~src:"(x) => x == 3;"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok (_, _) ->
            Js.Promise.resolve (pass)
          | Belt.Result.Error (e, _j) ->
            Js.Promise.resolve (fail (Printf.sprintf "error from imandra: %s" e))
        )
    )

let () =
  testPromise ~timeout:20000 "verify ocaml again" (fun () ->
      let ip = !runningImandraServerInfo |> Belt.Option.getExn in
      Imandra_client.Verify.by_src ip ~syntax ~src:"fun x -> x = 3"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok (_, _) ->
            Js.Promise.resolve (pass)
          | Belt.Result.Error (e, _j) ->
            Js.Promise.resolve (fail (Printf.sprintf "error from imandra: %s" e))
        )
    )


let () =
  testPromise ~timeout:20000 "reset" (fun () ->
      let ip = !runningImandraServerInfo |> Belt.Option.getExn in
      Imandra_client.Eval.by_src ip ~syntax ~src:"let to_be_reset x = x = 3"
      |> Js.Promise.then_ (function
          | Belt.Result.Ok (_, _) ->
            Imandra_client.Verify.by_name ip ~name:"to_be_reset"
            |> Js.Promise.then_ (function
                | Belt.Result.Ok (_, _) ->
                  Imandra_client.reset ip
                  |> Js.Promise.then_ (function
                      | Belt.Result.Ok (_, _) ->
                        Imandra_client.Verify.by_name ip ~name:"to_be_reset"
                        |> Js.Promise.then_ (function
                            | Belt.Result.Error (e, _j) ->
                              Js.Promise.resolve (Expect.toContainString "Unknown verification goal" (`Just e))
                            | _ ->
                              Js.Promise.resolve (fail "unexpected result")
                          )
                      | Belt.Result.Error (e, _j) ->
                        Js.Promise.resolve (fail (Printf.sprintf "error from imandra: %s" e))
                    )
                | Belt.Result.Error (e, _j) ->
                  Js.Promise.resolve (fail (Printf.sprintf "error from imandra: %s" e))
              )
          | Belt.Result.Error (e, _j) ->
            Js.Promise.resolve (fail (Printf.sprintf "error from imandra: %s" e))
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

open Jest

let runningImandraProcess = ref None

let () =
  beforeAllPromise (fun () ->
      let open Imandra_client in
      Imandra_client.start (imandraOptions ~syntax:"ocaml" ~debug:true ~serverCmd:"imandra-http-server-dev" ())
      |> Js.Promise.then_ (fun ip ->
          runningImandraProcess := Some ip;
          Js.Promise.resolve ()
        )
    )

let () =
  testPromise "verify refuted" (fun () ->
      match !runningImandraProcess with
      | Some ip ->
        Imandra_client.Verify.by_src ip ~src:"fun x -> x = 3"
        |> Js.Promise.then_ (function
            | Belt.Result.Ok (Imandra_client.Verify.Refuted _, _) -> Js.Promise.resolve pass
            | Belt.Result.Ok _ -> Js.Promise.resolve (fail "wrong verify result")
            | Belt.Result.Error (e, _) -> Js.Promise.resolve (fail (Printf.sprintf "error from imandra: %s" e))
          )
      | None ->
        Js.Promise.reject (Failure "no imandra process available?")
    )

let () =
  testPromise "verify by name proved" (fun () ->
      match !runningImandraProcess with
      | Some ip ->
        Imandra_client.Eval.by_src ip ~src:"let rev_rev x = 3 = 3"
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
      | None ->
        Js.Promise.reject (Failure "no imandra process available?")
    )

let () =
  testPromise "instance" (fun () ->
      match !runningImandraProcess with
      | Some ip ->
        Imandra_client.Instance.by_src ip ~src:"fun x -> List.length x > 4"
        |> Js.Promise.then_ (function
            | Belt.Result.Ok (Imandra_client.Instance.Sat _, _) -> Js.Promise.resolve pass
            | Belt.Result.Ok (_, _) -> Js.Promise.resolve (fail "instance result not satisifed")
            | Belt.Result.Error (e, _) -> Js.Promise.resolve (fail (Printf.sprintf "error from imandra: %s" e))
          )
      | None ->
        Js.Promise.reject (Failure "no imandra process available?")
    )

let () =
  testPromise "eval failure" (fun () ->
      match !runningImandraProcess with
      | Some ip ->
        Imandra_client.Instance.by_src ip ~src:"garbage"
        |> Js.Promise.then_ (function
            | Belt.Result.Ok (_, _) -> Js.Promise.resolve (fail "unexpected success")
            | Belt.Result.Error (e, _) ->
              Js.Promise.resolve (Expect.toEqual e (`Just "Unbound value garbage"))
          )
      | None ->
        Js.Promise.reject (Failure "no imandra process available?")
    )

let () =
  afterAllAsync (fun finish ->
      match !runningImandraProcess with
      | Some ip ->
        Imandra_client.stop ip
        |> Js.Promise.then_ (fun _ ->
            runningImandraProcess := None;
            finish ();
            Js.Promise.resolve ()
          )
        |> ignore

      | None ->
        (* assume failure during startup *)
        finish ();
    )

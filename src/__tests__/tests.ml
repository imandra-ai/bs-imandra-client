open Jest

let runningImandraProcess = ref None

let () =
  beforeAllAsync (fun finish ->
      let open Imandra_client in
      Imandra_client.start (imandraOptions ~syntax:"ocaml" ~serverCmd:"imandra-http-server-dev" ())
      |> Js.Promise.then_ (fun ip ->
          runningImandraProcess := Some ip;
          finish ();
          Js.Promise.resolve ()
        )
      |> ignore
    )

let () =
  testPromise "verify refuted" (fun () ->
      match !runningImandraProcess with
      | Some ip ->
        Imandra_client.Verify.by_src ip ~src:"fun x -> x = 3"
        |> Js.Promise.then_ (fun json ->
            let res =
              json
              |> Imandra_client.Verify.Decode.verifyResult
            in
            let assertion =
              match res with
              | Refuted _ -> pass
              | _ -> fail "Wrong verify result"
            in
            Js.Promise.resolve assertion
          )
      | None ->
        Js.Promise.reject (Failure "no imandra process available?")
    )

let () =
  testPromise "verify by name proved" (fun () ->
      match !runningImandraProcess with
      | Some ip ->
        Imandra_client.Eval.by_src ip ~src:"let rev_rev x = 3 = 3"
        |> Js.Promise.then_ (fun json ->
            Imandra_client.Verify.by_name ip ~name:"rev_rev"
            |> Js.Promise.then_ (fun json ->
                let res =
                  json
                  |> Imandra_client.Verify.Decode.verifyResult
                in
                let assertion =
                  match res with
                  | Proved -> pass
                  | _ -> fail "Wrong verify result"
                in
                Js.Promise.resolve assertion
              )
          )
      | None ->
        Js.Promise.reject (Failure "no imandra process available?")
    )

let () =
  testPromise "instance" (fun () ->
      match !runningImandraProcess with
      | Some ip ->
        Imandra_client.Instance.by_src ip ~src:"fun x -> List.length x > 4"
        |> Js.Promise.then_ (fun json ->
            Js.Console.log json;
            let res =
              json
              |> Imandra_client.Instance.Decode.instanceResult
            in
            let assertion =
              match res with
              | Sat _ -> pass
              | _ -> fail "Wrong verify result"
            in
            Js.Promise.resolve assertion
          )
      | None ->
        Js.Promise.reject (Failure "no imandra process available?")
    )

let () =
  afterAllAsync ~timeout:10000 (fun finish ->
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
        fail "no imandra process available during teardown?" |> ignore
    )

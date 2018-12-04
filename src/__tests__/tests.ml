open Jest

let runningImandraProcess = ref None

let () =
  beforeAllAsync (fun finish ->
      let open Imandra_client in
      Imandra_client.start (imandraOptions ~syntax:"ocaml" ())
      |> Js.Promise.then_ (fun ip ->
          runningImandraProcess := Some ip;
          finish ();
          Js.Promise.resolve ()
        )
      |> ignore
    )

let () =
  testPromise "some test" (fun () ->
      match !runningImandraProcess with
      | Some ip ->
        Imandra_client.verify ip ~src:"fun x -> x = 3"
        |> Js.Promise.then_ (fun json ->
            Js.Console.log json;
            let res =
              json
              |> Imandra_client.Decode.verifyResult
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

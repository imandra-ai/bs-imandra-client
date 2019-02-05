module I = Imandra_client

let print_with_instance (m : I.Api.Response.with_instance) =
  Printf.sprintf "%s:\n%s"
    m.instance.type_
    (match m.instance.printed with
     | Some p -> p
     | None -> m.instance.model.src_base64 |> I.from_base64
    )

let print_with_unknown_reason (m : I.Api.Response.with_unknown_reason) =
  Printf.sprintf "%s" m.unknown_reason

let print_upto (x : I.Api.Response.upto) =
  match x with
  | Upto_steps s -> (Printf.sprintf "steps: %d" s)
  | Upto_bound b -> (Printf.sprintf "bound: %d" b)

let print_verify_response (res: (I.Api.Response.verify_result, I.Error.t) Belt.Result.t) =
  match res with
  | Belt.Result.Ok(I.Api.Response.V_proved) ->
    "proved"
  | Belt.Result.Ok(I.Api.Response.V_proved_upto x) ->
    (Printf.sprintf "proved up to:\n%s" (print_upto x))
  | Belt.Result.Ok(I.Api.Response.V_refuted x) ->
    (Printf.sprintf "refuted with instance:\n%s" (print_with_instance x))
  | Belt.Result.Ok(I.Api.Response.V_unknown x) ->
    (Printf.sprintf "unknown with reason:\n%s" (print_with_unknown_reason x))
  | Belt.Result.Error(e) ->
    (Format.asprintf "ERROR\n%a" I.Error.pp e)

let to_be_proved (res : (I.Api.Response.verify_result, I.Error.t) Belt.Result.t) =
  match res with
  | Belt.Result.Ok(I.Api.Response.V_proved) ->
    Jest.pass
  | x ->
    Jest.fail (Format.asprintf "Expected: proved, Got: %s" (print_verify_response x))

let to_be_proved_upto (res : (I.Api.Response.verify_result, I.Error.t) Belt.Result.t) =
  match res with
  | Belt.Result.Ok(I.Api.Response.V_proved_upto _) ->
    Jest.pass
  | x ->
    Jest.fail (Format.asprintf "Expected: proved_upto, Got: %s" (print_verify_response x))

let to_be_unknown (res : (I.Api.Response.verify_result, I.Error.t) Belt.Result.t) =
  match res with
  | Belt.Result.Ok(I.Api.Response.V_unknown _) ->
    Jest.pass
  | x ->
    Jest.fail (Format.asprintf "Expected: unknown, Got: %s" (print_verify_response x))

let to_be_refuted (res : (I.Api.Response.verify_result, I.Error.t) Belt.Result.t) =
  match res with
  | Belt.Result.Ok(I.Api.Response.V_refuted _) ->
    Jest.pass
  | x ->
    Jest.fail (Format.asprintf "Expected: refuted, Got: %s" (print_verify_response x))

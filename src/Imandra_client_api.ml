let append_opt_key k f opt xs =
  match opt with None -> xs | Some x -> xs @ [ (k, f x) ]


type src_syntax =
  | Reason
  | Iml

module D = Decoders

module Request = struct
  module Hints = struct
    module Induct = struct
      type functional = { f_name : string }

      type structural =
        { style : [ `Multiplicative | `Additive ]
        ; vars : string list
        }

      type t =
        | Functional of functional
        | Structural of structural
        | Default
    end

    module Method = struct
      type unroll = { steps : int option }

      type ext_solver = { name : string }

      type t =
        | Unroll of unroll
        | Ext_solver of ext_solver
        | Auto
        | Induct of Induct.t
    end

    type t = { method_ : Method.t }
  end

  type printer_details =
    { name : string
    ; cx_var_name : string
    }

  type verify_req_src =
    { syntax : src_syntax
    ; src_base64 : string
    ; instance_printer : printer_details option
    ; hints : Hints.t option
    }

  type verify_req_name =
    { name : string
    ; instance_printer : printer_details option
    ; hints : Hints.t option
    }

  type instance_req_src =
    { syntax : src_syntax
    ; src_base64 : string
    ; instance_printer : printer_details option
    ; hints : Hints.t option
    }

  type instance_req_name =
    { name : string
    ; instance_printer : printer_details option
    ; hints : Hints.t option
    }

  type eval_req_src =
    { syntax : src_syntax
    ; src_base64 : string
    }
end

module Response = struct
  type model =
    { syntax : src_syntax
    ; src_base64 : string
    }

  type instance =
    { model : model
    ; type_ : string
    ; printed : string option
    }

  type with_instance = { instance : instance }

  type with_unknown_reason = { unknown_reason : string }

  type error = { error : string }

  type upto =
    | Upto_steps of int
    | Upto_bound of int

  type instance_result =
    | I_unsat
    | I_unsat_upto of upto
    | I_sat of with_instance
    | I_unknown of with_unknown_reason

  type verify_result =
    | V_proved
    | V_proved_upto of upto
    | V_refuted of with_instance
    | V_unknown of with_unknown_reason
end

module Decoders (D : Decoders.Decode.S) = struct
  open D

  let src_syntax : src_syntax decoder =
    maybe string
    >>= function
    | Some "reason" ->
        succeed Reason
    | Some "iml" ->
        succeed Iml
    | Some _ ->
        fail (Printf.sprintf "Expected 'reason' or 'iml'")
    | None ->
        succeed Iml


  module Request = struct
    module Hints = struct
      module Induct = struct
        open Request.Hints.Induct

        let structural =
          field "vars" (list string)
          >>= fun vars ->
          field "style" string
          >>= fun style_str ->
          let style =
            match style_str with
            | "multiplicative" ->
                succeed `Multiplicative
            | "additive" ->
                succeed `Additive
            | _ ->
                fail "Expected 'multiplicative' or 'additive'"
          in
          style >|= fun style -> Structural { style; vars }


        let functional =
          field "f_name" string >|= fun f_name -> Functional { f_name }


        let t =
          field "type" string
          >>= function
          | "functional" ->
              field "body" functional
          | "structural" ->
              field "body" structural
          | "default" ->
              succeed Default
          | _ ->
              fail "Expected 'functional', 'structural' or 'default'"
      end

      module Method = struct
        open Request.Hints.Method

        let unroll = field_opt "steps" int >>= fun steps -> succeed { steps }

        let ext_solver = field "name" string >>= fun name -> succeed { name }

        let t : t decoder =
          field "type" string
          >>= function
          | "unroll" ->
              field "body" unroll >|= fun x -> Unroll x
          | "ext_solver" ->
              field "body" ext_solver >|= fun x -> Ext_solver x
          | "auto" ->
              succeed Auto
          | "induct" ->
              field "body" Induct.t >|= fun t -> Induct t
          | _ ->
              fail "Expected 'unroll', 'ext_solver', 'auto' or 'induct'"
      end

      let t : Request.Hints.t decoder =
        field "method" Method.t
        >>= fun method_ -> succeed Request.Hints.{ method_ }
    end

    let printer_details : Request.printer_details decoder =
      field "name" string
      >>= fun name ->
      field "cx_var_name" string
      >>= fun cx_var_name -> succeed Request.{ name; cx_var_name }


    let verify_req_src : Request.verify_req_src decoder =
      field "syntax" src_syntax
      >>= fun syntax ->
      field "src_base64" string
      >>= fun src_base64 ->
      field_opt "instance_printer" printer_details
      >>= fun instance_printer ->
      field_opt "hints" Hints.t
      >>= fun hints ->
      let r : Request.verify_req_src =
        Request.{ syntax; src_base64; instance_printer; hints }
      in
      succeed r


    let verify_req_name : Request.verify_req_name decoder =
      field "name" string
      >>= fun name ->
      field_opt "instance_printer" printer_details
      >>= fun instance_printer ->
      field_opt "hints" Hints.t
      >>= fun hints ->
      let r : Request.verify_req_name =
        Request.{ name; instance_printer; hints }
      in
      succeed r


    let instance_req_src : Request.instance_req_src decoder =
      field "syntax" src_syntax
      >>= fun syntax ->
      field "src_base64" string
      >>= fun src_base64 ->
      field_opt "instance_printer" printer_details
      >>= fun instance_printer ->
      field_opt "hints" Hints.t
      >>= fun hints ->
      let r : Request.instance_req_src =
        Request.{ syntax; src_base64; instance_printer; hints }
      in
      succeed r


    let instance_req_name : Request.instance_req_name decoder =
      field "name" string
      >>= fun name ->
      field_opt "instance_printer" printer_details
      >>= fun instance_printer ->
      field_opt "hints" Hints.t
      >>= fun hints ->
      let r : Request.instance_req_name =
        Request.{ name; instance_printer; hints }
      in
      succeed r


    let eval_req_src : Request.eval_req_src decoder =
      field "syntax" src_syntax
      >>= fun syntax ->
      field "src_base64" string
      >>= fun src_base64 -> succeed Request.{ syntax; src_base64 }
  end

  module Response = struct
    open Response

    type my_error = Response.error

    let src_syntax : src_syntax decoder =
      maybe string
      >>= function
      | Some "reason" ->
          succeed Reason
      | Some "iml" ->
          succeed Iml
      | Some _ ->
          fail (Printf.sprintf "Expected 'reason' or 'iml'")
      | None ->
          succeed Iml


    let model : model decoder =
      field "syntax" src_syntax
      >>= fun syntax ->
      field "src_base64" string
      >>= fun src_base64 -> succeed { syntax; src_base64 }


    let instance : instance decoder =
      field "model" model
      >>= fun model ->
      field "type" string
      >>= fun type_ ->
      field_opt "printed" string
      >>= fun printed -> succeed { model; type_; printed }


    let with_instance : with_instance decoder =
      field "instance" instance >>= fun instance -> succeed { instance }


    let with_unknown_reason : with_unknown_reason decoder =
      field "unknown_reason" string
      >>= fun unknown_reason -> succeed { unknown_reason }


    let upto : upto decoder =
      one_of
        [ ("steps", field "steps" int >|= fun x -> Upto_steps x)
        ; ("bound", field "bound" int >|= fun x -> Upto_bound x)
        ]


    let error : my_error decoder =
      field "error" string >>= fun e -> succeed { error = e }


    let verify_result : verify_result decoder =
      field "type" string
      >>= function
      | "proved" ->
          succeed V_proved
      | "proved_upto" ->
          field "body" upto >|= fun x -> V_proved_upto x
      | "refuted" ->
          field "body" with_instance >|= fun x -> V_refuted x
      | "unknown" ->
          field "body" with_unknown_reason >|= fun x -> V_unknown x
      | _ ->
          fail "Expected 'verified', 'refuted' or 'unknown'"


    let instance_result : instance_result decoder =
      field "type" string
      >>= function
      | "unsat" ->
          succeed I_unsat
      | "unsat_upto" ->
          field "body" upto >|= fun x -> I_unsat_upto x
      | "sat" ->
          field "body" with_instance >|= fun x -> I_sat x
      | "unknown" ->
          field "body" with_unknown_reason >|= fun x -> I_unknown x
      | _ ->
          fail "Expected 'verified', 'refuted' or 'unknown'"
  end
end

module Encoders (E : D.Encode.S) = struct
  open E

  let src_syntax : src_syntax encoder = function
    | Reason ->
        string "reason"
    | Iml ->
        string "iml"


  module Request = struct
    module Hints = struct
      module Induct = struct
        open Request.Hints.Induct

        let functional x = obj [ ("f_name", string x.f_name) ]

        let structural x =
          obj
            [ ("vars", list string x.vars)
            ; ( "style"
              , string
                  ( match x.style with
                  | `Multiplicative ->
                      "multipilicative"
                  | `Additive ->
                      "additive" ) )
            ]


        let t : t encoder = function
          | Default ->
              obj [ ("type", string "default") ]
          | Functional x ->
              obj [ ("type", string "functional"); ("body", functional x) ]
          | Structural x ->
              obj [ ("type", string "structural"); ("body", structural x) ]
      end

      module Method = struct
        open Request.Hints.Method

        let unroll x = obj [ ("steps", option int x.steps) ]

        let ext_solver x = obj [ ("name", string x.name) ]

        let t : t encoder = function
          | Unroll x ->
              obj [ ("type", string "unroll"); ("body", unroll x) ]
          | Ext_solver x ->
              obj [ ("type", string "ext_solver"); ("body", ext_solver x) ]
          | Auto ->
              obj [ ("type", string "auto") ]
          | Induct x ->
              obj [ ("type", string "auto"); ("body", Induct.t x) ]
      end

      let t (x : Request.Hints.t) =
        obj [ ("method", Method.t x.Request.Hints.method_) ]
    end

    module Hints_e = Hints

    let src_syntax : src_syntax encoder = function
      | Reason ->
          string "reason"
      | Iml ->
          string "iml"


    open Request

    let printer_details (x : Request.printer_details) =
      obj [ ("name", string x.name); ("cx_var_name", string x.cx_var_name) ]


    let verify_req_src (x : Request.verify_req_src) =
      obj
        ( [ ("syntax", src_syntax x.syntax)
          ; ("src_base64", string x.src_base64)
          ]
        |> append_opt_key "instance_printer" printer_details x.instance_printer
        |> append_opt_key "hints" Hints_e.t x.hints )


    let verify_req_name (x : Request.verify_req_name) =
      obj
        ( [ ("name", string x.name) ]
        |> append_opt_key "instance_printer" printer_details x.instance_printer
        |> append_opt_key "hints" Hints_e.t x.hints )


    let instance_req_src (x : Request.instance_req_src) =
      obj
        ( [ ("syntax", src_syntax x.syntax)
          ; ("src_base64", string x.src_base64)
          ]
        |> append_opt_key "instance_printer" printer_details x.instance_printer
        |> append_opt_key "hints" Hints_e.t x.hints )


    let instance_req_name (x : Request.instance_req_name) =
      obj
        ( [ ("name", string x.name) ]
        |> append_opt_key "instance_printer" printer_details x.instance_printer
        |> append_opt_key "hints" Hints_e.t x.hints )


    let eval_req_src (x : Request.eval_req_src) =
      obj
        [ ("syntax", src_syntax x.syntax); ("src_base64", string x.src_base64) ]
  end

  module Response = struct
    open Response

    let model (x : model) =
      obj
        [ ("syntax", src_syntax x.syntax); ("src_base64", string x.src_base64) ]


    let instance (x : instance) =
      obj
        ( [ ("model", model x.model); ("type", string x.type_) ]
        |> append_opt_key "printed" string x.printed )


    let with_instance (x : with_instance) =
      obj [ ("instance", instance x.instance) ]


    let with_unknown_reason (x : with_unknown_reason) =
      obj [ ("unknown_reason", string x.unknown_reason) ]


    let error_response (x : error) = obj [ ("error", string x.error) ]

    let upto = function
      | Upto_steps s ->
          obj [ ("steps", int s) ]
      | Upto_bound b ->
          obj [ ("bound", int b) ]


    let verify_result : verify_result encoder = function
      | V_proved ->
          obj [ ("type", string "proved") ]
      | V_proved_upto x ->
          obj [ ("type", string "proved_upto"); ("body", upto x) ]
      | V_refuted x ->
          obj [ ("type", string "refuted"); ("body", with_instance x) ]
      | V_unknown x ->
          obj [ ("type", string "unknown"); ("body", with_unknown_reason x) ]


    let instance_result : instance_result encoder = function
      | I_unsat ->
          obj [ ("type", string "unsat") ]
      | I_unsat_upto x ->
          obj [ ("type", string "unsat_upto"); ("body", upto x) ]
      | I_sat x ->
          obj [ ("type", string "sat"); ("body", with_instance x) ]
      | I_unknown x ->
          obj [ ("type", string "unknown"); ("body", with_unknown_reason x) ]
  end
end

let append_opt_key k f opt xs =
  match opt with
  | None -> xs
  | Some x -> xs @ [(k, f x)]

module Common = struct
  type src_syntax =
    | Reason
    | OCaml
end

module Request = struct
  module Hints = struct
    module Induct = struct
      type functional =
        { f_name: string; }

      type structural = {
        style: [`Multiplicative | `Additive];
        vars: string list;
      }

      type t =
        | Functional of functional
        | Structural of structural
        | Default
    end
    module Method = struct
      type unroll = { steps: int option }

      type ext_solver = { name: string }

      type t =
        | Unroll of unroll
        | Ext_solver of ext_solver
        | Auto
        | Induct of Induct.t
    end

    type t =
      { method_ : Method.t
      }
  end

  type printer_details =
    { name : string
    ; cx_var_name : string
    }

  type verify_req_src =
    { syntax : Common.src_syntax
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
    { syntax : Common.src_syntax
    ; src_base64 : string
    ; instance_printer : printer_details option
    }

  type instance_req_name =
    { name : string
    ; instance_printer : printer_details option
    }

  type eval_req_src =
    { syntax : Common.src_syntax
    ; src_base64 : string
    }
end

module Response = struct
  type model =
    { syntax : Common.src_syntax
    ; src_base64 : string
    }

  type instance =
    { model : model
    ; type_ : string
    ; printed : string option
    }

  type with_instance =
    { result : string
    ; instance : instance
    }

  type base =
    { result : string
    }

  type with_unknown_reason =
    { result : string
    ; unknown_reason : string
    }

  type error =
    { error : string }
end


module Decoders(D: Decoders.Decode.S)(E: Decoders.Encode.S) = struct
  module Common = struct
    module Decode = struct
      open D
      let src_syntax : Common.src_syntax decoder =
        (maybe string) >>= function
        | Some "reason" -> succeed Reason
        | Some "ocaml" -> succeed OCaml
        | Some _ -> fail (Printf.sprintf "Expected 'reason' or 'ocaml'")
        | None -> succeed OCaml
    end

    module Encode = struct
      open E
      let src_syntax : Common.src_syntax encoder = function
        | Reason -> string "reason"
        | OCaml -> string "ocaml"
    end
  end

  module Request = struct
    module Hints = struct
      module Induct = struct
        open Request.Hints.Induct

        module Decode = struct
          open D

          let structural =
            (field "vars" (list string)) >>= fun vars ->
            (field "style" string) >>= fun style_str ->
            let style = match style_str with
              | "multiplicative" -> succeed `Multiplicative
              | "additive" -> succeed `Additive
              | _ -> fail "Expected 'multiplicative' or 'additive'"
            in
            style >|= fun style ->
            (Structural { style; vars })

          let functional =
            field "f_name" string >|= fun f_name -> Functional { f_name }

          let t =
            (field "type" string) >>= function
            | "functional" -> (field "body" functional)
            | "structural" -> (field "body" structural)
            | "default" -> succeed Default
            | _ -> fail "Expected 'functional', 'structural' or 'default'"
        end

        module Encode = struct
          open E
          let functional x =
            obj [ ("f_name", string x.f_name ) ]

          let structural x =
            obj
              [ ("vars", list string x.vars)
              ; ("style"
                , string (match x.style with
                      | `Multiplicative -> "multipilicative"
                      | `Additive -> "additive")
                )
              ]

          let t : t encoder = function
            | Default ->  obj [("type", string "default")]
            | Functional x -> obj [("type", string "functional"); ]list value [ string "functional"; functional x ]
            | Structural x -> list value [ string "structural"; structural x ]
        end
      end

      module Method = struct
        open Request.Hints.Method

        module Decode = struct
          open D
          let unroll =
            (maybe (field "steps" int)) >>= fun steps ->
            succeed (Unroll { steps })

          let ext_solver =
            field "name" string >>= fun name ->
            succeed (Ext_solver { name })

          let t : t decoder =
            D.index 0 string >>= function
            | "unroll" -> (D.index 1 unroll)
            | "ext_solver" -> (D.index 1 ext_solver)
            | "auto" -> succeed Auto
            | "induct" -> (D.index 1 (Induct.Decode.t >|= fun t -> Induct t))
            | _ -> fail "Expected 'unroll', 'ext_solver', 'auto' or 'induct'"
        end

        module Encode = struct
          open E

          let unroll x =
            obj [("steps", option int x.steps)]

          let ext_solver x =
            obj [("name", string x.name)]

          let t : t encoder = function
            | Unroll x -> list value [ string "unroll"; unroll x ]
            | Ext_solver x -> list value [ string "ext_solver"; ext_solver x ]
            | Auto -> list value [ string "auto" ]
            | Induct x -> list value [ string "induct"; Induct.Encode.t x ]
        end
      end

      module Decode = struct
        open D
        let t : Request.Hints.t decoder =
          (field "method" Method.Decode.t) >>= fun method_ ->
          succeed { method_ }
      end

      module Encode = struct
        open E
        let t : Request.Hints.t encoder = fun x ->
          obj [ ("method", Method.Encode.t x.method_) ]
      end
    end

    module Decode = struct
      open D
      let printer_details : Request.printer_details decoder =
        (field "name" string) >>= fun name ->
        (field "cx_var_name" string) >>= fun cx_var_name ->
        succeed { name; cx_var_name }

      let verify_req_src : Request.verify_req_src decoder =
        (field "syntax" Common.Decode.src_syntax) >>= fun syntax ->
        (field "src_base64" string) >>= fun src_base64 ->
        (maybe (field "instance_printer" printer_details)) >>= fun instance_printer ->
        (maybe (field "hints" Hints.Decode.t)) >>= fun hints ->
        succeed { syntax; src_base64; instance_printer; hints }

      let verify_req_name : Request.verify_req_name decoder =
        (field "name" string) >>= fun name ->
        (maybe (field "instance_printer" printer_details)) >>= fun instance_printer ->
        (maybe (field "hints" Hints.Decode.t)) >>= fun hints ->
        succeed { name; instance_printer; hints }

      let instance_req_src : Request.instance_req_src decoder =
        (field "syntax" Common.Decode.src_syntax) >>= fun syntax ->
        (field "src_base64" string) >>= fun src_base64 ->
        (maybe (field "instance_printer"  printer_details)) >>= fun instance_printer ->
        succeed { syntax; src_base64; instance_printer }

      let instance_req_name : Request.instance_req_name decoder =
        (field "name" string) >>= fun name ->
        (maybe (field "instance_printer" printer_details)) >>= fun instance_printer ->
        succeed { name; instance_printer }

      let eval_req_src : Request.eval_req_src decoder =
        (field "syntax" Common.Decode.src_syntax) >>= fun syntax ->
        (field "src_base64" string) >>= fun src_base64 ->
        succeed { syntax; src_base64 }
    end

    module Encode = struct
      open E
      let printer_details (x : Request.printer_details) =
        obj [ ("name", string x.name ) ]

      let verify_req_src (x : Request.verify_req_src) =
        obj ([ ("syntax", Common.Encode.src_syntax x.syntax )
             ; ("src_base64", string x.src_base64)
             ]
             |> append_opt_key "instance_printer" printer_details x.instance_printer
             |> append_opt_key "hints" Hints.Encode.t x.hints)

      let verify_req_name (x : Request.verify_req_name) =
        obj ([ ("name", string x.name )
             ]
             |> append_opt_key "instance_printer" printer_details x.instance_printer
             |> append_opt_key "hints" Hints.Encode.t x.hints)

      let instance_req_src (x : Request.instance_req_src) =
        obj ([ ("syntax", Common.Encode.src_syntax x.syntax )
             ; ("src_base64", string x.src_base64)
             ]
             |> append_opt_key "instance_printer" printer_details x.instance_printer)

      let instance_req_name (x : Request.instance_req_name) =
        obj ([ ("name", string x.name )
             ]
             |> append_opt_key "instance_printer" printer_details x.instance_printer)

      let eval_req_src (x : Request.eval_req_src) =
        obj ([ ("syntax", Common.Encode.src_syntax x.syntax)
             ; ("src_base64", string x.src_base64)
             ])
    end
  end

  module Response = struct
    open Response

    module Decode = struct
      type my_error = error
      open D
      let model : model decoder =
        (field "syntax" Common.Decode.src_syntax) >>= fun syntax ->
        (field "src_base64" string) >>= fun src_base64 ->
        succeed { syntax; src_base64 }

      let instance : instance decoder =
        (field "model" model) >>= fun model ->
        (field "type" string) >>= fun type_ ->
        (maybe (field "printed" string)) >>= fun printed ->
        succeed { model; type_; printed }

      let with_instance : with_instance decoder =
        (field "result" string) >>= fun result ->
        (field "instance" instance) >>= fun instance ->
        succeed { result; instance }

      let base : base decoder =
        (field "result" string) >>= fun result ->
        succeed { result }

      let with_unknown_reason : with_unknown_reason decoder =
        (field "result" string) >>= fun result ->
        (field "unknown_reason" string) >>= fun unknown_reason ->
        succeed { result; unknown_reason }

      let error : my_error decoder =
        (field "error" string) >>= fun e ->
        succeed { error = e }
    end

    module Encode = struct
      open E
      let model (x : model) =
        obj [ ("syntax", Common.Encode.src_syntax x.syntax )
            ; ("src_base64", string x.src_base64)
            ]

      let instance (x : instance) =
        obj ([ ("model", model x.model)
             ; ("type", string x.type_)
             ]
             |> append_opt_key "printed" string x.printed)

      let with_instance (x : with_instance) =
        obj [ ("result", string x.result)
            ; ("instance", instance x.instance )
            ]

      let base (x : base) =
        obj [ ("result", string x.result)
            ]

      let with_unknown_reason (x : with_unknown_reason) =
        obj [ ("result", string x.result)
            ; ("unknown_reason", string x.unknown_reason)
            ]

      let error_response (x : error) =
        obj [ ("error", string x.error) ]
    end
  end
end

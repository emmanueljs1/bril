open Yojson

module StrMap = Map.Make(String)

type ty = Prim of string | Param of (string * ty)

type lit =
  | IntLit of int64
  | BoolLit of bool

type ins =
  { op: string
  ; dest: string option
  ; dest_ty: ty option
  ; value: lit option
  ; args: (string list) option
  ; funcs: (string list) option
  ; labels: (string list) option
  }

type instr =
  | Label of string
  | Instr of ins

type func =
  { name: string
  ; args: (string * ty) list
  ; ret_ty: ty option
  ; instrs: instr list
  }

type prog = func list

let empty_ins : ins =
  { op = ""
  ; dest = None
  ; dest_ty = None
  ; value = None
  ; args = None
  ; funcs = None
  ; labels = None
  }

let jmp (lbl: string) : instr =
  Instr { empty_ins with op = "\"jmp\""; labels = Some [lbl] }

let str_of_json : Raw.t -> string = function
  | `Stringlit str -> str
  | _ -> failwith "ill-forced bril code"

let strs_of_json : Raw.t -> string list = function
  | `List l -> List.map str_of_json l
  | _ -> failwith "ill-formed bril code"

let rec ty_of_json : Raw.t -> ty = function
  | `Stringlit s -> Prim s
  | `Assoc [name, json] -> Param (name, ty_of_json json)
  | _ -> failwith "ill-formed bril code"

let lit_of_json : Raw.t -> lit = function
  | `Intlit s -> IntLit (Int64.of_string s)
  | `Bool b -> BoolLit b
  | _ -> failwith "ill-formed bril code"

let arg_of_json : Raw.t -> string * ty = function
  | `Assoc l ->
      let m = StrMap.of_list l in
      StrMap.find "name" m |> str_of_json, StrMap.find "type" m |> ty_of_json
  | _ -> failwith "ill-formed bril code"

let args_of_json : Raw.t -> (string * ty) list = function
  | `List l -> List.map arg_of_json l
  | _ -> failwith "ill-formed bril code"

let instr_of_json : Raw.t -> instr = function
  | `Assoc ["label", `Stringlit lbl] -> Label lbl
  | `Assoc l ->
      let m = StrMap.of_list l in
      Instr { op = StrMap.find "op" m |> str_of_json
            ; dest = StrMap.find_opt "dest" m |> Option.map str_of_json
            ; dest_ty = StrMap.find_opt "type" m |> Option.map ty_of_json 
            ; value = StrMap.find_opt "value" m |> Option.map lit_of_json
            ; args = StrMap.find_opt "args" m |> Option.map strs_of_json
            ; funcs = StrMap.find_opt "funcs" m |> Option.map strs_of_json
            ; labels = StrMap.find_opt "labels" m |> Option.map strs_of_json
            }
  | _ -> failwith "ill-formed bril code"

let instrs_of_json : Raw.t -> instr list = function
  | `List l -> List.map instr_of_json l
  | _ -> failwith "ill-formed bril code"

let func_of_json : Raw.t -> func = function
  | `Assoc l ->
      let m = StrMap.of_list l in
      { name = StrMap.find "name" m |> str_of_json
      ; args = (try StrMap.find "args" m |> args_of_json
                with Not_found -> [])
      ; ret_ty = StrMap.find_opt "type" m |> Option.map ty_of_json
      ; instrs = StrMap.find "instrs" m |> instrs_of_json
      }
  | _ -> failwith "ill-formed bril code"

let prog_of_json : Raw.t -> prog = function
  | `Assoc ["functions", `List l] ->
      List.map func_of_json l
  | _ -> failwith "ill-formed bril code"

let rec json_of_ty : ty -> Raw.t = function
  | Prim s -> `Stringlit s
  | Param (name, t) -> `Assoc [name, json_of_ty t]

let json_of_lit : lit -> Raw.t = function
  | IntLit i -> `Intlit (Int64.to_string i)
  | BoolLit b -> `Bool b

let json_of_arg : (string * ty) -> Raw.t =
  fun (name, t) -> `Assoc [("name", `Stringlit name); ("type", json_of_ty t)]

let json_of_instr : instr -> Raw.t = function
  | Label l -> `Assoc ["label", `Stringlit l]
  | Instr ins ->
      let op = ["op", `Stringlit ins.op] in
      let dest =
        match ins.dest with
        | None -> []
        | Some s -> ["dest", `Stringlit s]
      in
      let dest_ty =
        match ins.dest_ty with
        | None -> []
        | Some t -> ["type", json_of_ty t]
      in
      let value =
        match ins.value with
        | None -> []
        | Some v -> ["value", json_of_lit v]
      in
      let args =
        match ins.args with
        | None -> []
        | Some l -> ["args", `List (List.map (fun s -> `Stringlit s) l)]
      in
      let funcs =
        match ins.funcs with
        | None -> []
        | Some l -> ["funcs", `List (List.map (fun s -> `Stringlit s) l)]
      in
      let labels =
        match ins.labels with
        | None -> []
        | Some l -> ["labels", `List (List.map (fun s -> `Stringlit s) l)]
      in
      `Assoc (op @ dest @ dest_ty @ value @ args @ funcs @ labels)

let json_of_func (f: func) : Raw.t =
  let assoc =
    [ ("name", `Stringlit f.name)
    ; ("args", `List (List.map json_of_arg f.args))
    ; ("instrs", `List (List.map json_of_instr f.instrs))
    ]
  in
  match f.ret_ty with
  | None -> `Assoc assoc
  | Some t -> `Assoc (assoc @ ["type", json_of_ty t])

let json_of_prog (p: prog) : Raw.t =
  `Assoc [("functions", `List (List.map json_of_func p))]

let rec add_jumps : instr list -> instr list = function
  | [] -> []
  | Label s :: instrs -> jmp s :: Label s :: add_jumps instrs
  | instr :: instrs -> instr :: add_jumps instrs

let () =
  let p = Raw.from_file Sys.argv.(1) |> prog_of_json in
  let p' = List.map (fun f -> { f with instrs = add_jumps f.instrs }) p in
  json_of_prog p' |> Raw.to_string |> print_endline

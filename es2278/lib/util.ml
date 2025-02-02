open Syntax
open Yojson

module StrMap = Map.Make(String)

let map_json_lst (f : Raw.t -> 'a) : Raw.t -> 'a list = function
  | `List l -> List.map f l
  | _ -> failwith "ill-formed bril code"

let str_of_json : Raw.t -> string = function
  | `Stringlit s -> String.sub s 1 (String.length s - 2)
  | _ -> failwith "ill-forced bril code"

let prim_ty_of_str : string -> ty = function
  | "int" -> Int
  | "bool" -> Bool
  | _ -> failwith "ill-formed bril code"

let rec ty_of_json : Raw.t -> ty = function
  | `Stringlit _ as json -> str_of_json json |> prim_ty_of_str 
  | `Assoc [name, json] -> Param (name, ty_of_json json)
  | _ -> failwith "ill-formed bril code"

let lit_of_json : Raw.t -> lit = function
  | `Intlit s -> Int (Int64.of_string s)
  | `Bool b -> Bool b
  | _ -> failwith "ill-formed bril code"

let arg_of_json: Raw.t -> arg = function
  | `Assoc l ->
      let m = StrMap.of_list l in
      StrMap.find "name" m |> str_of_json, StrMap.find "type" m |> ty_of_json
  | _ -> failwith "ill-formed bril code"

let dest_of_json (m: Raw.t StrMap.t) : dest =
  StrMap.find "dest" m |> str_of_json, StrMap.find "type" m |> ty_of_json

let value_of_json (m: Raw.t StrMap.t) : lit =
  StrMap.find "value" m |> lit_of_json

let two_args : id list -> id * id = function
  | id1 :: id2 :: _ -> id1 , id2
  | _ -> failwith "ill-formed bril code"

let exp_of_json (op: string) (m: Raw.t StrMap.t) : exp =
  let args = StrMap.find "args" m |> map_json_lst str_of_json in
  match op with
  | "add" -> Add (two_args args)
  | "mul" -> Mul (two_args args)
  | "sub" -> Sub (two_args args)
  | "div" -> Div (two_args args)
  | "eq" -> Eq (two_args args)
  | "lt" -> Lt (two_args args)
  | "gt" -> Gt (two_args args)
  | "le" -> Le (two_args args)
  | "ge" -> Ge (two_args args)
  | "not" -> Not (List.hd args)
  | "and" -> And (two_args args)
  | "or" -> Or (two_args args)
  | "id" -> Id (List.hd args)
  | "call" ->
      let func = StrMap.find "funcs" m |> map_json_lst str_of_json |> List.hd in
      Call (func, args)
  | _ -> failwith "ill-formed bril code"

let eff_of_json (op: string) (m: Raw.t StrMap.t) : eff =
  let labels =
    try StrMap.find "labels" m |> map_json_lst str_of_json
    with Not_found -> []
  in
  let args =
    try StrMap.find "args" m |> map_json_lst str_of_json
    with Not_found -> []
  in
  let funcs =
    try StrMap.find "funcs" m |> map_json_lst str_of_json
    with Not_found -> []
  in
  match op with
  | "jmp" -> Jmp (List.hd labels)
  | "br" ->
      let (lbl1, lbl2) = two_args labels in
      Br (List.hd args, lbl1, lbl2)
  | "call" -> Call (List.hd funcs, args)
  | "print" -> Print (List.hd args)
  | "ret" ->
      begin match args with
      | [] -> Ret None
      | [arg] -> Ret (Some arg)
      | _ -> failwith "ill-formed bril code"
      end
  | _ -> failwith "ill-formed bril code"

let ins_of_json : Raw.t -> ins = function
  | `Assoc ["label", lbl] -> Label (str_of_json lbl)
  | `Assoc l ->
      let m = StrMap.of_list l in
      begin match StrMap.find "op" m |> str_of_json with
      | "const" -> Const (dest_of_json m, value_of_json m)
      | op ->
          begin match StrMap.find_opt "dest" m with
          | None -> Effect (eff_of_json op m)
          | Some _ -> Assign (dest_of_json m, exp_of_json op m)
          end
      end
  | _ -> failwith "ill-formed bril code"

let fn_of_json : Raw.t -> fn = function
  | `Assoc l ->
      let m = StrMap.of_list l in
      { name = StrMap.find "name" m |> str_of_json
      ; args = (try StrMap.find "args" m |> map_json_lst arg_of_json
                with Not_found -> [])
      ; ret_ty = StrMap.find_opt "type" m |> Option.map ty_of_json
      ; instrs = StrMap.find "instrs" m |> map_json_lst ins_of_json
      }
  | _ -> failwith "ill-formed bril code"

let prog_of_json : Raw.t -> prog = function
  | `Assoc ["functions", `List l] ->
      List.map fn_of_json l
  | _ -> failwith "ill-formed bril code"

let string_lit (s: string) : Raw.t =
  `Stringlit ("\"" ^ s ^ "\"")

let rec json_of_ty : ty -> Raw.t = function
  | Int -> string_lit "int"
  | Bool -> string_lit "bool"
  | Param (name, t) -> `Assoc [name, json_of_ty t]

let json_of_arg : arg -> Raw.t =
  fun (name, t) -> `Assoc [("name", string_lit name); ("type", json_of_ty t)]

let json_of_lit : lit -> Raw.t = function
  | Int i -> `Intlit (Int64.to_string i)
  | Bool b -> `Bool b

let json_of_exp ((name, t): dest) (e: exp) : Raw.t =
  let dest_type = [("dest", string_lit name); ("type", json_of_ty t)] in
  let opcode =
    match e with
    | Add _ -> "add"
    | Mul _ -> "mul"
    | Sub _ -> "sub"
    | Div _ -> "div"
    | Eq _ -> "eq"
    | Lt _ -> "lt"
    | Gt _ -> "gt"
    | Le _ -> "le"
    | Ge _ -> "ge"
    | Not _ -> "not"
    | And _ -> "and"
    | Or _ -> "or"
    | Id _ -> "id"
    | Call _ -> "call"
  in
  let op = ["op", string_lit opcode] in
  let arguments =
    match e with
    | Add (x1, x2) | Mul (x1, x2) | Sub (x1, x2) | Div (x1, x2) | Eq (x1, x2)
    | Lt (x1, x2) | Gt (x1, x2) | Le (x1, x2) | Ge (x1, x2) | And (x1, x2)
    | Or (x1, x2) -> [x1; x2]
    | Id x | Not x -> [x]
    | Call (_, xs) -> xs
  in
  let args = ["args", `List (List.map string_lit arguments)] in
  let funcs =
    match e with
    | Call (x, _) -> ["funcs", `List [string_lit x]]
    | _ -> []
  in
  `Assoc (op @ dest_type @ args @ funcs)

let json_of_eff (e: eff) : Raw.t =
  let opcode =
    match e with
    | Jmp _ -> "jmp"
    | Br _ -> "br"
    | Call _ -> "call"
    | Ret _ -> "ret"
    | Print _ -> "print"
    | Nop -> "nop"
  in
  let op = ["op", string_lit opcode] in
  let arguments =
    match e with
    | Ret (Some x) | Br (x, _, _) | Print x -> [x]
    | Call (_, xs) -> xs
    | _ -> [] 
  in
  let args =
    match arguments with
    | [] -> []
    | _ -> ["args", `List (List.map string_lit arguments)]
  in
  let funcs =
    match e with
    | Call (x, _) -> ["funcs", `List [string_lit x]]
    | _ -> []
  in
  let lbls =
    match e with
    | Jmp lbl -> [lbl]
    | Br (_, lbl1, lbl2) -> [lbl1; lbl2]
    | _ -> []
  in
  let labels =
    match lbls with
    | [] -> []
    | _ -> ["labels", `List (List.map string_lit lbls)]
  in
  `Assoc (op @ args @ funcs @ labels)


let json_of_ins : ins -> Raw.t = function
  | Label l -> `Assoc ["label", string_lit l]
  | Const ((name, t), lit) ->
      `Assoc [ ("op", string_lit "const")
             ; ("value", json_of_lit lit)
             ; ("dest", string_lit name)
             ; ("type", json_of_ty t)
             ]
  | Assign (dst, e) -> json_of_exp dst e
  | Effect e -> json_of_eff e


let json_of_fn (f: fn) : Raw.t =
  let assoc =
    [ ("name", string_lit f.name)
    ; ("args", `List (List.map json_of_arg f.args))
    ; ("instrs", `List (List.map json_of_ins f.instrs))
    ]
  in
  match f.ret_ty with
  | None -> `Assoc assoc
  | Some t -> `Assoc (assoc @ ["type", json_of_ty t])

let json_of_prog (p : prog) : Raw.t =
  `Assoc [("functions", `List (List.map json_of_fn p))]

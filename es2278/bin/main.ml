open Lib.Syntax
open Lib.Util

module IntMap = Map.Make(Int)

type block = ins list

type blocks = block IntMap.t

type cfg =
  { entry: int
  ; exit: int
  ; successors: (int list) IntMap.t
  ; blocks: blocks
  }

let is_terminator : eff -> bool = function
  | Ret _ | Jmp _ | Br _ -> true
  | _ -> false

let mk_blocks : ins list -> blocks * int =
  let rec loop (b: block) (k: int) (bs: blocks) : ins list -> blocks * int = function
    | Label s :: instrs ->
        begin match b with
        | [] -> loop [Label s] k bs instrs
        | _ -> loop [Label s] (k + 1) (IntMap.add k (List.rev b) bs) instrs
        end
    | Effect eff :: instrs when is_terminator eff ->
        loop [] (k + 1) (IntMap.add k (List.rev (Effect eff :: b)) bs) instrs
    | instr :: instrs -> loop (instr :: b) k bs instrs
    | [] ->
        begin match b with
        | [] ->
            if IntMap.is_empty bs then
              (IntMap.singleton 0 [], 1)
            else
              (bs, k)
        | _ -> (IntMap.add k (List.rev b) bs, k + 1)
        end
  in
  loop [] 0 IntMap.empty

let mk_successors (bs: blocks) (size: int) : (int list) IntMap.t =
  let lbls =
    IntMap.fold (fun k b acc ->
      match b with
      | Label s :: _ -> StrMap.add s k acc
      | _ -> acc
    ) bs StrMap.empty
  in
  IntMap.mapi (fun k b ->
    match List.rev b with
    | Effect (Jmp lbl) :: _ -> [StrMap.find lbl lbls]
    | Effect (Br (_, lbl1, lbl2)) :: _ ->
        [StrMap.find lbl1 lbls; StrMap.find lbl2 lbls]
    | Effect (Ret _) :: _ -> []
    | _ -> if k + 1 = size then [] else [k + 1]
  ) bs

let compute_cfg (instrs: ins list) : cfg =
  let (bs, size) = mk_blocks instrs in
  let successors = mk_successors bs size in

  let exits =
    IntMap.filter (fun k _ ->
      IntMap.find k successors |> List.is_empty
    ) bs |> IntMap.to_list
  in

  match exits with
  | [(k, _)] -> { entry = 0; exit = k; successors = successors; blocks = bs }
  | _ ->
      { entry = 0
      ; exit = size
      ; successors = IntMap.map (function | [] -> [size] | l -> l) successors
      ; blocks = IntMap.add size [] bs
      }

let () =
  let p = Yojson.Raw.from_file Sys.argv.(1) |> prog_of_json in
  let cfgs = List.map (fun f -> compute_cfg f.instrs) p in
  List.iter (fun cfg ->
    Printf.sprintf "entry: %d" cfg.entry |> print_endline;
    Printf.sprintf "exit: %d" cfg.exit |> print_endline;
    print_endline "blocks:";
    IntMap.iter (fun k b ->
      Printf.sprintf "block: %d" k |> print_endline;
      List.iter (fun i ->
        json_of_ins i |> Yojson.Raw.to_string |> print_endline
      ) b
    ) cfg.blocks;
    print_endline "edges:";
    IntMap.iter (fun k l ->
      List.iter (fun k' ->
        Printf.sprintf "%d -> %d" k k' |> print_endline
      ) l
    ) cfg.successors
  ) cfgs

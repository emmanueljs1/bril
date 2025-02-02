type id = string

type ty =
  | Int
  | Bool
  | Param of (id * ty)

type lit =
  | Int of int64
  | Bool of bool

type lbl = string
type dest = id * ty

type exp =
  | Add of (id * id)
  | Mul of (id * id)
  | Sub of (id * id)
  | Div of (id * id)
  | Eq of (id * id)
  | Lt of (id * id)
  | Gt of (id * id)
  | Le of (id * id)
  | Ge of (id * id)
  | Not of id
  | And of (id * id)
  | Or of (id * id)
  | Id of id
  | Call of (id * id list)

type eff =
  | Jmp of lbl
  | Br of (id * lbl * lbl)
  | Call of (id * id list)
  | Ret of id option
  | Print of id
  | Nop

type ins =
  | Const of (dest * lit)
  | Assign of (dest * exp)
  | Effect of eff
  | Label of lbl

type arg = id * ty

type fn =
  { name: id
  ; args: arg list
  ; ret_ty: ty option
  ; instrs: ins list
  }

type prog = fn list

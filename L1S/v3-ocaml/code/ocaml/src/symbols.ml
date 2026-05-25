open Ast

type sym_type =
  | SymPrim of prim_type
  | SymPtr of sym_type
  | SymFunc of sym_type list * sym_type

type scope = (string, sym_type) Hashtbl.t

type t = {
  scopes: scope list ref;
}

let create () = { scopes = ref [Hashtbl.create 16] }

let enter_scope t = t.scopes := Hashtbl.create 16 :: !(t.scopes)

let exit_scope t = t.scopes := List.tl !(t.scopes)

let insert t name st =
  match !(t.scopes) with
  | h :: _ -> Hashtbl.replace h name st
  | [] -> failwith "no scope"

let lookup t name =
  let rec go = function
    | [] -> None
    | h :: rest ->
      match Hashtbl.find_opt h name with
      | Some st -> Some st
      | None -> go rest
  in
  go !(t.scopes)

let ast_type_to_sym = function
  | TPrim TI32 -> SymPrim TI32
  | TPrim TBool -> SymPrim TBool
  | TPrim TVoid -> SymPrim TVoid
  | TPtr t -> SymPtr (ast_type_to_sym t)

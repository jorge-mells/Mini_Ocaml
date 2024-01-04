let rec foldl f l b = 
  match l with
  | [] -> b
  | h::t -> foldl f t (f h b)
(*Mini_Ocaml*)
type 'b env = (string * 'b) list
(*ASG and CSG using ADTs*)
(*CSG*)
type token = ADD | SUB | MUL | LEQ | DIV | MOD |COLON
           | LP | RP | EQ | ARROW
           | LET | REC | IN | FUN | IF | THEN | ELSE 
           | INT | BOOL
           | VAR of string | ICON of int | BCON of bool | END
(*ASG*)
type op = Add | Sub | Mul | Div | Mod | Leq
type ty = Int | Func of ty * ty | Bool | Tuple of ty * ty
type expr = Icon of int | Bcon of bool | Pair of expr * expr | Var of string
            | Binary of op * expr * expr 
            | Let of string * expr * expr 
            | If of expr * expr * expr
            | Letrec of string * string * ty * ty * expr * expr
            (*Let f (x:int) :int = f x in x is Letrec ("f","x",Int (the type of x),Int (the return type of f), f x, f)*)
            | App of expr * expr
            | Lam of string * ty * expr
            | Bracket of expr
type va = Ival of int | Bval of bool | Tval of va * va | Fval of string * expr * venv | Rval of string * string * expr * venv and venv = va env

(*important functions and types for evaluating environments and closures*)
let rec update k v env = 
  match env with
  | [] -> [(k,v)]
  | (k',v')::t -> if k = k' then (k,v)::t else (k',v')::update k v t
;;

let rec remove k env =
  match env with
  | [] -> []
  | (k',v)::t -> if k' = k then t else (k',v)::remove k t

let rec lookup k env =
  match env with
  | [] -> None
  | (k',v')::t -> if k = k' then Some v' else lookup k t


(*type checking*)
type tenv = ty env

let rec check_ty (tenv: tenv) expr =
  let ty_eq e1 e2 = match e1, e2 with 
                    | Some t1, Some t2 -> t1 = t2 
                    | _ -> false in
  match expr with
  | Bcon _ -> Some Bool
  | Icon _ -> Some Int
  | Var x -> lookup x tenv
  | Bracket x -> check_ty tenv x
  | Pair (x,y) -> (match check_ty tenv x with 
                   | None -> None 
                   | Some x -> (match check_ty tenv y with 
                                | None -> None 
                                | Some y -> Some (Tuple (x,y))))
  | Binary (op,e1,e2) -> let t1 = check_ty tenv e1 in
                            let t2 = check_ty tenv e2 in
                            if ty_eq t1 (Some Int) 
                            && ty_eq t2 (Some Int) 
                            then Some (match op with 
                                       | Leq -> Bool
                                       | _ -> Int)
                            else None
  | Let (id,e1,e2) -> let type_e1 = check_ty tenv e1 
                      in (match type_e1 with 
                          | None -> None 
                          | Some x -> let new_tenv = update id x tenv 
                                      in check_ty new_tenv e2)
  | If (e1,e2,e3) -> let t1 = check_ty tenv e1 
                     in let t2 = check_ty tenv e2
                     in let t3 = check_ty tenv e3 
                     in if ty_eq t1 (Some Bool) && ty_eq t2 t3 then t2 else None
  | Lam (id,ty,e) -> let new_tenv = update id ty tenv 
                      in (match check_ty new_tenv e with 
                          | None -> None 
                          | Some x -> Some (Func (ty,x)))
  | App (e1,e2) -> let type_e1 = check_ty tenv e1 
                    in (match type_e1 with 
                        | Some (Func (x,y)) -> if check_ty tenv e2 = Some x then Some y else None 
                        | _ -> None)
  | Letrec (id1,id2,ty1,ty2,e1,e2) -> let types = foldl (fun (a,c) b -> update a c b) ([(id2,ty1);(id1,Func (ty1,ty2))]) (tenv) in let check_e1 = check_ty types e1 
                                          in (match check_e1 with 
                                              | None -> None 
                                              | Some _ -> check_ty (remove id2 types) e2)

                                              
(*evaluation*)

let rec eval (venv: venv) expr =
  match expr with
  | Bcon x -> (Bval x)
  | Icon x -> (Ival x)
  | Bracket x -> eval venv x
  | Pair (e1,e2) -> Tval (eval venv e1, eval venv e2)
  | Var x -> (match lookup x venv with 
              | None -> failwith ("no value for var " ^ x) 
              | Some x -> x)
  | Binary (op,e1,e2) -> let val_e1 = eval venv e1 
                         in let val_e2 = eval venv e2 
                         in (match op, val_e1, val_e2 with
                            | Add, Ival v1, Ival v2 -> Ival (v1 + v2)
                            | Sub, Ival v1, Ival v2 -> Ival (v1 - v2)
                            | Mul, Ival v1, Ival v2 -> Ival (v1 * v2)
                            | Div, Ival v1, Ival v2 -> Ival (v1 / v2)
                            | Mod, Ival v1, Ival v2 -> Ival (v1 mod v2)
                            | Leq, Ival v1, Ival v2 -> Bval (v1 <= v2)
                            | _ -> failwith "error")
  | Let (id,e1,e2) -> let val_e1 = eval venv e1 
                      in eval (update id val_e1 venv) e2
  | If (e1,e2,e3) -> (match eval venv e1 with 
                     | Bval true -> eval venv e2
                     | Bval false -> eval venv e3
                     | _ -> failwith ("error"))
  | Lam (id,_,e) -> Fval (id,e,venv)
  | App (e1,e2) -> let closure = eval venv e1  in
                   let eval_e2 = eval venv e2 
                   in (match closure with
                      | Fval (x,e,venv') -> let new_venv = update x eval_e2 venv' in eval new_venv e 
                      | Rval (f,x,e,venv') -> let new_venv1 = update x eval_e2 venv' 
                      in let new_venv2 = update f closure new_venv1 in eval new_venv2 e
                      | _ -> failwith "no value for function")
  | Letrec (id1,id2,_,_,e1,e2) -> let closure = Rval (id1,id2,e1,venv) 
                                      in let new_venv = update id1 closure venv
                                      in eval new_venv e2 


(*lexer*)
let lex s = 
  let whitespace c = match c with
    | ' ' | '\n' | '\t' -> true
    | _ -> false in
  let lc_letter c = match c with
    | 'a' .. 'z' -> true
    | _ -> false in
  let id_rest c = match c with
    | 'A' .. 'Z' | 'a' .. 'z' | '_' | '\'' | '0' .. '9' -> true
    | _ -> false in
  let digit c = match c with
    | '0' .. '9' -> true
    | _ -> false in
  let rec to_int s = match s with
    | "0" -> 0
    | "1" -> 1
    | "2" -> 2
    | "3" -> 3
    | "4" -> 4
    | "5" -> 5
    | "6" -> 6
    | "7" -> 7
    | "8" -> 8
    | "9" -> 9 
    | _ -> failwith "invalid integer" in
  let rec to_ints s acc = if String.length s = 0 then acc 
                          else to_ints (String.sub s 1 (String.length s - 1)) (acc * 10 + to_int (String.sub s 0 1)) in
                          
  let n = String.length s in
  let get i = String.get s i in
  
  let rec lex_id i j l = 
    if j >= n then id_classify (String.sub s i (j - i)) j l else 
    match get j with 
    | c when id_rest c -> lex_id i (j + 1) l
    | c when digit c -> lex_id i (j + 1) l
    | _ -> id_classify (String.sub s i (j - i)) (j) l 
  
  and id_classify s i l =
    let t = match s with
    | c when digit (String.get c 0) -> ICON (to_ints c 0)
    | "mod" -> MOD
    | "let" -> LET
    | "rec" -> REC
    | "fun" -> FUN
    | "in" -> IN
    | "if" -> IF
    | "then" -> THEN
    | "else" -> ELSE
    | "true" -> BCON true
    | "false" -> BCON false
    | "int" -> INT
    | "bool" -> BOOL
    | _ -> VAR s
    in work i (t :: l)
  
  and work i l = 
    if i >= n then (END::l) else
    if whitespace (get i) then work (i + 1) l else
    match get i with
    | '+' -> work (i + 1) (ADD :: l)
    | '-' -> if i + 1 < n && get (i + 1) = '>'
             then work (i + 2) (ARROW :: l)
             else work (i + 1) (SUB :: l)
    | '*' -> work (i + 1) (MUL :: l)
    | '(' -> work (i + 1) (LP :: l)
    | ')' -> work (i + 1) (RP :: l)
    | '=' -> work (i + 1) (EQ :: l)
    | '/' -> work (i + 1) (DIV :: l)
    | ':' -> work (i + 1) (COLON :: l)
    | '<' -> if i + 1 < n && get (i + 1) = '=' 
             then work (i + 2) (LEQ :: l)
             else failwith "illegal <"
    | c when lc_letter c -> lex_id i i l
    | c when digit c -> lex_id i i l
    | _ -> failwith "illegal character"
  in List.rev (work 0 []) 

(*parser*)

(*operator precedence parser*)
let parse_binary parse_simpl parse_op ts = 
  let rec binary p (l, ts) =   
    match parse_op ts with
      | None -> (l, ts)
      | Some (op, lp, rp, ts') -> 
        if lp < p then (l, ts) else
          let r, ts = binary rp (parse_simpl ts')
          in binary p (op l r, ts) 
in binary 0 (parse_simpl ts)

let expect_id ts = match ts with
  | VAR x :: ts -> x, ts
  | _ -> failwith "identifier expected"
  
let expect t ts = match ts with
  | t' :: ts when t = t' -> ts
  | _ -> failwith "parse error"

let parse_op ts =
  let create op l r = Binary (op, l, r) in
  let create_f l r = App (l,r) in
  match ts with
  | LEQ :: ts -> Some (create Leq, 0, 1, ts)
  | ADD :: ts -> Some (create Add, 2, 3, ts)
  | SUB :: ts -> Some (create Sub, 2, 3, ts)
  | DIV :: ts -> Some (create Div, 4, 5, ts)
  | MUL :: ts -> Some (create Mul, 6, 7, ts)
  | MOD :: ts -> Some (create Mod, 6, 7, ts)
  | VAR x :: ts -> Some (create_f, 8, 9, VAR x :: ts)
  | ICON x :: ts -> Some (create_f, 8, 9, ICON x :: ts)
  | BCON x :: ts -> Some (create_f, 8, 9, BCON x :: ts)
  | LP :: ts -> Some (create_f, 8, 9, LP :: ts)
  | _ -> None

(* function type parser *)
let parse_binary_ty parse_simpl parse_op ts = 
  let rec binary p (l, ts) =   
    match parse_op ts with
      | None -> (l, ts)
      | Some (op, lp, rp, ts') -> 
        if lp < p then (l, ts) else
          let r, ts = binary rp (parse_simpl ts')
          in binary p (op l r, ts) 
  in binary 0 (parse_simpl ts)

(*type parser*)
let parse_op_ty ts =
  let create_ty l r = Func (l,r) in
  match ts with
  | ARROW :: ts -> Some (create_ty, 0, -1, ts)
  | _ -> None
let rec parse_ty ts = parse_binary_ty parse_simple_expr parse_op_ty ts and parse_simple_expr ts = match ts with
  | INT :: ts -> Int, ts
  | BOOL :: ts -> Bool, ts
  | _ -> failwith "not a type"

(* expr parser *)
let rec parse_expr ts = parse_binary parse_simple_expr parse_op ts
and parse_simple_expr ts = match ts with
  | LP :: ts -> let e ,ts = parse_expr ts in
                let ts = expect RP ts in
                Bracket e, ts
  | ICON c :: ts -> Icon c, ts 
  | BCON c :: ts -> Bcon c, ts
  | VAR x  :: ts -> Var x, ts
  | LET :: REC :: ts -> let f, ts = expect_id ts in
                        let ts = expect LP ts in
                        let x, ts = expect_id ts in
                        let ts = expect COLON ts in
                        let ty1, ts = parse_ty ts in
                        let ts = expect RP ts in
                        let ts = expect COLON ts in
                        let ty2, ts = parse_ty ts in
                        let ts = expect EQ ts in
                        let e1, ts = parse_expr ts in
                        let ts = expect IN ts in
                        let e2, ts = parse_expr ts in
                        Letrec (f,x,ty1,ty2,e1,e2), ts
  | LET    :: ts -> let x,  ts = expect_id ts in
                    let     ts = expect EQ ts in
                    let e1, ts = parse_expr ts in
                    let     ts = expect IN ts in
                    let e2, ts = parse_expr ts in
                    Let (x, e1, e2), ts
  | IF     :: ts -> let e1, ts = parse_expr ts in
                    let     ts = expect THEN ts in
                    let e2, ts = parse_expr ts in
                    let     ts = expect ELSE ts in
                    let e3, ts = parse_expr ts in
                    If (e1, e2, e3), ts
  | FUN :: ts -> let ts = expect LP ts in
                 let x, ts = expect_id ts in
                 let ts = expect COLON ts in
                 let ty, ts = parse_ty ts in
                 let ts = expect RP ts in
                 let ts = expect ARROW ts in
                 let e, ts = parse_expr ts in
                 Lam (x,ty,e), ts
  | _ -> failwith "syntax error"

  (*checking the end of input*)
let parse_f (exp,ts) = match ts with | END::_ -> exp | _ -> failwith "incomplete parsing"

(*final parser*)
let final_parse exp = parse_f (parse_expr exp)

(* ocaml expression evaluator *)
let evaluate_ocaml_expression s = 
  let lexed = lex s 
  in let parsed = parse_f (parse_expr lexed)
      in let type_checked = check_ty [] parsed 
          in match type_checked with | None -> failwith "type error" | Some _ -> eval [] parsed

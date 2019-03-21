open Core
open Base

type sExpr = 
    | Atom of string
    | List of sExpr list

type expr = 
    | Num of float
    | Var of string
    | Op1 of string*expr
    | Op2 of string*expr*expr
    | Fct of string * expr list

type statement = 
    | Assign of string*expr
    | Return of expr
    | Expr of expr
    | If of expr*statement list * statement list
    | While of expr*statement list
    | For of statement*expr*statement*statement list
    | FctDef of string * string list * statement list 

type eval_status =
    | Normal
    | Ret of float
    | Continue
    | Break
    | Err of string

type block = statement list 

type func = string list * block

type scope = (Base.string, float, String.comparator_witness) Map.t 

type env = scope * scope list * (Base.string, func, String.comparator_witness) Map.t

let empty_env = (Map.empty (module String), [], Map.empty (module String))

let find_or_zero (scp: scope) (var: string): float = 
    match Map.find scp var with
    | Some(value) -> value
    | None -> 0.

(* convert boolean to float *)
let btf (b: bool): float = if b then 1. else 0.

(* convert float to boolean *)
let ftb (f: float): bool = f <>. 0.

let get_var (_k: string) (_e:env): float = 
    let (globals, locals, _) = _e in
    match locals with
    | [] -> find_or_zero globals _k
    | current :: rest ->
            match Map.find current _k with
            | Some(value) -> value
            | None -> find_or_zero globals _k

let put_var (_k: string) (_v: float) (_e: env): env =
    let (globals, locals, funcs) = _e in
    (* let updateOrElse (scp: scope): *)
    match locals with
    | [] -> ((Map.update globals _k ~f:(fun x -> _v )), locals, funcs)
    | current :: rest ->  (Map.update globals _k ~f:(fun x-> _v), locals, funcs)

let add_func (name: string) (f: func) (_env: env): env =
    let (globals, locals, funcs) = _env in
    globals, locals, (Map.update funcs name ~f:(fun x-> f))

let rec call_func (name: string) (args: float list) (_env: env): (float*env, string) Result.t=
    let (globals, locals, funcs) = _env in
    match Map.find funcs name with
    | None -> Error("Could not function with name " ^ name)
    | Some(argnames, body) -> (
        match List.zip argnames args with
        | Some(assigns) -> 
            ( match Map.of_alist (module String) assigns with
            | `Ok(newlocals) -> 
                (let new_env, status = evalBlock body (globals, newlocals :: locals, funcs) in
                match status with
                | Normal -> Ok(0., new_env)
                | Ret(value) -> Ok(0., new_env)
                | Err(msg) -> Error(msg)
                | _ -> Error("Error: Continue/Break outside of for loop.")
                )
            | _ -> Error("Error: Duplicate arguments.") )
        | None -> Error("Error: Incorrect number of arguments."))

and collect_args (args: expr list) (_env: env): (float list*env, string) Result.t=
    match args with
    | [] -> Ok([], _env)
    | arg :: tail -> 
        (match evalExpr arg _env with
        | Error(_) as err -> err
        | Ok(result, new_env) -> 
            (* not doing tail recursion bc on a crunch *)
            match collect_args tail new_env with
            | Error(_) as err -> err
            | Ok( other_results, new_env ) -> Ok(result :: other_results, new_env ))

and evalOp1 (op: string) ( _e: expr) (_env: env): (float*env, string) Result.t =
    match evalExpr _e _env with
    | Error(_) as err -> err
    | Ok(value, _new_env) ->
            match op with
            | _ -> Error("Unrecognized operator '" ^ op ^ "'")

and evalOp2(op: string) (_e1: expr) (_e2: expr) (_env: env): (float*env, string) Result.t =
    match evalExpr _e1 _env with
    | Error(_) as err -> err
    | Ok(val1, _new_env) ->
        match evalExpr _e2 _new_env with
        | Error(_) as err -> err
        | Ok(val2, _new_env) -> 
            match op with
            | "+"  -> Ok(val1 +. val2, _new_env)
            | "-"  -> Ok(val1 -. val2, _new_env)
            | "*"  -> Ok(val1 *. val2, _new_env)
            | "/"  -> Ok(val1 /. val2, _new_env)
            | "^"  -> Ok(val1 **. val2, _new_env)
            | "==" -> Ok( (btf ( val1 =. val2)), _new_env)
            | "!=" -> Ok( (btf ( val1 <>. val2)), _new_env)
            | "<=" -> Ok( (btf ( val1 <=. val2)), _new_env)
            | ">=" -> Ok( (btf ( val1 >=. val2)), _new_env)
            | "<"  -> Ok( (btf ( val1 <. val2)), _new_env)
            | ">"  -> Ok( (btf ( val1 >. val2)), _new_env)
            | "&&" -> Ok( btf ((ftb val1) && (ftb val2)), _new_env)
            | "||" -> Ok( btf ((ftb val1) || (ftb val2)), _new_env)
            | _ -> Error("Unrecognized operator '" ^ op ^ "'")

and evalExpr (_e: expr) (_env: env): (float*env, string) Result.t = 
    match _e with 
    | Num(value) -> Ok(value, _env)
    | Op1(op, exp) -> (evalOp1 op exp _env)
    | Op2(op, exp1, exp2) -> (evalOp2 op exp1 exp2 _env)
    | Var(id) -> Ok((get_var id _env), _env)
    | Fct(name, args) -> (
        match collect_args args _env with
        | Error(_) as err -> err
        | Ok(args, new_env) -> call_func name args new_env )

and evalBlock (b: block) (_env: env): env*eval_status =
    match b with
    | [] -> _env, Normal
    | stmt :: rest ->
        match evalStatement stmt _env with
        | new_env, Normal -> (evalBlock rest new_env)
        (* if we get a non-normal status, move it up *)
        | new_env, (_ as abnormal) -> new_env, abnormal

and evalStatement (s: statement) (_env: env): env*eval_status =
    match s with 
    | Assign(_k, _e) -> 
        (match evalExpr _e _env with
        | Ok(value, new_env) -> (put_var _k value new_env), Normal
        | Error(msg) -> _env, Err(msg) )
    | Return(_e) ->
        (match evalExpr _e _env with
        | Ok(value, new_env) -> new_env, Ret(value)
        | Error(msg) -> _env, Err(msg) )
    | Expr(_e) ->
        (match evalExpr _e _env with
        | Ok(value, new_env) -> (printf "%F" value; new_env, Normal)
        | Error(msg) -> _env, Err(msg) )
    | If(_e, codeT, codeF) ->
        (match evalExpr _e _env with
        | Error(msg) -> _env, Err(msg)
        | Ok(value, new_env) -> (if (ftb value) then evalBlock codeT new_env else evalBlock codeF new_env) )
    | While(cond, body) ->
        (match evalExpr cond _env with
        | Error(msg) -> _env, Err(msg)
        | Ok(value, new_env) ->
            if (ftb value) then 
                (let new_env, status = evalBlock body new_env in
                match status with
                (* in the normal/ continue case, simply recurse *)
                | Normal|Continue -> evalStatement s new_env 
                (* handle the break by leaving *)
                | Break -> new_env, Normal
                | _ as stat -> new_env, stat)
            else new_env, Normal )
    
    | For(pre, cond, post, body) ->
        ( let new_env, stat = evalStatement pre _env in
        match stat with Normal -> (
            match evalExpr cond new_env with
            | Error(msg) -> new_env, Err(msg)
            | Ok(value, new_env) ->
                if (ftb value) then
                    (let new_env, status = evalBlock body new_env in
                    match status with 
                    (* in the normal/ continue case, simply recurse *)
                    | Normal|Continue -> 
                        (* run the post statement and check, then recurse if good*)
                        (let new_env, status = evalStatement post new_env in 
                        match status with
                        | Normal -> evalStatement s new_env
                        | _ as abnormal -> new_env, abnormal)
                    (* handle the break by leaving *)
                    | Break -> new_env, Normal
                    | _ as stat -> new_env, stat)
                else new_env, Normal)
        | _ -> new_env, stat )
    | FctDef(name, args, body) -> (add_func name (args, body) _env), Normal
;;

let evalCode (_code: block) (_env: env): unit = 
    match evalBlock _code _env with
    | _, Normal -> ()
    | _, Ret(value) -> Stdio.print_endline "Error: Return outside of function."
    | _, Continue | _, Break -> Stdio.print_endline "Error: Continue/Break outside of for loop."
    | _, Err(msg) -> Stdio.print_endline msg
    ;;
    

(* Test expressions *)
let%expect_test "addition" = 
    (match evalExpr (Op2("+", (Op2("+", Num(1.), Num(1.))), Op2("-", Num(12.), Num(7.)))) empty_env with
    | Ok(value, _) -> printf "%F" value
    | Error(msg) -> Stdio.print_endline msg);
    [%expect {| 7. |}]
    ;;

let%expect_test "multiplication" = 
    (match evalExpr (Op2("/", Op2("*", Num(80.), Num(3.)), Op2("*", Num(6.), Num(4.)))) empty_env with
    | Ok(value, _) -> printf "%F" value
    | Error(msg) -> Stdio.print_endline msg);
    [%expect {| 10. |}]
    ;;

let%expect_test "exponentation" = 
    (match evalExpr (Op2("^", Num(2.), Op2("*", Num(0.5), Num(4.) ))) empty_env with
    | Ok(value, _) -> printf "%F" value
    | Error(msg) -> Stdio.print_endline msg);
    [%expect {| 4. |}]
    ;;

let%expect_test "putting_var" = 
    let my_env = put_var "abc" 37. empty_env in
    (match evalExpr (Var("abc")) my_env with
    | Ok(value, _) -> printf "%F" value
    | Error(msg) -> Stdio.print_endline msg);
    [%expect {| 37. |}]
    ;;

let%expect_test "equal" = 
    (match evalExpr (Op2("!=", Num(2.), Num(2.) )) empty_env with
    | Ok(value, _) -> printf "%F" value
    | Error(msg) -> Stdio.print_endline msg);
    [%expect {| 0. |}]
    ;;

let%expect_test "and" = 
    (match evalExpr (Op2("&&", Num(0.), Num(1.) )) empty_env with
    | Ok(value, _) -> printf "%F" value
    | Error(msg) -> Stdio.print_endline msg);
    [%expect {| 0. |}]
    ;;

let%expect_test "or" = 
    (match evalExpr (Op2("||", Num(0.), Num(1.) )) empty_env with
    | Ok(value, _) -> printf "%F" value
    | Error(msg) -> Stdio.print_endline msg);
    [%expect {| 1. |}]
    ;;

(* statement tests *)
let testexpr = Expr(Op2("/", Op2("*", Num(80.), Num(3.)), Op2("*", Var("myvar"), Num(4.))))
let%expect_test "statment_expression" =
    let my_env = put_var "myvar" (-6.) empty_env in
    (match evalStatement testexpr my_env with
    | _, Normal -> ()
    | _, _ -> Stdio.print_endline "Invalid status code");
    [%expect {| -10. |}]

let assign = Assign("myvar", Num(-6.))
let%expect_test "state_assign" =
    let myexpr = Expr(Var("myvar")) in
    (* evaluate the assignment *)
    let _new_env, _ = evalStatement assign empty_env in
    (* evaluate the retrieval *)
    let _, _ = evalStatement myexpr _new_env in
    [%expect {| -6. |}]

let%expect_test "block_test" =
    let b: block = [assign ; testexpr] in 
    let _ = evalBlock b empty_env in
    [%expect {| -10. |}]
;;

let%expect_test "block_return" =
    let b: block = [assign ; Return(Num(77.)); testexpr] in 
    (match evalBlock b empty_env with
    | _ , Ret(value) -> printf "%F" value
    | _ , _ -> Stdio.print_endline "error");
    [%expect {| 77. |}]
;;

let%expect_test "if_else" =
    let b: block = [Assign("var2", Num(3.)); If( Op2("!=", Var("var2"), Num(3.)) , [Expr(Op2("+", Var("var2"), Num(1.)))], [Expr(Op2("-", Var("var2"), Num(1.)))] ) ] in 
    let _ = evalBlock b empty_env in
    [%expect {| 2. |}]
;;

let%expect_test "while" =
    let b: block = [While( Op2( "<", Var("abc"), Num(10.) ), [Assign("abc", Op2("+", Var("abc"), Num(3.)))] ) ; Expr(Var("abc")) ] in
    let _ = evalBlock b empty_env in
    [%expect {| 12. |}]

let for_block: block = [
    Assign("result", Num(1.)) ;
    For( Assign("i", Num(1.)), 
         Op2("<", Var("i"), Num(9.)),
         Assign("i", Op2("+", Var("i"), Num(1.))),
         [Assign("result", Op2("+", Var("result"), Op2("^", Num(2.), Var("i")) ) );
         Assign("i", Op2("+", Var("i"), Num(1.)));
         Expr(Var("i"))]);
    Expr(Var("result"))]

let%expect_test "For" =
    evalCode for_block empty_env;
    [%expect {| 12. |}]

let p1: block = [
        Assign("v", Num(1.0));
        Expr(Var("v")) 
]

let%expect_test "p1" =
    evalCode p1 empty_env;
    [%expect {| 1. |}]

(*
    v = 1.0;
    if (v>10.0) then
        v = v + 1.0
    else
        for(i=2.0; i<10.0; i++) {
            v = v * i
        }
    v   // display v
*)
(*
let p2: block = [
    Assign("v", Num(1.0));
    If(
        Op2(">", Var("v"), Num(10.0)), 
        [Assign("v", Op2("+", Var("v"), Num(1.0)))], 
        [For(
            Assign("i", Num(2.0)),
            Op2("<", Var("i"), Num(10.0)),
            Expr(Op1("++a", Var("i"))),
            [
                Assign("v", Op2("*", Var("v"), Var("i")))
            ]
        )]
    );
    Expr(Var("v"))
] 


let%expect_test "p2" =
    evalCode p2 empty_env; 
    [%expect {| 3628800. |}] *)

(*  Fibbonaci sequence
    define f(x) {
        if (x<1.0) then
            return (1.0)
        else
            return (f(x-1)+f(x-2))
    }

    f(3)
    f(5)
 *)
(*
let p3: block = 
    [
        FctDef("f", ["x"], [
            If(
                Op2("<", Var("x"), Num(1.0)),
                [Return(Num(1.0))],
                [Return(Op2("+",
                    Fct("f", [Op2("-", Var("x"), Num(1.0))]),
                    Fct("f", [Op2("-", Var("x"), Num(1.0))])
                ))])
        ]);
        Expr(Fct("f", [Num(3.0)]));
        Expr(Fct("f", [Num(5.0)]));
    ]
*)
(*
let%expect_test "p3" =
    evalCode p3 []; 
    [%expect {| 
        2. 
        5.      
    |}]
*)
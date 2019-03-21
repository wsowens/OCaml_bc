open Core
open Base

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
    | Continue
    | Break
    | Pass

type eval_status =
    | Normal
    | Ret of float
    | Cont
    | Brk
    | Err of string

type block = statement list 

type func = string list * block

type scope = (Base.string, float, String.comparator_witness) Map.t 

type env = scope * scope list * (Base.string, func, String.comparator_witness) Map.t

(*fresh env to be used for starting blocks of code *)
let empty_env = (Map.empty (module String), [], Map.empty (module String))

(* finds a value in a scope, or returns zero *)
let find_or_zero (scp: scope) (var: string): float = 
    match Map.find scp var with
    | Some(value) -> value
    | None -> 0.

(* convert boolean to float (in a c-like sense) *)
let btf (b: bool): float = if b then 1. else 0.

(* convert float to boolean (in a c-like sense) *)
let ftb (f: float): bool = f <>. 0.

(* get a variable from an environment *)
let get_var (_k: string) (_e:env): float = 
    let (globals, locals, _) = _e in
    match locals with
    (*if no locals exist, look in globals *)
    | [] -> find_or_zero globals _k
    (*if locals do exist, look in locals *)
    | current :: _ ->
            match Map.find current _k with
            (* return a value if we find it in locals *) 
            | Some(value) -> value
            (*otherwise refer to globals *)
            | None -> find_or_zero globals _k

let put_var (_k: string) (_v: float) (_e: env): env =
    let (globals, locals, funcs) = _e in
    (* function to update map with given value *)
    let update_map (m: scope): scope = Map.update m _k ~f:(fun _ -> _v ) in
    match locals with
    (* if no locals exist, put it in globals *)
    | [] -> ((update_map globals), locals, funcs)
    | current :: rest -> 
        (* otherwise, check locals first *)
        (match Map.find current _k with
        | Some(_) -> ( globals, (update_map current) :: rest , funcs)
        | None -> (update_map globals), locals, funcs) 

let add_func (name: string) (f: func) (_env: env): env =
    let (globals, locals, funcs) = _env in
    globals, locals, (Map.update funcs name ~f:(fun _ -> f))

let rec call_func (name: string) (args: float list) (_env: env): (float*env, string) Result.t=
    let (globals, locals, funcs) = _env in
    match Map.find funcs name with
    | None -> Error("Error: Could not find function with name " ^ name)
    | Some(argnames, body) -> (
        match List.zip argnames args with
        | Some(assigns) -> 
            (* convert the name, value pairs into a proper scope *)
            ( match Map.of_alist (module String) assigns with
            | `Ok(newlocals) -> 
                (* call the func with a new env containing the new local scope*)
                (let new_env, status = evalBlock body (globals, newlocals :: locals, funcs) true in
                match new_env with
                | globals, _ :: locals, funcs -> 
                    (* if no err, return an env without the newlocal scope popped off*)
                    (match status with
                    | Normal -> Ok(0., (globals, locals, funcs))
                    | Ret(value) -> Ok(value, (globals, locals, funcs))
                    | Err(msg) -> Error(msg)
                    | _ -> Error("Error: Continue/Break outside of for/while loop."))
                | _, [], _ -> Error("Stack integrity failure.")
                )
            | _ -> Error("Error: Duplicate arguments.") )
        | None -> Error("Error: Incorrect number of arguments."))

(* unoptimized function for evaluating the list of expressions *)
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
    | Ok(value, new_env) ->
            match op with
            | "!" -> Ok(value |> ftb |> not |> btf, new_env)
            | "++a" -> 
                (match _e with 
                | Var(id) -> Ok(value +. 1., (put_var id (value +. 1.) new_env))
                | _ -> Error("Error: Preincrement must be applied to Var.") )
            | "--a" -> 
                (match _e with 
                | Var(id) -> Ok(value -. 1., (put_var id (value -. 1.) new_env))
                | _ -> Error("Error: Predecrement must be applied to Var.") )
            | "a++" -> 
                (match _e with 
                | Var(id) -> Ok(value, (put_var id (value +. 1.) new_env))
                | _ -> Error("Error: Postincrement must be applied to Var.") )
            | "a--" ->
                (match _e with 
                | Var(id) -> Ok(value, (put_var id (value -. 1.) new_env))
                | _ -> Error("Error: Postdecrement must be applied to Var.") )
             | _ -> Error("Error: Unrecognized operator '" ^ op ^ "'")

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

and evalBlock (b: block) (_env: env) (do_print: bool): env*eval_status =
    match b with
    | [] -> _env, Normal
    | stmt :: rest ->
        match evalStatement stmt _env do_print with
        | new_env, Normal -> (evalBlock rest new_env do_print)
        (* if we get a non-normal status, move it up *)
        | new_env, (_ as abnormal) -> new_env, abnormal

and evalStatement (s: statement) (_env: env) (do_print: bool): env*eval_status =
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
        | Ok(value, new_env) -> (if do_print then printf "%F\n" value; new_env, Normal)
        | Error(msg) -> _env, Err(msg) )
    | If(_e, codeT, codeF) ->
        (match evalExpr _e _env with
        | Error(msg) -> _env, Err(msg)
        | Ok(value, new_env) -> (if (ftb value) then evalBlock codeT new_env do_print else evalBlock codeF new_env do_print) )
    | While(cond, body) ->
        (match evalExpr cond _env with
        | Error(msg) -> _env, Err(msg)
        | Ok(value, new_env) ->
            if (ftb value) then 
                (let new_env, status = evalBlock body new_env do_print in
                match status with
                (* in the normal/ continue case, simply recurse *)
                | Normal|Cont -> evalStatement s new_env do_print
                (* handle the break by leaving *)
                | Brk -> new_env, Normal
                | _ as stat -> new_env, stat)
            else new_env, Normal )
    
    | For(pre, cond, post, body) ->
        ( let new_env, stat = evalStatement pre _env false in
        match stat with Normal -> (
            match evalExpr cond new_env with
            | Error(msg) -> new_env, Err(msg)
            | Ok(value, new_env) ->
                if (ftb value) then
                    (let new_env, status = evalBlock body new_env do_print in
                    match status with 
                    (* in the normal/ continue case, simply recurse *)
                    | Normal|Cont -> 
                        (* run the post statement and check, then recurse if good*)
                        (let new_env, status = evalStatement post new_env false in 
                        match status with
                        | Normal -> ( evalStatement (For(Pass, cond, post, body)) new_env do_print)
                        | _ as abnormal -> new_env, abnormal)
                    (* handle the break by leaving *)
                    | Brk -> new_env, Normal
                    | _ as stat -> new_env, stat)
                else new_env, Normal)
        | _ -> new_env, stat )
    | FctDef(name, args, body) -> (add_func name (args, body) _env), Normal
    | Break -> _env, Brk
    | Continue -> _env, Cont
    | Pass -> _env, Normal
;;

let evalCode (_code: block) (_env: env): unit = 
    match evalBlock _code _env true with
    | _, Normal -> ()
    | _, Ret(_) -> Stdio.print_endline "Error: Return outside of function."
    | _, Cont | _, Brk -> Stdio.print_endline "Error: Continue/Break outside of for/while loop."
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
    (match evalStatement testexpr my_env true with
    | _, Normal -> ()
    | _, _ -> Stdio.print_endline "Invalid status code");
    [%expect {| -10. |}]

let assign = Assign("myvar", Num(-6.))
let%expect_test "state_assign" =
    let myexpr = Expr(Var("myvar")) in
    (* evaluate the assignment *)
    let _new_env, _ = evalStatement assign empty_env true in
    (* evaluate the retrieval *)
    let _, _ = evalStatement myexpr _new_env true in
    [%expect {| -6. |}]

let%expect_test "block_test" =
    let b: block = [assign ; testexpr] in 
    let _ = evalBlock b empty_env true in
    [%expect {| -10. |}]
;;

let%expect_test "block_return" =
    let b: block = [assign ; Return(Num(77.)); testexpr] in 
    (match evalBlock b empty_env true with
    | _ , Ret(value) -> printf "%F" value
    | _ , _ -> Stdio.print_endline "error");
    [%expect {| 77. |}]
;;

let%expect_test "if_else" =
    let b: block = [Assign("var2", Num(3.)); If( Op2("!=", Var("var2"), Num(3.)) , [Expr(Op2("+", Var("var2"), Num(1.)))], [Expr(Op2("-", Var("var2"), Num(1.)))] ) ] in 
    let _ = evalBlock b empty_env true in
    [%expect {| 2. |}]
;;

let%expect_test "while" =
    let b: block = [While( Op2( "<", Var("abc"), Num(10.) ),
                        [Assign("abc", Op2("+", Var("abc"), Num(3.)))] ); 
                    Expr(Var("abc")) ] in
    let _ = evalBlock b empty_env true in
    [%expect {| 12. |}]

let for_block: block = [
    Assign("result", Num(1.)) ;
    For( Assign("i", Num(1.)), 
         Op2("<=", Var("i"), Num(9.)),
         Assign("i", Op2("+", Var("i"), Num(1.))),
         [Assign("result", Op2("+", Var("result"), Op2("^", Num(2.), Var("i")) ) )]
         );
    Expr(Var("result"))]

let%expect_test "For" =
    evalCode for_block empty_env;
    [%expect {| 1023. |}]

let break_block: block = [
    Assign("result", Num(1.)) ;
    For( Assign("i", Num(1.)), 
         Op2("<=", Var("i"), Num(9.)),
         Assign("i", Op2("+", Var("i"), Num(1.))),
         [ If( Op2("==", Var("i"), Num(3.)),
            [ Break ],
            [ Assign("result", Op2("+", Var("result"), Op2("^", Num(2.), Var("i")) ) ) ] )
         ]
         );
    Expr(Var("result"))]

let%expect_test "Break" =
    evalCode break_block empty_env;
    [%expect {| 7. |}]

let continue_block: block = [
    Assign("result", Num(1.)) ;
    For( Assign("i", Num(1.)), 
         Op2("<=", Var("i"), Num(9.)),
         Assign("i", Op2("+", Var("i"), Num(1.))),
         [ If( Op2("==", Var("i"), Num(3.)),
            [ Continue ],
            [ Assign("result", Op2("+", Var("result"), Op2("^", Num(2.), Var("i")) ) ) ] )
         ]
         );
    Expr(Var("result"))]

let%expect_test "Continue" =
    evalCode continue_block empty_env;
    [%expect {| 1015. |}]

let factorial: block = [
    FctDef("fact", ["x"], [
            (Assign("randomglobal", Op2("+", Num(42.), Var("x"))));
            (Assign("x", Var("x")));
            If ( (Op2("<", Var("x"), Num(2.))),
                [Return(Num(1.))],
                [Return( 
                    Op2("*", Var("x"), Fct("fact", [Op2("-", Var("x"), Num(1.))] )))
                ]
            )
        ]
    );
    Expr(Fct("fact", [Num(5.)]));
    Expr(Var("randomglobal"));
    Expr(Var("x")) ]

let%expect_test "Factorial" =
    evalCode factorial empty_env;
    [%expect {| 
        120. 
        43.
        0.      
    |}]

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

let p2: block = [
    Assign("v", Num(1.0));
    If(
        Op2(">", Var("v"), Num(10.0)), 
        [Assign("v", Op2("+", Var("v"), Num(1.0)))], 
        [For(
            Assign("i", Num(2.0)),
            Op2("<=", Var("i"), Num(10.0)),
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
    [%expect {| 3628800. |}] 

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

(* using the corrected version posted on the discussion board *)
let p3: block = 
    [
        FctDef("f", ["x"], [
            If(
                Op2("<", Var("x"), Num(2.0)),
                [Return(Num(1.0))],
                [Return(Op2("+",
                    Fct("f", [Op2("-", Var("x"), Num(1.0))]),
                    Fct("f", [Op2("-", Var("x"), Num(2.0))])
                ))])
        ]);
        Expr(Fct("f", [Num(3.0)]));
        Expr(Fct("f", [Num(5.0)]));
    ]


let%expect_test "p3" =
    evalCode p3 empty_env; 
    [%expect {| 
        3. 
        8.      
    |}]

(*block fails due to invalid function name *)
let f1:block = [
        FctDef("f", ["x"], [
            If(
                Op2("<", Var("x"), Num(2.0)),
                [Return(Num(1.0))],
                [Return(Op2("+",
                    Fct("fib", [Op2("-", Var("x"), Num(1.0))]),
                    Fct("f", [Op2("-", Var("x"), Num(2.0))])
                ))])
        ]);
        Expr(Fct("f", [Num(3.0)]));
        Expr(Fct("f", [Num(5.0)]));
    ]

let%expect_test "f1" =
    evalCode f1 empty_env; 
    [%expect {| 
        Error: Could not find function with name fib
    |}]

let f2:block = [
        FctDef("f", ["x"], [
            If(
                Op2("<", Var("x"), Num(2.0)),
                [Break],
                [Return(Op2("+",
                    Fct("f", [Op2("-", Var("x"), Num(1.0))]),
                    Fct("f", [Op2("-", Var("x"), Num(2.0))])
                ))])
        ]);
        Expr(Fct("f", [Num(3.0)]));
        Expr(Fct("f", [Num(5.0)]));
    ]

let%expect_test "f2" =
    evalCode f2 empty_env; 
    [%expect {| 
        Error: Continue/Break outside of for/while loop.
    |}]

let f3:block = [ Continue ]


let%expect_test "f3" =
    evalCode f2 empty_env; 
    [%expect {| 
        Error: Continue/Break outside of for/while loop.
    |}]

let f4:block = [
        FctDef("f", ["x"], [
            If(
                Op2("<", Var("x"), Num(2.0)),
                [Continue],
                [Return(Op2("+",
                    Fct("f", [Op2("-", Var("x"), Num(1.0)); Num(3.)]),
                    Fct("f", [Op2("-", Var("x"), Num(2.0))])
                ))])
        ]);
        Expr(Fct("f", [Num(3.0)]));
        Expr(Fct("f", [Num(5.0)]));
    ]

let%expect_test "f4" =
    evalCode f4 empty_env; 
    [%expect {| 
        Error: Incorrect number of arguments.
    |}]

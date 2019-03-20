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

type block = statement list 

type env = (Base.string, float, String.comparator_witness) Map.t

type envQueue = env list


let getVar (_v: string) (_q:envQueue): float = 0.0
    match _q with 
    | [] -> Error("No environments allocated")
    | globals :: [] ->
            (match Map.find

let putVar (_v: string) (_value: value) (_q:envQueue): (envQueue, result) result =
    match _q with
    | [] -> Error("No environments allocated")
    | globals -> Error
    | locals :: rest ->
*)
let rec evalOp1 (op: string) ( _e: expr) (_q:envQueue): (float, string) result =
    match evalExpr _e _q with
    | Error(msg) as err -> err
    | Ok(value) ->
            match op with
            | _ -> Error("Unrecognized operator '" ^ op ^ "'")

and evalOp2(op: string) (_e1: expr) (_e2: expr) (_q: envQueue): (float, string) result =
    match evalExpr _e1 _q with
    | Error(msg) as err -> err
    | Ok(val1) ->
        match evalExpr _e2 _q with
        | Error(msg) as err -> err
        | Ok(val2) -> 
            match op with
            | "+" -> Ok(val1 +. val2)
            | "-" -> Ok(val1 -. val2)
            | "*" -> Ok(val1 *. val2)
            | "/" -> Ok(val1 /. val2)
            | "^" -> Ok(val1 **. val2)
            | _ -> Error("Unrecognized operator '" ^ op ^ "'")

and evalExpr (_e: expr) (_q:envQueue): (float, string) result  = 
    match _e with 
    | Num(value) -> Ok(value)
    | Op1(op, exp) -> evalOp1 op exp _q
    | Op2(op, exp1, exp2) -> evalOp2 op exp1 exp2 _q
    (*| Var(id) -> getVar id _q*)
    | _ -> Error("Unrecognized expression.")
;;




let evalCode (_code: block) (_q:envQueue): unit = 
    (* crate new environment *)
    (* user fold_left  *)
    (* pop the local environment *)
    print_endline "Not implemented"

let evalStatement (s: statement) (q:envQueue): envQueue =
    match s with 
        | Assign(_v, _e) -> (* eval e and store in v *) q
        | If(e, codeT, codeF) -> q (* 
            let cond = evalExpr e q in
                if(cond>0.0) then
                    evalCode codeT q 
                else
                    evalCode codeF q
            ;q*)
        | _ -> q (*ignore *)


let p0: expr = Num(3.4)
(*
let%expect_test "p0" =
    evalExpr p0 [] |>
    printf "%F";
    [%expect {|3.4|}]
*)

(* Test expressions *)
let%expect_test "addition" = 
    (match evalExpr (Op2("+", (Op2("+", Num(1.), Num(1.))), Op2("-", Num(12.), Num(7.)))) [] with
    | Ok(value) -> printf "%F" value
    | Error(msg) -> print_endline msg);
    [%expect {| 7. |}]
    ;;

let%expect_test "multiplication" = 
    (match evalExpr (Op2("/", Op2("*", Num(80.), Num(3.)), Op2("*", Num(6.), Num(4.)))) [] with
    | Ok(value) -> printf "%F" value
    | Error(msg) -> print_endline msg);
    [%expect {| 10. |}]
    ;;

let%expect_test "exponentation" = 
    (match evalExpr (Op2("^", Num(2.), Op2("*", Num(0.5), Num(4.) ))) [] with
    | Ok(value) -> printf "%F" value
    | Error(msg) -> print_endline msg);
    [%expect {| 4. |}]
    ;;
(* 
    v = 10; 
    v // display v
 *)
 (*
let p1: block = [
        Assign("v", Num(1.0));
        Expr(Var("v")) 
]
*)
(*
let%expect_test "p1" =
    evalCode p1 []; 
    [%expect {| 1. |}]
*)
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
*)
(*
let%expect_test "p1" =
    evalCode p2 []; 
    [%expect {| 3628800. |}]
*)
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
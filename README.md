# OCaml_bc
Implementation of bc in OCaml

## Getting Started
Make sure that you have core, base, and dune installed. Then simply run:
```
dune runtest
```

# Status
Currently have only added loops, not functions. I will be submitting a version after the deadline so I can show I completed them.
I wrote ample testcases to prove that all current functionality works perfectly.

## Key changes to the provided code

### Changing env
I changed the type of `env`.
I picture the `env` as being a stack of scopes like so:
```
 _____
|_____| <---- locals in this scope
|_____| <--
|_____| <-- other locals
|_____| <-- 
|_____| <---- globals
```
This means that, accessing OCaml's linked-lists with match statements would be difficult, since `globals` and `locals` lie at opposite ends of the list.
Therefore, I replaced `env` with a product type containing a list of `scope`s [the stack of locals] and globals as its own separate `scope`.
```
type scope = (Base.string, float, String.comparator_witness) Map.t 
type env = scope * scope list
```
In true functional programming spirit, I used the Base.Map.t, which is completely immutable. Therefore, I also had to return an env from most functions.
However, this means that `envQueue` is obviated.


### Adding eval_status typefa
type statement = 
    | Assign of string*expr
    | Return of expr
    | Expr of expr
    | If of expr*statement list * statement list
    | While of expr*statement list
    | For of statement*expr*statement*statement list
    | FctDef of string * string list * statement list 

## Scoping Rules
This project follows the rules set out by bc:
only function arguments are added to the global scope. All other arguments are placed in the global scope. 
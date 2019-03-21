# OCaml_bc
Implementation of `bc` (basic calculator) in OCaml for *COP4020 Programming Language Concepts*.

## Getting Started
This package requires the core, base, and dune packages.
You can install those like so:
```
opam install [package name]
```
Make sure you are running a relatively recent version of OCaml (4.03^). 
If you need to switch to another version, you can use `opam switch install`. 

Once everything is installed, simply run:

Then simply run:

```
dune runtest
```

You will receive no output, meaning all the testcases pass.

## Key changes to the provided code
### SExpr

I did nothing with the SExpr type, and simply removed it.

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
(Env also contains `(Base.string, func, String.comparator_witness) Map.t`, which is used for storing function definitions.)
```OCaml
type scope = (Base.string, float, String.comparator_witness) Map.t 
type env = scope * scope list * (Base.string, func, String.comparator_witness) Map.t
```
In true functional programming spirit, I used the Base.Map.t, which is completely immutable. Therefore, I also had to return an env from most functions.
However, this means that `envQueue` is obviated and no longer used.

### Adding eval_status type to evalStatement
I added a special type, `eval_status` that is passed through the evalStatement function.
This sum type tracks any return/continue/break statements and also propagates error messages.
```OCaml
type eval_status =
    | Normal
    | Ret of float
    | Cont
    | Brk
    | Err of string
```

### Adding print control to evalStatement
I added a boolean parameter, `do_print` to `evalStatement`. 
This can be use to prevent unwanted printing.
For instance, the expressions in the pre/post statements of a For loop should not be printed (according to `bc`.)


## Statements supported
All of the following statements are supported:
```OCaml
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
```
A few of these were not in the original code.
`Nop` does nothing, and this is can be useful in the blocks of if/else/for/while loops.
`Continue` acts as the continue keyword (jumping to the next iteration of a loop).
`Break` acts as the break keyword (breaking out of a loop entirely).  

## Scoping Rules
This project follows the rules set out by `bc`:
only function arguments are added to the global scope. All other arguments are placed in the global scope. 

See the test `factorial` for confirmation of these rules.

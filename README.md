# OCaml_bc
Implementation of bc in OCaml


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


### Adding eval_status type
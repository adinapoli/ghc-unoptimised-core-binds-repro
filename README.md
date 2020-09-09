# Overview

This is a small Cabal project demonstrating that is not possible to extract the 'CoreBind' with
optimisations turned off in case things like `UNPACK` are used. Here is the testing scenario:

1. A "target" package that simply introduces a datatype with an unpacked field, and is
   compiled with `-O2`.

2. A 'ToyPlugin' GHC Plugin that modifies the GHC pipeline at the typechecking and core phases;
    a. During typechecking, we disable optimisations (we call `updOptLevel 0` on the `DynFlags`)
       and we call 'hscDesugar', hoping to get suitable binds;
    b. During core, we print the "standard" binds associated to the optimisation level for the
       user program (-O2).
    c. We print the binds of both phases.

One would expect that the generated Core to look something like that:

```
unFoo :: Foo -> Int
unFoo
  = \ (ds_d3hC :: Foo) -> case ds_d3hC of { Foo ds_d3hD -> ds_d3hD }
```

But we always get:

```
unFoo :: Foo -> Int
unFoo
  = \ (ds_d3pn :: Foo) -> case ds_d3pn of { Foo dt_d3pS -> I# dt_d3pS }
```

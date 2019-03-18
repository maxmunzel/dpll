# DPLL in Haskell

A simple SAT-Solver written in Haskell.

## Usage

```bash
$ cd dpll
$ cabal build
$ ./dist/build/dpll/dpll [filename]
```

## Example

The file format is pretty easy and roughly modeled after the standard notation for CNFs. 

(A v B v -C) && (-B v -A) would translate to:

```
c File: Example.cnf
c 
c space for  
c some comments
c 
1 2 -3
-2 -1
```

```bash
$ ./dist/build/dpll/dpll Example.cnf
Satisfiable:
[-3,-1]
```

## Performance

As this is just intended for me to get warm with haskell, there are currently no real optimizations in place so problems with over 100-200 clauses may take some of time.
If you need a high performance solution for big problems, consider `z3` or `miniSAT`. For small-ish problems, it performs fine though.

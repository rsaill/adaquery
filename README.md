# Adaquery

**Adaquery** is an experimental cross referencing tool for *Ada95*.

## Install

  * Make sure you have a recent version of [OCaml](http://ocaml.org) installed.
  * Make sure you have [Menhir](http://gallium.inria.fr/~fpottier/menhir/) installed (if not [Opam](http://opam.ocaml.org) is your friend).
```
  make
  sudo make install
```

## Usage

  1. Index the files of your project: `adaquery -p ada-rwth -index ada-rwth/**/*.ads`

  2. Query the database:

  * Search for a package or an object: `adaquery -p myproject -search 'inheritance'`
```
inheritance_mutant                                        
inheritance_child
inheritance_parent
inheritance_parents_parent
```

  * Locate a package or an object : `adaquery -p myproject -locate 'inheritance_mutant.Mutant'`
```
/home/username/repos/ada-rwth/chris/inheritance_mutant.ads
5
```

  * Print the content of a package: `adaquery -p myproject -print 'inheritance_mutant'`
```
Package
Mutant -> Type
test -> Subprog
```

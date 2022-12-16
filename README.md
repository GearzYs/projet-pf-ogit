# Ogit
## The Git Project codded in Ocaml

**Ogit is a university project based on git but coded in Ocaml, a general-purpose and multi-paradigm programming language**

[![Github](https://dabuttonfactory.com/button.png?t=Original%20Github&f=Ubuntu-Bold&ts=18&tc=fff&hp=200&vp=14&w=500&h=34&c=round&bgt=gradient&bgc=d900ff&ebgc=1763a9&bs=3&bc=000&shs=4&shc=666&sho=se)](https://github.com/etiloz/projet-pf-2022-2023-ogit/tree/main)
[![Word](https://dabuttonfactory.com/button.png?t=Google%20Word%20Subject&f=Ubuntu-Bold&ts=18&tc=fff&hp=200&vp=14&w=500&h=34&c=round&bgt=gradient&bgc=d900ff&ebgc=1763a9&bs=3&bc=000&shs=4&shc=666&sho=se)](https://docs.google.com/document/d/1OtQM95PCcBlJC8e2BRh-VQypuq-xDqIrDWZYESsvR9w)

## Installation

Compile it with :

```
> ocamlc objects.mli logs.mli commands.mli
> ocamlfind opt -linkpkg -package unix -o ogit ogit objects.ml logs.ml commands.ml ogit.ml
```

## Run

Use :
```
> ./ogit Ogit -c [command]
````

## Commands

- Ogit -c init
- Ogit -c commit "message"
- Ogit -c merge "hash"
- Ogit -c checkout "hash"
- Ogit-c log

## Libraries

Ogit is currently using these OCaml Libraries :

| Libraries | Documentation |
| ------ | ------ |
| List | [OCaml Library : List][lib1] |
| Array | [OCaml Library : Array][lib2] |
| String | [OCaml Library : String][lib3] |
| Sys | [OCaml Library : Sys][lib4] |
| Unix | [OCaml Library : Unix][lib5] |
| Arg | [OCaml Library : Arg][lib6] |
| Digest | [Ocaml Library : Digest][lib7] |
| Printf | [OCaml Library : Printf][lib8] |
| Filename | [OCaml Library : Filename][lib9] |

## Difficulties

What's a difficulty-free project ? Here are some difficulties we had during this project:

- First, we had a lot of problems with dune, It took us a while to understand dune.

- We found that utop can't show us the hash of an empty string (""). It makes it crash

- Before, we was compiling function by function and it took us so much time until we found the magic command ```dune utop```

- We had to seperate the hash function in two due to the typing of the hash function

- We prefered to use Sys.command to write to the files so we can use the -n option of echo to not have the final \n and so we can handle errors. If there's an error (by example if the user doesn't have enough perms), the code result will not be 0 (=success) and it'll fail with the message "erreur"

- For the store_work_directory function, we had to make a function to convert a Directory to a Type t called dir_t_to_object

- The log_graph has some problem with some specific tests

- clean_work_directory was a challenge to remove everything except hidden files with the best complexity and we did it with a shell command ```find . -not -path "*/.*" -delete```

## Developers

- Mr. BOUSSIK
- Mr. SAVASTA
- Mr. ESCOBAR

## Acknowledgements

Special thanks to our teachers :
- Mr. LOZES
- Ms. PELLEAU
- Mr. URSO

   [lib1]: <https://v2.ocaml.org/api/List.html>
   [lib2]: <https://v2.ocaml.org/api/Array.html>
   [lib3]: <https://v2.ocaml.org/api/String.html>
   [lib4]: <https://v2.ocaml.org/api/Sys.html>
   [lib5]: <https://v2.ocaml.org/api/Unix.html>
   [lib6]: <https://v2.ocaml.org/api/Arg.html>
   [lib7]: <https://v2.ocaml.org/api/Digest.html>
   [lib8]: <https://v2.ocaml.org/api/Printf.html>
   [lib9]: <https://v2.ocaml.org/api/Filename.html>

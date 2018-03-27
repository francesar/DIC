# 🍝  Do it Carefully (DIC) 🍝 
**Do it Carefully** is a programming languange that aims to provide developers with native support for matrix operations, while still following the object oriented mumbo jumbo that we all know and love. 

## Usage
In order to build the **DIC** compiler, run `make build`. Running `make build` uses `ocamlbuild` to generate a `toplevel.native` file, which serves as the driver for the **DIC**. Once built, you can pass a `.dic` file to be read by the compiler. 

## Running Hello World 👋🏽
In order to run hello world using **DIC**, make sure that LLVM is installed correctly (we tested ours using opam LLVM3.7). If everything is configured correctly, run `$ chmod u+x testhelloworld.sh` to make the script into an executable. Finally run `make test-helloworld` (which compiles the DIC compile and runs it on the test.dic file) and you should see **hello world**.

## Progress
Currently, all currently agreed upon syntax has been implemented. We do however have small bug in the variable declaration list inside of our functions. All `vdecl`'s (variable declarations) must happen before any `stmt` (statement) blocks. A bugfix is coming soon 🙊.

## Running the tests
Tests are defined in the `tests/` directory. In order to run the tests, make sure to have `ocamlbuild` installed on your machine, and run `make clean` (in the case that **DIC** has been built before) followed by `make test` in order to run the test suite. Currently there are 10 tests total, 5 positive tests that show showcase possible valid programs written in **DIC**, and 5 negative tests that show invalid programs that are expected to through syntax errors. The output's format prints the contents of the test file to STDOUT and either returns Ocaml's `Fatal error: exception Parsing.Parse_error` in the case that the test file is expected to throw an error or nothing if the test ran sucessfully. 

## Authors 
* **Yulissa Arroyo-Paredes** - ([yulissaa](https://github.com/yulissaa)) - ya2340@barnard.edu -  *Project Manager*
* **Abdurrauf Muhammad**  ([amuhammad135](https://github.com/amuhammad135)) - am4411@columbia.edu - *System Architect* 
* **Nick Liu**  ([liunick](https://github.com/liunick)) - nl2523@columbia.edu - *System Architect* 
* **Orko Sarkar**  ([blackfish](https://github.com/blackfish)) - os2301@columbia.edu - *Tester* 
* **Cesar Ibarra**  ([francesar](https://github.com/francesar)) - cfi2103@columbia.edu - *Language Guru* 

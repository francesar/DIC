# 🍝  Do it Carefully (DIC) 🍝 
**Do it Carefully** is a programming languange that aims to provide developers with native support for matrix operations, while still following the object oriented mumbo jumbo that we all know and love. 

## Usage
In order to build the **DIC** compiler, run `make`. Running `make` uses `ocamlbuild` to generate a `toplevel.native` file, which serves as the driver for the **DIC**. Once built, you can pass a `.dic` file to be read by the compiler. 

## Running the tests
Tests are defined in the `tests/` directory. Inside of `tests/` there is a directory for positive and negative tests. In order to run the tests, make sure to have `ocamlbuild` installed on your machine, and run `make clean` (in the case that **DIC** has been built before) followed by `make` in order to build the compiler. To run both positive and negative tests, run `make test`. When run, the file path and the test outcome will be outputed on the same line, either PASSED or FAILED. 

The positive tests the following 

    - test-binop-1: tests all binary operators

    - test-class-1: tests properly formatted class (not in microc)

    - test-control-1: tests if else statements

    - test-for-1: tests for loops

    - test-unop-1: tests ++, --, and - unops

    - test-vardecl-1: tests variable declaration and assignment inline (not in microc)

    - test-while-1: tests while loops

The negative tests the following
    - test-class-1: tests for incorrect class structure

    - test-for-1: tests for incorrect conditional in for loop

    - test-function-1: tests for incorrect functional call for function that expects no arguments
    
    - test-types-not-matching: tests for assignment of variable with incorrect declaration type.

## Authors 
* **Yulissa Arroyo-Paredes** - ([yulissaa](https://github.com/yulissaa)) - ya2340@barnard.edu -  *Project Manager*
* **Abdurrauf Muhammad**  ([amuhammad135](https://github.com/amuhammad135)) - am4411@columbia.edu - *System Architect* 
* **Nick Liu**  ([liunick](https://github.com/liunick)) - nl2523@columbia.edu - *System Architect* 
* **Orko Sarkar**  ([blackfish](https://github.com/blackfish)) - os2301@columbia.edu - *Tester* 
* **Cesar Ibarra**  ([francesar](https://github.com/francesar)) - cfi2103@columbia.edu - *Language Guru* 

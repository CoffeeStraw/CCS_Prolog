# CCS_Prolog
Interpreter for [CCS](https://en.wikipedia.org/wiki/Calculus_of_communicating_systems) language *(Calculus of Communicating Systems)* written in [SWI-Prolog](https://www.swi-prolog.org/).

Project for the **Principles for Software Composition course 2020/2021 @ UniPi**.

In addition to the parsing and execution of a CCS script, it is also possible to produce the derivation tree of a run, possibly saving it in a .tex file.

## Project organization
The source code has been split in 4 different files:
- `/src/ccs.pl`: main file of the project, contains **macros** to use the interpreter more easily;
- `/src/parser.pl`: contains dedicated functions to **parsing**. The allowed syntax is the one allowed by [CAAL](http://caal.cs.aau.dk/);
- `/src/interpreter.pl`: contains the implementations of the **operational semantics**;
- `/src/utils.pl`: contains some utilities to write a derivation tree in **LaTeX**;

## Run
First, be sure to run SWI-Prolog and load the main file of the project:
```console
swipl -s src/ccs.pl
```

To execute one step of your CCS code, you can use `execute_step`. Generally, you may want to pass your source code and the program starting point:
```prolog
execute_step('P = done.0;', var('P'), A, T, D).
```

Which will produce:
```prolog
A = in(done),
T = nil,
D = infer(const, red(var('P'), in(done), nil), [infer(pre, red(pre(in(done), nil), in(done), nil), []), infer('', in('P', pre(in(done), nil)), [])]) .
```

### Run from file
You can also put your code in a separated file and then use `execute_step_from_file` to run it. Suppose you have a file `test.ccs` containing some CCS code, you can run it using:
```prolog
execute_step_from_file('test.ccs', var('A'), A, T, D).
```

### Run more steps
If you want to run more then one single step, you can use `execute` or `execute_from_file`.

### Write derivation in LaTeX
To obtain a more readable derivation tree, you can write it in LaTeX using `write_derivation_to_tex`. As an example, consider the CCS program:
```console
A = (a.0 | 'a.0) \ {a};
```

The derivation tree terminating in `0 | 0` from `A` can be obtained in LaTeX using:
```prolog
execute_step("A = (a.0 | 'a.0) \\ {a};", var('A'), A, T, D), write_derivation_to_tex(D, 'out.tex').
```

## Tests
Tests are located in `/tests` directory. You can run one of them using the command:
```console
swipl -g run_tests -t halt PATH_TO_TEST
```
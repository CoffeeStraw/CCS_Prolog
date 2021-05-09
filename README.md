# CCS_Prolog
Interpreter for **CCS** language *(Calculus of Communicating Systems)* written in [SWI-Prolog](https://www.swi-prolog.org/).
Project for the **Principles for Software Composition course 2020/2021 @ UniPi**.

In addition to the parsing and execution of a CCS script, it is also possible to produce the derivation tree of a run, possibly saving it in a .tex file.

## Run
*WIP (only parsing done so far).*

### Tests
Tests are located in `/tests` directory. You can run them using the command:
```console
swipl -g run_tests -t halt ./tests/test_main.pl
```
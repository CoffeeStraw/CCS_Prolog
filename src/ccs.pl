/*===============================================================================================
CCS_Prolog -- Interpreter for CCS language (Calculus of Communicating Systems) written in Prolog.
Author: Antonio Strippoli (CoffeeStraw)
License: MIT
===============================================================================================*/

% Imports
:- [parser, interpreter, utils].

% Parse a program from file
parse_from_file(Path, ASTs) :- open(Path, read, Stream),
                               read_string(Stream, "", "", -1, String),
                               close(Stream),
                               parse(String, ASTs).

% Sequentially perform parsing and derive
execute_step(String, E, A, T, D)         :- parse(String, ASTs),         derive(E, A, T, [D], ASTs).
execute_step_from_file(Path, E, A, T, D) :- parse_from_file(Path, ASTs), derive(E, A, T, [D], ASTs).

% Sequentially execute 1 or more steps of a given program
execute(String, E, A_List, T_List, D_List)         :- parse(String, ASTs),         execute_more(E, A_List, T_List, D_List, ASTs).
execute_from_file(Path, E, A_List, T_List, D_List) :- parse_from_file(Path, ASTs), execute_more(E, A_List, T_List, D_List, ASTs).

execute_more(_, [], [], [], _).
execute_more(E, A_List, T_List, D_List, ASTs) :- derive(E, E_A, E_T, [E_D], ASTs),
                                                 execute_more(E_T, A_List_More, T_List_More, D_List_More, ASTs),
                                                 append([E_A], A_List_More, A_List),
                                                 append([E_T], T_List_More, T_List),
                                                 append([E_D], D_List_More, D_List).

% Write a derivation in a LaTeX file
write_derivation_to_tex(D, Path) :- derivation_to_tex(D, Latex), write_to_tex(Latex, Path).
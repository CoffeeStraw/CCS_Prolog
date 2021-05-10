/*===============================================================================================
CCS_Prolog -- Interpreter for CCS language (Calculus of Communicating Systems) written in Prolog.
Author: Antonio Strippoli (CoffeeStraw)
License: MIT
===============================================================================================*/

/*CONTROLLI DA FARE
- Un processo è valido solo quando ogni strada che può percorrere termina con 0 oppure con una variabile; (Si può assumere che quando 0 non c'è vada bene uguale)
- Una cosa tipo "X = a.P \\ {a} . 0;" dovrebbe essere permessa? (Per ora lo è)
*/

% Auxiliary predicate to check if two elements are the same.
same(X,X).

/*=================================================
SYNTAX:
- Nil process                   0
- Process definition            A = a.tau.P;
- Action prefixing - input      a.tau.P
- Action prefixing - output     'a.tau.P
- Choice operator               P + Q
- Parallel composition          P | Q
- Restriction                   P \ {a,b}
- Renaming of channels          P[b/a, d/c]
- Set definition                set L = {a,b,c}
- Comment                       * This is a comment
=================================================*/

/*==============
     PARSER
==============*/

% To skip during parsing: whitespaces and comments
skip --> [W], {char_type(W, space)}, !, skip.
skip --> ['*'], !, comment.
skip --> [].
comment --> [C], {char_type(C, graph); char_type(C, white)}, !, comment.
comment --> [C], {char_type(C, end_of_line)}, !, skip.

% Alphanumeric string, it is used to parse variables and actions
str_an(Res, C1_Type) --> [C], str_an(S), {char_type(C, C1_Type), atom_string(Res, [C|S])}.
str_an([C|S])        --> [C], {char_type(C, alnum)}, str_an(S), !.
str_an([])           --> [].

% String defining a set (e.g. {a,b}), it is used to parse sets of actions
str_set(L) --> ['{'], skip, str_an(H, lower), skip, str_set_more(T), {append([H], T, L)}.
str_set_more(L) --> [','], skip, str_an(H, lower), skip, str_set_more(T), {append([H], T, L)}.
str_set_more([]) --> ['}'].

% Parse a string returning str_an Abstract Syntax Tree
% NOTE: '\n' is appended to the string to allow comments as last line (since comments must end with '\n').
parsing(String, AST) :- atom_chars(String, Tmp), append(Tmp, ['\n'], Chars), phrase(cmds(AST), Chars).

cmds([E|Es]) --> skip, cmd(E), skip, !, cmds(Es).
cmds([])     --> [].

% COMMANDS
cmd(def_set(V, S)) --> ['s','e','t',' '], skip, str_an(V, upper), skip, ['='], skip, str_set(S), skip, [';'].
cmd(def_pro(V, E)) --> str_an(V, upper), skip, ['='], expr(E), [';'].

% EXPRESSIONS
expr(E) --> skip, expr_par(E), skip, !.

expr_par(par(E1, E2)) --> expr_or(E1), skip, ['|'], skip, expr_par(E2).
expr_par(E)           --> expr_or(E).

expr_or(or(E1, E2)) --> expr_pre(E1), skip, ['+'], skip, expr_or(E2).
expr_or(E)          --> expr_pre(E).

expr_pre(E)                         --> expr_pre_sequence(E).
expr_pre(E)                         --> expr_res(E).
expr_pre_sequence(pre(E1, E2))      --> ['('], skip, expr(E1), skip, [')'], skip, ['.'], skip, expr_pre_sequence(E2).
expr_pre_sequence(pre(E1, E2))      --> atom(E1), skip, ['.'], skip, expr_pre_sequence(E2).
expr_pre_sequence(pre(var(E1), E2)) --> atom_end(var(E1)), skip, ['.'], skip, expr_pre_sequence(E2).
expr_pre_sequence(E)                --> expr_res(E).

expr_res(res(E, S))      --> expr_brackets(E), skip, ['\\'], skip, str_set(S).
expr_res(res(E, var(V))) --> expr_brackets(E), skip, ['\\'], skip, atom_end(var(V)).
expr_res(E)              --> expr_brackets(E).

expr_brackets(E) --> ['('], skip, expr(E), skip, [')'].
expr_brackets(E) --> atom_end(E).

atom_end(nil)    --> ['0'].
atom_end(var(V)) --> str_an(V, upper).
atom(tau)    --> ['t', 'a', 'u'].
atom(in(A))  --> str_an(A, lower).
atom(out(A)) --> ['\''], str_an(A, lower), {not(same(A, tau))}.

/*==============
   INTERPRETER
==============*/
/*
run(Program, Result) :-
    parsing(Program, AST),
    eval_cmds(AST, Result).

eval_cmds([C1|C], R) :- eval_cmd(C1, R1), !, eval_cmds(Cs).
eval_cmds([], R) :- [].

eval_cmd(C) -->  {write(C)}.
*/
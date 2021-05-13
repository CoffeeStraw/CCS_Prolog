/*===============================================================================================
CCS_Prolog -- Interpreter for CCS language (Calculus of Communicating Systems) written in Prolog.
Author: Antonio Strippoli (CoffeeStraw)
License: MIT
===============================================================================================*/

% Parse a code written in CCS language, returning a list of Abstract Syntax Trees
% NOTE: '\n' is appended to the string to allow comments as last line (since comments must end with '\n').
parsing(String, Defs) :- atom_chars(String, Tmp), append(Tmp, ['\n'], Chars), phrase(cmds(Defs), Chars).

cmds([C|Cs]) --> skip, cmd(C), skip, !, cmds(Cs).
cmds([])     --> [].

% COMMANDS
cmd(def_set(V, S)) --> ['s','e','t',' '], skip, str_an(V, upper), skip, ['='], skip, str_set(S), skip, [';'].
cmd(def_pro(V, E)) --> str_an(V, upper), skip, ['='], expr(E), [';'].

% EXPRESSIONS
expr(E) --> skip, expr_par(E), skip, !.

% Parallel composition
expr_par(par(E1, E2)) --> expr_or(E1), skip, ['|'], skip, expr_par(E2).
expr_par(E)           --> expr_or(E).

% Nondeterministic choice
expr_or(or(E1, E2)) --> expr_pre(E1), skip, ['+'], skip, expr_or(E2).
expr_or(E)          --> expr_pre(E).

% Action prefix
expr_pre(pre(E1, E2)) --> atom(E1), skip, ['.'], skip, expr_pre(E2).
expr_pre(E)           --> expr_res(E).

% Restriction
expr_res(res(E, S))      --> expr_brackets(E), skip, ['\\'], skip, str_set(S).
expr_res(res(E, var(V))) --> expr_brackets(E), skip, ['\\'], skip, atom_end(var(V)).
expr_res(E)              --> expr_brackets(E).

% Parenthesis
expr_brackets(p(E)) --> ['('], expr(E), [')'].
expr_brackets(E)    --> atom_end(E).

% Atoms
atom_end(var(V)) --> atom(var(V)).
atom_end(nil)    --> ['0'].
atom(tau)    --> ['t', 'a', 'u'].
atom(var(V)) --> str_an(V, upper).
atom(in(A))  --> str_an(A, lower).
atom(out(A)) --> ['\''], str_an(A, lower), {A \== tau}.

% BASIC DCGs
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
str_set_more(L)  --> [','], skip, str_an(H, lower), skip, str_set_more(T), {append([H], T, L)}.
str_set_more([]) --> ['}'].
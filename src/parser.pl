/*===============================================================================================
CCS_Prolog -- Interpreter for CCS language (Calculus of Communicating Systems) written in Prolog.
Author: Antonio Strippoli (CoffeeStraw)
License: MIT
===============================================================================================*/

% Parse a code written in CCS language, returning a list of Abstract Syntax Trees
% NOTE: '\n' is appended to the string to allow comments as last line (since comments must end with '\n').
parse(String, Defs) :- atom_chars(String, Tmp), append(Tmp, ['\n'], Chars), phrase(cmds(Defs), Chars).

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
expr_pre(E)           --> expr_res_rel(E).

% Restriction
expr_res_rel(AST) --> expr_brackets(E), res_rel(E, AST).
expr_res_rel(E)   --> expr_brackets(E).

res_rel(E, AST)   --> skip, (res(E, AST1); rel(E, AST1)), !, res_rel(AST1, AST).
res_rel(AST, AST) --> [].

res(E, AST) --> ['\\'], skip, (str_set(S), {AST=res(E, S)}; atom_end(var(V)), {AST=res(E, var(V))}).
rel(E, AST) --> str_rel(Fi), {AST=rel(E, Fi)}.

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
% String defining a set (e.g. {a,b}), it is used to parse sets of actions
str_set(L) --> ['{'], skip, str_an(H, lower), skip, str_set_more(T), {append([H], T, L)}.
str_set_more(L)  --> [','], skip, str_an(H, lower), skip, str_set_more(T), {append([H], T, L)}.
str_set_more([]) --> ['}'].

% String defining a relabelling (e.g. [a/b, c/d]). 'tau' is not a suitable action name in a relabelling
str_rel(L) --> ['['], skip, str_an(H1, lower), skip, ['/'], skip, str_an(H2, lower), skip, str_rel_more(T), {H1 \== tau, H2 \== tau, append([pair(H1, H2)], T, L)}.
str_rel_more(L)  --> [','], skip, str_an(H1, lower), skip, ['/'], skip, str_an(H2, lower), skip, str_rel_more(T), {H1 \== tau, H2 \== tau, append([pair(H1, H2)], T, L)}.
str_rel_more([]) --> [']'].

% Alphanumeric string, it is used to parse variables and actions
str_an(Res, C1_Type) --> [C], str_an(S), {char_type(C, C1_Type), atom_string(Res, [C|S])}.
str_an([C|S])        --> [C], {char_type(C, alnum); C='_'}, str_an(S), !.
str_an([])           --> [].

% To skip during parsing: whitespaces and comments
skip --> [W], {char_type(W, space)}, !, skip.
skip --> ['*'], !, comment.
skip --> [].
comment --> [C], {char_type(C, graph); (char_type(C, space), \+ char_type(C, end_of_line))}, !, comment.
comment --> [C], {char_type(C, end_of_line)}, !, skip.
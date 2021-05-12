/*===============================================================================================
CCS_Prolog -- Interpreter for CCS language (Calculus of Communicating Systems) written in Prolog.
Author: Antonio Strippoli (CoffeeStraw)
License: MIT
===============================================================================================*/

/*==============
     PARSER
==============*/

% Parse a code written in CCS language, returning a list of Abstract Syntax Tree
% NOTE: '\n' is appended to the string to allow comments as last line (since comments must end with '\n').
parsing(String, ASTs) :- atom_chars(String, Tmp), append(Tmp, ['\n'], Chars), phrase(cmds(ASTs), Chars).

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

expr_pre(pre(E1, E2)) --> atom(E1), skip, ['.'], skip, expr_pre(E2).
expr_pre(E)           --> expr_res(E).

expr_res(res(E, S))      --> expr_brackets(E), skip, ['\\'], skip, str_set(S).
expr_res(res(E, var(V))) --> expr_brackets(E), skip, ['\\'], skip, atom_end(var(V)).
expr_res(E)              --> expr_brackets(E).

expr_brackets(p(E)) --> ['('], expr(E), [')'].
expr_brackets(E)    --> atom_end(E).

atom_end(var(V)) --> atom(var(V)).
atom_end(nil)    --> ['0'].
atom(tau)    --> ['t', 'a', 'u'].
atom(var(V)) --> str_an(V, upper).
atom(in(A))  --> str_an(A, lower).
atom(out(A)) --> ['\''], str_an(A, lower), {A \== tau}.

% UTILITIES
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

/*===============
   INTERPRETER
===============*/
/* TODO
- Check for recursion;
- Check for guarded process?;
*/

% Given a program String, derive the expression E in the epxression target T, consuming action A.
% The derivation tree D is computed as a proof of the derivation
parse_and_derive(String, E, A, T, D) :- parsing(String, ASTs), derive(E, A, T, [D], ASTs).

derive(par(E1, E2), A, T, D, ASTs) :-   derive(E1, A, E1_T, P, ASTs),
                                        T=par(E1_T, E2),
                                        D=[infer(parL, red(par(E1, E2), A, T), P)].
derive(par(E1, E2), A, T, D, ASTs) :-   derive(E2, A, E2_T, P, ASTs),
                                        T=par(E1, E2_T),
                                        D=[infer(parR, red(par(E1, E2), A, T), P)].
derive(par(E1, E2), tau, T, D, ASTs) :- (
                                             (derive(E1, in(A), E1_T, P1, ASTs), derive(E2, out(A), E2_T, P2, ASTs));
                                             (derive(E1, out(A), E1_T, P1, ASTs), derive(E2, in(A), E2_T, P2, ASTs))
                                        ),
                                        T=par(E1_T, E2_T),
                                        append(P1, P2, P),
                                        D=[infer(com, red(par(E1, E2), tau, T), P)].

derive(or(E1, E2), A, T, D, ASTs) :-    derive(E1, A, T, P, ASTs),
                                        D=[infer(sumL, red(or(E1, E2), A, T), P)].
derive(or(E1, E2), A, T, D, ASTs) :-    derive(E2, A, T, P, ASTs),
                                        D=[infer(sumR, red(or(E1, E2), A, T), P)].

derive(pre(A, T), A, T, D, _) :- D=[infer(pre, red(pre(A, T), A, T), [])].

derive(res(E, X), A, T, D, ASTs) :-     (X=var(V) -> get_def(V, S, ASTs); X=S),
                                        derive(E, A, T, P1, ASTs),
                                        (A=in(A_Name); A=out(A_Name); (A=tau, A_Name=tau)),
                                        \+ member(A_Name, S),
                                        P2=[infer('', notin(A, S), [])],
                                        append(P1, P2, P),
                                        D=[infer(res, red(res(E, S), A, res(p(T), S)), P)].

derive(p(E), A, T, D, ASTs) :- derive(E, A, T, D, ASTs).

derive(var(V), A, T, D, ASTs) :-   get_def(V, E, ASTs),
                                   derive(E, A, T, P, ASTs),
                                   D=[infer(rec, red(var(V), A, T), P)].

% Get definition for V inside a list of definitions
get_def(V, E, [AST|ASTs]) :- AST=def_pro(V, E); AST=def_set(V, E); get_def(V, E, ASTs).

/*===============
 LaTeX FORMATTER
===============*/

% Convert derivation in a more readable format compatible with LaTeX
derivation_to_tex([D1|D], Latex) :-     derivation_to_tex(D1, D1_Latex),
                                        derivation_to_tex(D, D_Latex),
                                        (length(D, 0) -> Sep='' ; Sep=' \\qquad '),
                                        atomic_list_concat(['{', D1_Latex, '}', Sep, D_Latex], Latex).
derivation_to_tex([], '').
derivation_to_tex(infer(Name, Conclusion, P), Latex) :-     derivation_to_tex(P, P_Latex),
                                                            expr_to_tex(Conclusion, Conclusion_Latex),
                                                            (Name='' -> (Label='', Infer_Deduce='\\deduce'); (atomic_list_concat(['[', Name, ']'], Label), Infer_Deduce='\\infer')),
                                                            atomic_list_concat([Infer_Deduce, Label, '\n{',
                                                                                Conclusion_Latex, '}\n{',
                                                                                P_Latex, '}\n'], Latex).

% Convert expression in a more readable format compatible with LaTeX
expr_to_tex(notin(A, S), Latex) :- expr_to_tex(A, A_Latex),
                                   atomic_list_concat(S, ', ', S_Latex),
                                   atomic_list_concat([A_Latex, ' \\notin ', '\\{', S_Latex, '\\}'], Latex).

expr_to_tex(red(E1, A, E2), Latex) :-   expr_to_tex(E1, E1_Latex),
                                        expr_to_tex(A, A_Latex),
                                        expr_to_tex(E2, E2_Latex),
                                        atomic_list_concat([E1_Latex, ' \\xrightarrow{', A_Latex, '} ', E2_Latex], Latex).

expr_to_tex(par(E1, E2), Latex) :- expr_to_tex(E1, E1_Latex),
                                   expr_to_tex(E2, E2_Latex),
                                   atomic_list_concat([E1_Latex, '\\;|\\;', E2_Latex], Latex).

expr_to_tex(or(E1, E2), Latex) :-  expr_to_tex(E1, E1_Latex),
                                   expr_to_tex(E2, E2_Latex),
                                   atomic_list_concat([E1_Latex, ' + ', E2_Latex], Latex).

expr_to_tex(pre(E1, E2), Latex) :- expr_to_tex(E1, E1_Latex),
                                   expr_to_tex(E2, E2_Latex),
                                   atomic_list_concat([E1_Latex, '.', E2_Latex], Latex).

expr_to_tex(res(E, S), Latex) :-   expr_to_tex(E, E_Latex),
                                   atomic_list_concat(S, ', ', S_Latex),
                                   atomic_list_concat([E_Latex, ' \\symbol{92} \\{', S_Latex, '\\}'], Latex).

expr_to_tex(p(E), Latex) :-   expr_to_tex(E, E_Latex),
                              atom_concat('(', E_Latex, Res1),
                              atom_concat(Res1, ')', Latex).

expr_to_tex(nil, '\\textbf{nil}').
expr_to_tex(tau, '\\tau').
expr_to_tex(var(V), V).
expr_to_tex(in(A), A).
expr_to_tex(out(A), Latex) :- atom_concat('\\overline{', A, Res1),
                              atom_concat(Res1, '}', Latex).

% Write Latex content to file
write_to_tex(Content, Path) :-     open(Path, write, Out),
                                   write(Out, '\\documentclass[20pt]{extarticle}\n\\usepackage{proof}\n\\usepackage{amsmath}\n\n\\begin{document}\n\\[\n'),
                                   write(Out, Content),
                                   write(Out, '\\]\n\\end{document}'),
                                   close(Out).
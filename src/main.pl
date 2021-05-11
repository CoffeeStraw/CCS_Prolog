/*===============================================================================================
CCS_Prolog -- Interpreter for CCS language (Calculus of Communicating Systems) written in Prolog.
Author: Antonio Strippoli (CoffeeStraw)
License: MIT
===============================================================================================*/

% Auxiliary predicate to check if two elements are the same.
same(X,X).

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
cmd(def_pro(V, E)) --> str_an(V, upper), skip, ['='], expr(E, false), [';'].

% EXPRESSIONS
% NOTE: 'InPre' is a boolean used to check whether an expression
% E1 that we are parsing is inside the form (E1).E2
% In those cases, processes in E1 can also end with atom besides atom_end
expr(E, InPre) --> skip, expr_par(E, InPre), skip, !.

expr_par(par(E1, E2), InPre) --> expr_or(E1, InPre), skip, ['|'], skip, expr_par(E2, InPre).
expr_par(E, InPre)           --> expr_or(E, InPre).

expr_or(or(E1, E2), InPre) --> expr_pre(E1, InPre), skip, ['+'], skip, expr_or(E2, InPre).
expr_or(E, InPre)          --> expr_pre(E, InPre).

expr_pre(E, InPre)                       --> expr_pre_sequence(E, InPre).
expr_pre(E, InPre)                       --> expr_res(E, InPre).
expr_pre_sequence(pre(p(E1), E2), InPre) --> ['('], skip, expr(E1, true), skip, [')'], skip, ['.'], skip, expr_pre_sequence(E2, InPre).
expr_pre_sequence(pre(E1, E2), InPre)    --> atom(E1), skip, ['.'], skip, expr_pre_sequence(E2, InPre).
expr_pre_sequence(E, InPre)              --> expr_res(E, InPre).

expr_res(res(E, S), InPre)      --> expr_brackets(E, InPre), skip, ['\\'], skip, str_set(S).
expr_res(res(E, var(V)), InPre) --> expr_brackets(E, InPre), skip, ['\\'], skip, atom_end(var(V)).
expr_res(E, InPre)              --> expr_brackets(E, InPre).

expr_brackets(p(E), InPre) --> ['('], skip, expr(E, InPre), skip, [')'].
expr_brackets(E, _)        --> atom_end(E).
expr_brackets(E, true)     --> atom(E).

atom_end(var(V)) --> atom(var(V)).
atom_end(nil)    --> ['0'].
atom(var(V))     --> str_an(V, upper).
atom(tau)        --> ['t', 'a', 'u'].
atom(in(A))      --> str_an(A, lower).
atom(out(A))     --> ['\''], str_an(A, lower), {not(same(A, tau))}.

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

/*==============
   INTERPRETER
==============*/
/* TODO
- Check for recursion;
- Check for guarded process?;
*/

% LEGEND: E=Expression, V=Variable, A=Action, T=Target, D=Derivation, P=Premises, Defs=List of AST containing definitions

% Compute a derivation tree D starting from a process P, terminating (possibly partially) in T using action A.
parse_and_derive(String, red(var(P),A,T), D) :- parsing(String, ASTs), derive(red(var(P),A,T), D, ASTs).
derive(red(var(P),A,T), D, Defs) :- derive_step(var(P), A, T, [D], Defs).

% DERIVATION FOR SINGLE STEPS
derive_step(or(E1, E2), A, T, D, Defs)  :- derive_step(E1, A, T, P, Defs),
                                           D=[infer(suml, red(or(E1, E2), A, T), P)].
derive_step(or(E1, E2), A, T, D, Defs)  :- derive_step(E2, A, T, P, Defs),
                                           D=[infer(sumr, red(or(E1, E2), A, T), P)].

% PRE: Perform step(s) in E
derive_step(pre(E1, E2), A, T, D, Defs)  :- derive_step(E1, A, T, P, Defs),
                                            D=[infer(pre, red(pre(E1, E2), A, pre(T, E2)), P)].
% PRE: Reduce E to atom and then perform step(s) in T2
derive_step(pre(E, T2), A, T, D, Defs)  :- derive_step(E, _, T1, P1, Defs),
                                           is_atom(T1),
                                           derive_step(pre(T1, T2), A, T, P2, Defs),
                                           append(P1, P2, P),
                                           D=[infer(pre, red(pre(E, T2), A, T), P)].
% PRE: Perform one step consuming the action A
derive_step(pre(A, T), A, T, D, _)      :- is_atom(A), D=[infer(pre, red(pre(A, T), A, T), [])].
% PRE: Consume action A1 and then perform step(s) in T1
derive_step(pre(A1, T1), A, T, D, Defs) :- is_atom(A1), derive_step(T1, A, T, P, Defs),
                                           D=[infer(pre, red(pre(A1, T1), A, T), [P])].

% PARENTHESIS
derive_step(p(E), A, T, D, Defs) :- derive_step(E, A, T, D, Defs).

% VAR: Perform one single step by performing a replacement
derive_step(var(V), A, T, D, Defs) :- get_def(V, E, Defs),
                                      derive_step(E, A, T, P, Defs),
                                      D=[infer(rec, red(var(V), A, T), P)].

% UTILITIES

% Predicates to check for atoms
is_atom(nil).
is_atom(tau).
is_atom(in(_)).
is_atom(out(_)).

% Get definition for V inside a list of definitions
get_def(V, E, [Def1|Defs]) :- Def1=def_pro(V, E); get_def(V, E, Defs).

% Convert Derivation in a more readable format compatible with LaTEX
derivation_to_tex(infer(Name, red(E1, A, E2), P), Latex) :- expr_to_latex(E1, E1_Latex),
                                                            expr_to_latex(A, A_Latex),
                                                            expr_to_latex(E2, E2_Latex),
                                                            derivation_to_tex(P, P_Latex),
                                                            atom_concat('\\infer[', Name, Res1),
                                                            atom_concat(Res1, ']\n{', Res2),
                                                            atom_concat(Res2, E1_Latex, Res3),
                                                            atom_concat(Res3, ' \\xrightarrow{', Res4),
                                                            atom_concat(Res4, A_Latex, Res5),
                                                            atom_concat(Res5, '} ', Res6),
                                                            atom_concat(Res6, E2_Latex, Res7),
                                                            atom_concat(Res7, '}\n', Res8),
                                                            atom_concat(Res8, P_Latex, Res9),
                                                            atom_concat(Res9, '\n', Latex).
derivation_to_tex([D1|D], Latex) :- derivation_to_tex(D1, D1_Latex),
                                    atom_concat('{', D1_Latex, D1_Latex1),
                                    atom_concat(D1_Latex1, '}', D1_Latex2),
                                    derivation_to_tex(D, D_Latex),
                                    atom_concat(D1_Latex2, D_Latex, Latex).
derivation_to_tex([], '{}').

% Convert expressions in a more readable format compatible with LaTEX
expr_to_latex(par(E1, E2), Latex) :- expr_to_latex(E1, E1_Latex),
                                     expr_to_latex(E2, E2_Latex),
                                     atom_concat(E1_Latex, ' | ', Res1),
                                     atom_concat(Res1, E2_Latex, Latex).

expr_to_latex(or(E1, E2), Latex) :- expr_to_latex(E1, E1_Latex),
                                    expr_to_latex(E2, E2_Latex),
                                    atom_concat(E1_Latex, ' + ', Res1),
                                    atom_concat(Res1, E2_Latex, Latex).

expr_to_latex(pre(E1, E2), Latex) :- expr_to_latex(E1, E1_Latex),
                                     expr_to_latex(E2, E2_Latex),
                                     atom_concat(E1_Latex, '.', Res1),
                                     atom_concat(Res1, E2_Latex, Latex).

expr_to_latex(p(E), Latex) :- expr_to_latex(E, E_Latex),
                              atom_concat('(', E_Latex, Res1),
                              atom_concat(Res1, ')', Latex).

expr_to_latex(nil, '0').
expr_to_latex(tau, tau).
expr_to_latex(var(V), V).
expr_to_latex(in(A), A).
expr_to_latex(out(A), Latex) :- atom_concat('\\overline{', A, Res1),
                                atom_concat(Res1, '}', Latex).

% Write Latex content to file
write_to_tex(Content, Path) :- open(Path, write, Out),
                               write(Out, '\\documentclass[20pt]{extarticle}\n\\usepackage{proof}\n\\usepackage{amsmath}\n\n\\begin{document}\n\\[\n'),
                               write(Out, Content),
                               write(Out, '\\]\n\\end{document}'),
                               close(Out).
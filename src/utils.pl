/*===============================================================================================
CCS_Prolog -- Interpreter for CCS language (Calculus of Communicating Systems) written in Prolog.
Author: Antonio Strippoli (CoffeeStraw)
License: MIT
===============================================================================================*/

% Write derivations in LaTeX using package `proof`
derivation_to_tex([D1|D], Latex) :- derivation_to_tex(D1, D1_Latex),
                                    derivation_to_tex(D, D_Latex),
                                    (length(D, 0) -> Sep='' ; Sep=' \\qquad '),
                                    atomic_list_concat(['{', D1_Latex, '}', Sep, D_Latex], Latex).
derivation_to_tex([], '').
derivation_to_tex(infer(Name, Conclusion, P), Latex) :- derivation_to_tex(P, P_Latex),
                                                        expr_to_tex(Conclusion, Conclusion_Latex),
                                                        (
                                                            Name='' -> Label='', Infer_Deduce='\\deduce';
                                                            atomic_list_concat(['[', Name, ']'], Label), Infer_Deduce='\\infer'
                                                        ),
                                                        atomic_list_concat([Infer_Deduce, Label, '\n{',
                                                                            Conclusion_Latex, '}\n{',
                                                                            P_Latex, '}\n'], Latex).

% Write expressions in LaTeX using package `proof`
expr_to_tex(in_delta(V, E), Latex) :- expr_to_tex(E, E_Latex),
                                      atomic_list_concat([V, ' \\triangleq ', E_Latex, ' \\in ', '\\Delta'], Latex).

expr_to_tex(notin_set(A, S), Latex) :- atomic_list_concat(S, ', ', S_Latex),
                                       atomic_list_concat([A, ' \\notin ', '\\{', S_Latex, '\\}'], Latex).

expr_to_tex(red(E1, A, E2), Latex) :- expr_to_tex(E1, E1_Latex),
                                      expr_to_tex(A, A_Latex),
                                      expr_to_tex(E2, E2_Latex),
                                      atomic_list_concat([E1_Latex, ' \\xrightarrow{', A_Latex, '} ', E2_Latex], Latex).

expr_to_tex(par(E1, E2), Latex) :- expr_to_tex(E1, E1_Latex),
                                   expr_to_tex(E2, E2_Latex),
                                   atomic_list_concat([E1_Latex, '\\;|\\;', E2_Latex], Latex).

expr_to_tex(or(E1, E2), Latex) :- expr_to_tex(E1, E1_Latex),
                                  expr_to_tex(E2, E2_Latex),
                                  atomic_list_concat([E1_Latex, ' + ', E2_Latex], Latex).

expr_to_tex(pre(E1, E2), Latex) :- expr_to_tex(E1, E1_Latex),
                                   expr_to_tex(E2, E2_Latex),
                                   atomic_list_concat([E1_Latex, '.', E2_Latex], Latex).

expr_to_tex(res(E, S), Latex) :- expr_to_tex(E, E_Latex),
                                 atomic_list_concat(S, ', ', S_Latex),
                                 atomic_list_concat([E_Latex, ' \\; \\symbol{92} \\{', S_Latex, '\\}'], Latex).

expr_to_tex(rel(E, S), Latex) :- expr_to_tex(E, E_Latex),
                                 maplist(expr_to_tex, S, S_mapped),
                                 atomic_list_concat(S_mapped, ', ', S_Latex),
                                 atomic_list_concat([E_Latex, '\\;[', S_Latex, ']'], Latex).

expr_to_tex(p(E), Latex) :- expr_to_tex(E, E_Latex),
                            atomic_list_concat(['(', E_Latex, ')'], Latex).

expr_to_tex(nil, '\\textbf{nil}').
expr_to_tex(tau, '\\tau').
expr_to_tex(var(V), V).
expr_to_tex(in(A), A).
expr_to_tex(out(A), Latex) :- atomic_list_concat(['\\overline{', A, '}'], Latex).
expr_to_tex(pair(A1, A2), Latex) :- atomic_list_concat([A1, '/', A2], Latex).

% Write LaTeX content to file
write_to_tex(Content, Path) :- open(Path, write, Out),
                               write(Out, '\\documentclass[10pt]{article}\n\\usepackage{proof}\n\\usepackage{amsmath}\n\\usepackage{amssymb}\n\n\\begin{document}\n\\[\n'),
                               write(Out, Content),
                               write(Out, '\\]\n\\end{document}'),
                               close(Out).
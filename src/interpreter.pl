/*===============================================================================================
CCS_Prolog -- Interpreter for CCS language (Calculus of Communicating Systems) written in Prolog.
Author: Antonio Strippoli (CoffeeStraw)
License: MIT
===============================================================================================*/

/* TODO
- Check for recursion;
- Check for guarded process?;
*/

% Derive an expression `E` (with variables defined in `Defs`) in the expression target `T`, consuming action `A`.
% The derivation tree `D` is computed as a proof of the derivation.

% Parallel composition
derive(par(E1, E2), A, T, D, Defs)   :- derive(E1, A, E1_T, P, Defs),
                                        T=par(E1_T, E2),
                                        D=[infer(parL, red(par(E1, E2), A, T), P)].
derive(par(E1, E2), A, T, D, Defs)   :- derive(E2, A, E2_T, P, Defs),
                                        T=par(E1, E2_T),
                                        D=[infer(parR, red(par(E1, E2), A, T), P)].
derive(par(E1, E2), tau, T, D, Defs) :- (
                                            derive(E1, in(A), E1_T, P1, Defs), derive(E2, out(A), E2_T, P2, Defs);
                                            derive(E1, out(A), E1_T, P1, Defs), derive(E2, in(A), E2_T, P2, Defs)
                                        ),
                                        T=par(E1_T, E2_T),
                                        append(P1, P2, P),
                                        D=[infer(com, red(par(E1, E2), tau, T), P)].

% Nondeterministic choice
derive(or(E1, E2), A, T, D, Defs) :- derive(E1, A, T, P, Defs),
                                     D=[infer(sumL, red(or(E1, E2), A, T), P)].
derive(or(E1, E2), A, T, D, Defs) :- derive(E2, A, T, P, Defs),
                                     D=[infer(sumR, red(or(E1, E2), A, T), P)].

% Action prefix
derive(pre(A, T), A, T, D, _) :- D=[infer(pre, red(pre(A, T), A, T), [])].

% Restriction
derive(res(E, X), A, T, D, Defs) :- (X=var(V) -> get_def(V, S, Defs); X=S),
                                    derive(E, A, E_T, P1, Defs),
                                    (A=in(A_Name); A=out(A_Name); A=tau, A_Name=A),
                                    \+ member(A_Name, S),
                                    P2=[infer('', notin_set(A_Name, S), [])],
                                    append(P1, P2, P),
                                    T=res(E_T, S),
                                    D=[infer(res, red(res(E, S), A, T), P)].

% Relabelling
derive(rel(E, L), A, T, D, Defs) :- derive(E, E_A, E_T, P, Defs),
                                    (
                                        E_A=in(A_Name),  (member(pair(Rel_Name, A_Name), L) -> A=in(Rel_Name);  A=E_A);
                                        E_A=out(A_Name), (member(pair(Rel_Name, A_Name), L) -> A=out(Rel_Name); A=E_A);
                                        E_A=tau, A=E_A
                                    ),
                                    T=rel(E_T, L),
                                    D=[infer(rel, red(rel(E, L), A, T), P)].

% Parenthesis
derive(p(E), A, p(T), D, Defs) :- derive(E, A, T, D, Defs).

% Recursion via process constant
derive(var(V), A, T, D, Defs) :- get_def(V, E, Defs),
                                 derive(E, A, T, P1, Defs),
                                 P2=[infer('', in_delta(V, E), [])],
                                 append(P1, P2, P),
                                 D=[infer(const, red(var(V), A, T), P)].

% Get definition for V inside a list of definitions
get_def(V, E, [AST|Defs]) :- AST=def_pro(V, E); AST=def_set(V, E); get_def(V, E, Defs).
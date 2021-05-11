:- consult("../src/main.pl").

:- begin_tests(var).
var_parsing(S) :- atom_chars(S, C), phrase(atom_end(var(_)), C).
test(var1)            :- var_parsing("A").
test(var2)            :- var_parsing("AA").
test(var3)            :- var_parsing("Aa").
test(var_fail1, fail) :- var_parsing("x").
test(var_fail2, fail) :- var_parsing("1X").
test(var_fail3, fail) :- var_parsing("xX").
:- end_tests(var).

:- begin_tests(in).
in_parsing(S) :- atom_chars(S, C), phrase(atom(in(_)), C).
test(in1)            :- in_parsing("a").
test(in2)            :- in_parsing("aa").
test(in3)            :- in_parsing("aA").
test(in_fail1, fail) :- in_parsing("'a").
test(in_fail2, fail) :- in_parsing("A").
test(in_fail3, fail) :- in_parsing("0").
:- end_tests(in).

:- begin_tests(out).
out_parsing(S) :- atom_chars(S, C), phrase(atom(out(_)), C).
test(out1)            :- out_parsing("'a").
test(out2)            :- out_parsing("'aa").
test(out3)            :- out_parsing("'aA").
test(out_fail1, fail) :- out_parsing("a").
test(out_fail2, fail) :- out_parsing("A").
test(out_fail3, fail) :- out_parsing("tau").
:- end_tests(out).

:- begin_tests(expr).
expr_parsing(S) :- atom_chars(S, C), phrase(expr(_, false), C).
test(expr1)            :- expr_parsing("a.0").
test(expr2)            :- expr_parsing("0").
test(expr3)            :- expr_parsing("A").
test(expr4)            :- expr_parsing("a.0+b.0").
test(expr5)            :- expr_parsing("a.A |        b.0").
test(expr6)            :- expr_parsing("a  *Comment\n  .AAAAAAAAA").
test(expr7)            :- expr_parsing("(a+b).0").
test(expr7)            :- expr_parsing("(a.0|b.0) + c.0").
test(expr_fail1, fail) :- expr_parsing("00").              % Not an atom
test(expr_fail2, fail) :- expr_parsing("a + *Nothing").    % Missing something after +
test(expr_fail3, fail) :- expr_parsing("a + b c").         % 'b c' is not an expression
test(expr_fail4, fail) :- expr_parsing("a + b").           % a and b does not end with 0 or a variable
test(expr_fail5, fail) :- expr_parsing("(a | b.X) + c.0"). % a does not end with 0 or a variable
:- end_tests(expr).

:- begin_tests(cmds).
cmds_parsing(S) :- parsing(S, _).
test(cmds1)            :- cmds_parsing("A = a.A;").
test(cmds2)            :- cmds_parsing("Welcome    =\n\n at \n\n\n\n. *Useless comment\n   home.0;    *EndOfString").
test(cmds3)            :- cmds_parsing("A = (a.0 + b.0 | c.A + d.A) + (e.A|f.0)     ;  ").
test(cmds_fail1, fail) :- cmds_parsing("A = a").    % missing ;
test(cmds_fail2, fail) :- cmds_parsing("A = a;").   % must end with 0 or a variable
test(cmds_fail2, fail) :- cmds_parsing("a = a.0;"). % leftside not a variable
test(cmds_fail3, fail) :- cmds_parsing("a + b;").   % not a command (no leftside)
:- end_tests(cmds).

/*

parse_and_derive("X = (a.b).c.0;", red(var('X'), A, T), D), derivation_to_tex(D, Latex), write_to_tex(Latex, 'test.tex').

:- begin_tests(derivations).
derive_pre(AST) :- derive_step(AST, A, S, D, _).
test(der1) :- derive_pre(pre(in(a), pre(in(b), nil)), ).
:- end_tests(derivations).
*/
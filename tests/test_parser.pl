:- consult("../src/ccs.pl").

:- begin_tests(var).
var_parse(S) :- atom_chars(S, C), phrase(atom_end(var(_)), C).
test(var1)            :- var_parse("A").
test(var2)            :- var_parse("AA").
test(var3)            :- var_parse("Aa").
test(var4)            :- var_parse("A_1").
test(var_fail1, fail) :- var_parse("x").
test(var_fail2, fail) :- var_parse("1X").
test(var_fail3, fail) :- var_parse("xX").
:- end_tests(var).

:- begin_tests(in).
in_parse(S) :- atom_chars(S, C), phrase(atom(in(_)), C).
test(in1)            :- in_parse("a").
test(in2)            :- in_parse("aa").
test(in3)            :- in_parse("aA").
test(in4)            :- in_parse("a_1").
test(in_fail1, fail) :- in_parse("'a").
test(in_fail2, fail) :- in_parse("A").
test(in_fail3, fail) :- in_parse("0").
:- end_tests(in).

:- begin_tests(out).
out_parse(S) :- atom_chars(S, C), phrase(atom(out(_)), C).
test(out1)            :- out_parse("'a").
test(out2)            :- out_parse("'aa").
test(out3)            :- out_parse("'aA").
test(out_fail1, fail) :- out_parse("a").
test(out_fail2, fail) :- out_parse("A").
test(out_fail3, fail) :- out_parse("tau").
:- end_tests(out).

:- begin_tests(expr).
expr_parse(S) :- atom_chars(S, C), phrase(expr(_), C).
test(expr1)            :- expr_parse("a.0").
test(expr2)            :- expr_parse("0").
test(expr3)            :- expr_parse("A").
test(expr4)            :- expr_parse("a.0+b.0").
test(expr5)            :- expr_parse("a.A |        b.0").
test(expr6)            :- expr_parse("a  *Comment\n  .AAAAAAAAA").
test(expr7)            :- expr_parse("(a.0|b.0) + c.0").
test(expr8)            :- expr_parse("(a.b.0)   [   a/c,  b/d    ]").
test(expr8)            :- expr_parse("(a.b.0) [a/c] \\ {a}").
test(expr_fail1, fail) :- expr_parse("00").              % Not an atom
test(expr_fail2, fail) :- expr_parse("a + *Nothing").    % Missing something after +
test(expr_fail3, fail) :- expr_parse("a + b c").         % 'b c' is not an expression
test(expr_fail4, fail) :- expr_parse("a + b").           % a and b does not end with 0 or a variable
test(expr_fail5, fail) :- expr_parse("(a | b.X) + c.0"). % a does not end with 0 or a variable
:- end_tests(expr).

:- begin_tests(cmds).
cmds_parse(S) :- parse(S, _).
test(cmds1)            :- cmds_parse("A = a.A;").
test(cmds2)            :- cmds_parse("Welcome    =\n\n at \n\n\n\n. *Useless comment\n   home.0;    *EndOfString").
test(cmds3)            :- cmds_parse("A = (a.0 + b.0 | c.A + d.A) + (e.A|f.0)     ;  ").
test(cmds4)            :- cmds_parse("set S = {a,b,c};").
test(cmds_fail1, fail) :- cmds_parse("A = a").             % missing ;
test(cmds_fail2, fail) :- cmds_parse("A = a;").            % must end with 0 or a variable
test(cmds_fail2, fail) :- cmds_parse("a = a.0;").          % leftside not a variable
test(cmds_fail3, fail) :- cmds_parse("a + b;").            % not a command (no leftside)
test(cmds_fail4, fail) :- cmds_parse("S = {a,b,c};").      % missing 'set'
test(cmds_fail5, fail) :- cmds_parse("set S = {a,'b,c};"). % 'b is not the name of a channel
:- end_tests(cmds).
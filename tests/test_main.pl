:- consult("../src/main.pl").

:- begin_tests(var).
var_parsing(S) :- atom_chars(S, C), phrase(atom(var(_)), C).
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
expr_parsing(S) :- atom_chars(S, C), phrase(expr(_), C).
test(expr1)            :- expr_parsing("a").
test(expr2)            :- expr_parsing("0").
test(expr3)            :- expr_parsing("tau").
test(expr4)            :- expr_parsing("a+b").
test(expr5)            :- expr_parsing("a |        b").
test(expr6)            :- expr_parsing("a    .0").
test(expr7)            :- expr_parsing("a  *Comment\n  .0").
test(expr8)            :- expr_parsing("a.0 + b.0").
test(expr_fail1, fail) :- expr_parsing("00").
test(expr_fail2, fail) :- expr_parsing("a + *Nothing").
test(expr_fail3, fail) :- expr_parsing("a + b c").
:- end_tests(expr).
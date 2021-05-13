/*===============================================================================================
CCS_Prolog -- Interpreter for CCS language (Calculus of Communicating Systems) written in Prolog.
Author: Antonio Strippoli (CoffeeStraw)
License: MIT
===============================================================================================*/

% Imports
:- [parser, interpreter, utils].

% Given a CCS program `String`, derive the expression `E` (with variables defined in `Defs`) in the expression target `T`, consuming action `A`.
% The derivation tree `D` is computed as a proof of the derivation
parse_and_derive(String, E, A, T, D) :- parsing(String, ASTs), derive(E, A, T, [D], ASTs).
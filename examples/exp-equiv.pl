% exp-equiv.pl

:- ['exp.pl'].

:- >>> ' prove that mult(2,3) ~ add(3,3)'.

% show that
% (forall s)(exists V)
%         [s:: mult(2,3)==>V ^ s::add(3,3)==>V]

:- show
      state(s):: mult(const(2),const(3)) ==> V, 
      state(s):: add(const(3),const(3)) ==> V.
:- qed.


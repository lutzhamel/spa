% imp-equiv.pl

:- ['imp.pl'].

:- >>> ' prove that mult(2,3) ~ add(3,3)'.
%
% show that
% (forall s)(exists V0,V1)
%         [s:: mult(2,3)==>V0 ^ s::add(3,3)==>V1 ^ V0=V1]

:- show
      state(s):: mult(const(2),const(3)) ==> V0, 
      state(s):: add(const(3),const(3)) ==> V1, 
      V0 = V1.
:- qed.


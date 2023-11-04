% imp-proof1.pl

:- ['imp.pl'].

% Proof score:
%
% Show that
% (forall x)(forall s)(forall vx)(exists V)
%         [(add(x,mult(3,5)),s)-->>V ^ V=vx+15]

% state our assumption
:- assume state(s):: lookup(var(x)) >--> vx.

% run the proof
:- show
      state(s):: add(var(x),mult(const(3),const(5))) ==> V,
      V = vx+15.

:- qed.
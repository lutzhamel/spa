% imp-comm.pl

:- ['imp.pl'].

:- >>> 'prove that add(a0,a1) ~ add(a1,a0)'.

% show that
% (forall s,a0,a1)(exists V0,V1)
%         [s::add(a0,a1)==>V0 ^ s::add(a1,a0)==>V1 ^ V0=V1]

:- assume syntax(a0). % necessary to make SPA happy
:- assume syntax(a1).
:- assume state(s):: a0 ==> va0.
:- assume state(s):: a1 ==> va1.
:- assume comm(A + B, B + A).    % integer addition commutativity

:- show 
      state(s):: add(a0,a1) ==> V0, 
      state(s):: add(a1,a0) ==> V1,
      comm(V0,V1).  % V0 and V1 are equivalent modulo commutativity
:- qed.

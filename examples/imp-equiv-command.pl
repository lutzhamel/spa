% imp-equiv-command.pl

:- ['imp.pl'].

:- >>> 'prove that'.
:- >>> '   assign(x,1) <> assign(y,2) ~ assign(y,2) <> assign(x,1)'.

% We need to show that
% (forall s,z)(exist S0,S1,V0,V1)
%     [s:: assign(x,1) seq assign(y,2) ==> S0 ^
%      s:: assign(y,2) seq assign(x,1) ==> S1 ^
%      S0:: lookup(z) >--> V0 ^ 
%      S1:: lookup(z) >--> V1 ^ 
%      V0=V1]

% define this property for all following proofs
:- define state(s):: lookup(var(z)) >--> vz.

:- >>> 'we show equivalence by case analysis on z' .
:- >>> 'case z = x'.

:- show
      state(s):: assign(var(x),const(1)) <> assign(var(y),const(2)) ==> S0,
      state(s):: assign(var(y),const(2)) <> assign(var(x),const(1)) ==> S1,
      S0:: lookup(var(x)) >--> V0,
      S1:: lookup(var(x)) >--> V1,
      V0=V1.

:- >>> 'case z = y'.
:- show
      state(s):: assign(var(x),const(1)) <> assign(var(y),const(2)) ==> S0,
      state(s):: assign(var(y),const(2)) <> assign(var(x),const(1)) ==> S1,
      S0:: lookup(var(y)) >--> V0,
      S1:: lookup(var(y)) >--> V1,
      V0=V1.

:- >>> 'case z =/= x and z =/= y'.
:- show 
      state(s):: assign(var(x),const(1)) <> assign(var(y),const(2)) ==> S0,
      state(s):: assign(var(y),const(2)) <> assign(var(x),const(1)) ==> S1,
      S0:: lookup(var(z)) >--> V0,
      S1:: lookup(var(z)) >--> V1,
      V0=V1.
:- qed.
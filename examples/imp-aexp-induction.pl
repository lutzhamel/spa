% imp-aexp-induction.pl

:- ['imp.pl'].

:- >>> 'show that all arithmetic operations terminate'.
:- >>> '  (forall a)(forall s)(exists K)[s::a==>K]'.
:- >>> 'we prove this by structural induction on arithmetic expressions'.

:- >>> 'Here are the grammar rules for arithmetic expressions:'.
:- listing(a::=_).

:- >>> 'base case: integer values'.
% assumption: n is an integer
:- assume int(n).
:- show 
      state(s):: const(n) ==> n.

:- >>> 'base case: variables'.
% assumption: a variable lookup will always produce a value
:- assume state(s):: lookup(var(x)) >--> vx.
:- show 
      state(s):: var(x) ==> vx.

:- >>> 'inductive step: add(a0,a1)'.
% inductive hypotheses
:- assume syntax(a0).  
:- assume syntax(a1).
:- assume state(s):: a0 ==> va0.
:- assume state(s):: a1 ==> va1.
% induction step
:- show 
      state(s):: add(a0,a1) ==> va0+va1.

:- >>> 'the remaining operators can be proved similarly'.

:- qed.


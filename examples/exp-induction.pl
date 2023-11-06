% exp-induction.pl

:- ['exp.pl'].

:- >>> 'show that all expressions terminate'.
:- >>> '  (forall exp)(forall s)(exists K)[s::exp==>K]'.
:- >>> 'we prove this by structural induction on expressions'.

:- >>> 'Here are the grammar rules for expressions:'.
:- listing(exp::=_).

:- >>> 'base case: integer values'.
% assumption: n is an integer
:- assume int(n).
:- show 
      state(s):: const(n) ==> n.

:- >>> 'base case: variables'.
% assumption: a variable lookup will always produce a value in any state _S
:- assume _S:: lookup(var(x)) >--> vx.  
:- show 
      state(s):: var(x) ==> vx.

:- >>> 'inductive step: add(e0,e1)'.
% inductive hypotheses
:- assume syntax(e0).       % e0 is considered a syntactic structure
:- assume syntax(e1).       % e1 is considered a syntactic structure
:- assume _S:: e0 ==> ve0.  % for any state _S the expression e0 terminates
:- assume _S:: e1 ==> ve1.  % for any state _S the expression e1 terminates
% induction step
:- show 
      state(s):: add(e0,e1) ==> ve0+ve1.

:- >>> 'the remaining arithmetic operators can be proved similarly'.

:- >>> 'inductive step: let(var(x),e0,e1)'.
% inductive hypotheses
:- assume syntax(e0).        % e0 is considered a syntactic structure
:- assume syntax(e1).        % e1 is considered a syntactic structure
:- assume _S:: e0 ==> ve0.   % for any state _S the expression e0 terminates
:- assume (S:: e1 ==> ve1 :- % for any state _S the expression e1 terminates if x is defined
      S:: lookup(var(x)) >--> _V).
% induction step
:- show 
      state(s):: let(var(x),e0,e1) ==> ve1.

:- qed.


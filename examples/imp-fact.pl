% imp-fact.pl
:- ['imp.pl'].

% prove that the value of y is equal to 6 when 
% following program p,
%
%        assign(x,3) <>
%        assign(y,1) <> 
%        whiledo(le(2,x),
%                assign(y,mult(y,x)) <>
%                assign(x,sub(x,1))
%        )
%
% is run in the context of any state.
%
% We need to prove
%
% (forall s)(exists SF)[s::p==>SF ^ SF::lookup(y)>-->6]

% define our program predicate
:- define 
        program(assign(var(x),const(3)) <>
                assign(var(y),const(1)) <> 
                whiledo(le(const(2),var(x)),
                        assign(var(y),mult(var(y),var(x))) <>
                        assign(var(x),sub(var(x),const(1))))).


:- show 
        program(P), 
        state(s):: P ==> SF, 
        SF:: lookup(var(y)) >--> 6.

:- qed.
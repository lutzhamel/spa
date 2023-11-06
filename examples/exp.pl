% exp.pl
% Version 1.0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% abstract syntax of a simple expression language

exp ::= const(n)       % any integer
    | var(x)           % any variable
    | add(exp,exp)
    | sub(exp,exp)
    | mult(exp,exp)
    | let(var(x),exp,exp)
    .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% load our state representation
:- consult("state.pl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% semantics of expressions

_:: const(C) ==> C :-              
    int(C),!.

State:: var(X) ==> Val :-               
    name(X),
    State:: lookup(var(X)) >--> Val,!.

State:: add(A,B)  ==> Val :-       
    State:: A ==> ValA,
    State:: B ==> ValB,
    Val xis ValA + ValB,!.

State:: sub(A,B) ==> Val :-       
    State:: A ==> ValA,
    State:: B ==> ValB,
    Val xis ValA - ValB,!.

State:: mult(A,B) ==> Val :-     
    State:: A ==> ValA,
    State:: B ==> ValB,
    Val xis ValA * ValB,!.

State:: let(var(X),A,B) ==> Val :-
    State:: A ==> ValA,
    State:: put(var(X),ValA) >--> SNew,
    SNew:: B ==> Val,!.

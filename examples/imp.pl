% imp.pl
% Version 4.0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% abstract syntax of the IMP language

% for convenience sake make command composition infix and left associative
:- op(750,yfx,(<>)).

a ::= const(n)       % any integer
    | var(x)         % any variable
    | add(a,a)
    | sub(a,a)
    | mult(a,a)
    .

b ::= true
    | false
    | eq(a,a)
    | le(a,a)
    | not(b)
    | and(b,b)
    | or(b,b)
    .

c ::= skip
    | assign(x,a)
    | c <> c
    | if(b,c,c)
    | whiledo(b,c)
    .


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% load our state representation
:- consult("state.pl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% semantics of arithmetic expressions

_:: const(C) ==> C :-              % constants
    int(C),!.

State:: var(X) ==> Val :-               % variables
    name(X),
    State:: lookup(var(X)) >--> Val,!.

State:: add(A,B)  ==> Val :-       % addition
    State:: A ==> ValA,
    State:: B ==> ValB,
    Val xis ValA + ValB,!.

State:: sub(A,B) ==> Val :-       % subtraction
    State:: A ==> ValA,
    State:: B ==> ValB,
    Val xis ValA - ValB,!.

State:: mult(A,B) ==> Val :-     % multiplication
    State:: A ==> ValA,
    State:: B ==> ValB,
    Val xis ValA * ValB,!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% semantics of boolean expressions

_:: true ==> true :- !.               % boolean constants
_:: false ==> false :- !.

State:: eq(A,B) ==> Val :-            % equality
    State:: A ==> ValA,         
    State:: B ==> ValB,         
    Val xis ValA == ValB,!. 
    
State:: le(A,B) ==> Val :-            % le
    State:: A ==> ValA,
    State:: B ==> ValB,
    Val xis ValA =< ValB,!.
    
State:: not(A) ==> Val :-             % not
    State:: A ==> ValA,
    Val xis not ValA,!.             

State:: and(A,B) ==> Val :-           % and
    State:: A ==> ValA,
    State:: B ==> ValB,
    Val xis ValA and ValB,!.

State:: or(A,B) ==> Val :-            % or
    State:: A ==> ValA,
    State:: B ==> ValB,
    Val xis ValA or ValB,!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% semantics of commands

State:: skip ==> State :- !.          % skip

State:: assign(var(X),A) ==> OState :-     % assignment
    State:: A ==> ValA,
    State:: put(var(X),ValA) >--> OState,!.

State:: C0 <> C1 ==> OState :-      % composition
    State:: C0 ==> S0,
    S0:: C1 ==> OState,!.

State:: if(B,C0,_) ==> OState :-     % if (true)
    State:: B ==> true,
    State:: C0 ==> OState,!.

State:: if(B,_,C1) ==> OState :-     % if (false)
    State:: B ==> false,
    State:: C1 ==> OState,!.

State:: whiledo(B,_) ==> State :-    % while (false)
    State:: B ==> false,!.

State:: whiledo(B,C) ==> OState :-    % while (true)
    State:: B ==> true,
    State:: C ==> SC,
    SC:: whiledo(B,C) ==> OState,!. 




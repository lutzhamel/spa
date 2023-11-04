% fun.pl
% a small functional language
% version 1.0

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% abstract syntax
exp ::= const(n)
      | id(x)
      | add(exp,exp)
      | sub(exp,exp)
      | mul(exp,exp)
      | lambda(id(x),exp)
      | if(bool,exp,exp)
      | apply(exp,exp)
      | let(stmt,exp)
      .

bool ::= true
      | false
      | eq(exp,exp)
      | le(exp,exp)
      | not(bool)
      | and(bool,bool)
      | or(bool,bool)
      .

stmt ::= val(id(x),exp)
      | fun(id(f),id(x),exp)
      .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% semantics

:-['env.pl'].

% Expressions

_:: const(C) ==> C :-
   int(C),!.

Env:: id(X) ==> Val :-
   name(X),
   Env:: lookup(id(X)) >--> Val,!.

Env:: add(A,B)  ==> Val :-       
    Env:: A ==> ValA,
    Env:: B ==> ValB,
    Val xis ValA + ValB,!.

Env:: sub(A,B) ==> Val :-      
    Env:: A ==> ValA,
    Env:: B ==> ValB,
    Val xis ValA - ValB,!.

Env:: mult(A,B) ==> Val :-     
    Env:: A ==> ValA,
    Env:: B ==> ValB,
    Val xis ValA * ValB,!.

Env:: lambda(id(X),Exp) ==> [[id(X),Exp,Env]] :- !.

Env:: if(B,E0,_E1) ==> Val :-     
    Env:: B ==> true,
    Env:: E0 ==> Val,!.

Env:: if(B,_E0,E1) ==> Val :-     
    Env:: B ==> false,
    Env:: E1 ==> Val,!.

Env:: apply(E1,E2) ==> Val :-
   Env:: E1 ==> [[id(X),E,Env1]],
   Env:: E2 ==> V2,
   Env1:: put(id(X),V2) >--> Env2,
   Env2:: E ==> Val,!.

Env:: let(Stmt,E) ==> Val :-
   Env:: Stmt ==> Env2,
   Env2:: E ==> Val,!.

% Booleans

_:: true ==> true :- !.               
_:: false ==> false :- !.

Env:: eq(A,B) ==> Val :-           
    Env:: A ==> ValA,         
    Env:: B ==> ValB,         
    Val xis ValA == ValB,!. 
    
Env:: le(A,B) ==> Val :-            
    Env:: A ==> ValA,
    Env:: B ==> ValB,
    Val xis ValA =< ValB,!.
    
Env:: not(A) ==> Val :-             
    Env:: A ==> ValA,
    Val xis not ValA,!.             

Env:: and(A,B) ==> Val :-           
    Env:: A ==> ValA,
    Env:: B ==> ValB,
    Val xis ValA and ValB,!.

Env:: or(A,B) ==> Val :-            
    Env:: A ==> ValA,
    Env:: B ==> ValB,
    Val xis ValA or ValB,!.

% Statements

Env:: val(id(X),Exp) ==> Env2 :-
   Env:: Exp ==> V,
   Env:: put(id(X),V) >--> Env2,!.

Env:: fun(id(F),id(X),Exp) ==> Env2 :-
   Env:: put(id(F),[[id(X),Exp,Env]]) >--> Env2,!.

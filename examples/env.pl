% env.pl
% version 1.0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% our states are represented as list-like, right-associative structures
%  env((x,10)#(y,25)#s0)
%
% NOTE: the 'env' functor is necessary otherwise the prolog parser
% will not parse the infix # constructor correctly.

:- op(1200,xfy,#). % internal environment representation right-associative list-like

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'lookup(+Variable)' looks up
% the variable in the state and returns its bound value.
:- dynamic lookup/1.                % modifiable predicate

env(e0):: lookup(id(_)) >--> 0. 

env((X,Val)):: lookup(id(X)) >--> Val.

env((X,Val)#_):: lookup(id(X)) >--> Val.

env(_#Rest):: lookup(id(X)) >--> Val :- 
    env(Rest):: lookup(id(X)) >--> Val.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the predicate 'put(+Variable,+Value)' adds
% a binding term to the state.
:- dynamic put/2.                   % modifiable predicate

env(E):: put(id(X),Val) >--> env((X,Val)#E).


% fun-double.pl

:- ['fun.pl'].

:- >>> 'Show that our inc function works'.

% define our program
:- define 
      program(let(fun(id(inc),id(x),add(id(x),const(1))),
               apply(id(inc),const(1)))).

% show that the program produces the value 2
:- show
      program(P),
      env(e):: P ==> 2.

:- qed.

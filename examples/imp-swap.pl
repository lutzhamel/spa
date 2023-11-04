% imp-swap.pl

:-['imp.pl'].

:- >>> 'Show that the values for x and y are swapped'.
:- >>> 'after the program runs'.

:- >>> 'define our program'.
:- define program(assign(var(t),var(x)) <> 
                    assign(var(x),var(y)) <> 
                    assign(var(y),var(t))).                                              

:- >>> 'define our initial values'.                                                                                               
:- assume state(s):: lookup(var(x)) >--> vx.
:- assume state(s):: lookup(var(y)) >--> vy.                                                                        

:- >>> 'show that the values in x and y are swapped after program runs'.                                                                                          
:- show
     program(P),
     state(s):: P ==> Q,
     state(s):: lookup(var(x)) >--> VX,                                                                               
     state(s):: lookup(var(y)) >--> VY,
     Q:: lookup(var(x)) >--> VY,                                                                               
     Q:: lookup(var(y)) >--> VX.

:- qed.
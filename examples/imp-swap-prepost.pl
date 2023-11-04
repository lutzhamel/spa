% imp-swap-prepost.pl

:-['imp.pl'].

:- >>> 'show that the program satisfies the program specification:'.
:- >>> ' pre(s) = s::lookup(x)>-->vx ^ s::lookup(y)>-->vy'.
:- >>> ' post(q) = q::lookup(x)>-->vy ^ q::lookup(y)>-->vx'.

:- >>> 'define our program'.
:- define program(assign(var(t),var(x)) <> 
                    assign(var(x),var(y)) <> 
                    assign(var(y),var(t))).                                              
                                                                                                
:- >>> 'assert precondition'.                                                                      
:- assume state(s):: lookup(var(x)) >--> vx.
:- assume state(s):: lookup(var(y)) >--> vy.                                                                        
                                                                                                   
:- >>> 'show that postcondition holds'.                                                            
:- show
     program(P),
     state(s):: P ==> Q,
     Q:: lookup(var(x)) >--> vy,                                                                               
     Q:: lookup(var(y)) >--> vx.

:- qed.
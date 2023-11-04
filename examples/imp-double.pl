% imp-double.pl
:-['imp.pl'].

:- set_remove_assumptions(true).

:- >>> 'show that the value stored in z after the program runs'.
:- >>> 'is twice the original value stored in z'.

:- define program(assign(var(z),add(var(z),var(z)))).

:- assume state(s):: lookup(var(z)) >--> vz.                                                                        
:- assume 2*I xis I+I. % property of integers                                               
                                                                                                   
:- show
      program(P),
      state(s):: lookup(var(z)) >--> V1, % lookup original value                                                                           
      state(s):: P ==> Q,                % run the program
      Q:: lookup(var(z)) >--> V2,        % lookup new value
      V2 = 2 * V1.                       % show that new value is twice 
                                         % the original value
:- qed.


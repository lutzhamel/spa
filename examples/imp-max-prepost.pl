% imp-max-prepost.pl
:-['imp.pl'].

:- >>> 'show that the program satisfies the program specification:'.
:- >>> ' pre(s) = s::lookup(m)>-->vm ^ s::lookup(n)>-->vm'.
:- >>> ' post(q) = q::lookup(z)>-->VZ ^ V xis max(vm,vn) ^ VZ=V'.

:- define
      program(if(le(var(n),var(m)),
               assign(var(z),var(m)),
               assign(var(z),var(n)))).

:- >>> 'assume precondition'.
% Note: we use define to make the assumptions "sticky" and
% therefore available in all parts of the proof
:- define state(s):: lookup(var(m)) >--> vm.
:- define state(s):: lookup(var(n)) >--> vn.                                                                        
                                                                                                   
:- >>> 'show that postcondition holds; case analysis on values vm and vn'.                                                       
:- >>> 'case max(vm,vn)=vm'.                                                                       
:- assume vm xis max(vm,vn).
% this implies that
:- assume true xis vn =< vm.

:- show 
      program(P),  
      state(s):: P ==> Q, 
      Q:: lookup(var(z)) >--> VZ, 
      V xis max(vm,vn), 
      VZ = V.

:- >>> 'case max(vm,vn)=vn'.
:- assume vn xis max(vm,vn).
% this implies that
:- assume false xis vn =< vm.

:- show
      program(P), 
      state(s):: P ==> Q, 
      Q:: lookup(var(z)) >--> VZ, 
      V xis max(vm,vn), 
      VZ = V.
:- qed.
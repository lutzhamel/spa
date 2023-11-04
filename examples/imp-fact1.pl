% imp-fact1.pl
:-['imp.pl'].

:- >>> 'show partial correctness of program'
:- >>> 'define the parts of our program'.
:- define 
    init(assign(var(i),const(1)) <> assign(var(z),const(1))).
:- define 
    guard(not(eq(var(i),var(n)))).
:- define 
    body(assign(var(i),add(var(i),const(1))) <> 
         assign(var(z),mult(var(z),var(i)))).

:- >>> 'define our fact operation'.
:- dynamic fact/2.
:- define fact(1,1).
:- define (fact(X,Y) :- 
        T1 is X-1,
        fact(T1,T2), 
        Y is X*T2).

:- >>> 'first proof obligation'.
:- >>> 'assume precondition'.
:- assume state(s)::lookup(n) >--> vn.
:- >>> 'prove the invariant'.
:- show 
    init(I),
    state(s):: I  ==> Q,
    Q:: lookup(var(z)) >--> VZ,
    Q:: lookup(var(i)) >--> VI, 
    fact(VI,VZ).

:- >>> 'second  proof obligation'.
:- >>> 'assume invariant on s'.
:- assume state(s):: lookup(var(z)) >--> vz.
:- assume state(s):: lookup(var(i)) >--> vi.
:- assume fact(vi,vz).
% the above implies
:- assume fact(vi+1,vz*(vi+1)).
:- >>> 'assume guard on s'.
:- assume s:: not(eq(var(i),var(n))) ==> true.
:- >>> 'prove the invariant on Q'.
:- show
    body(Bd),
    state(s):: Bd ==> Q,
    Q:: lookup(var(z)) >--> VZ,
    Q:: lookup(var(i)) >--> VI, 
    fact(VI,VZ).

:- >>> 'third  proof obligation'.
:- >>> 'assume the invariant on s'.
:- assume state(s):: lookup(var(z)) >--> vz.
:- assume state(s):: lookup(var(i)) >--> vi.
:- assume fact(vi,vz).
:- >>> 'assume NOT guard on s'.
:- assume state(s):: not(eq(i,n)) ==> not(true).
% implies
:- assume state(s):: eq(var(i),var(n)) ==> true.
% implies
:- assume fact(vn,vz).
:- >>> 'prove postcondition on s'.
:- show
    state(s):: lookup(var(z)) >--> VZ,
    fact(vn,VZ).
:- qed.


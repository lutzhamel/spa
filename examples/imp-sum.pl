% imp-sum.pl

:-['imp.pl'].

:- >>> 'define the parts of our program'.
:- define init(assign(var(i),const(0)) <> assign(var(p),const(0))).
:- define guard(not(eq(var(i),var(n)))).
:- define body(assign(var(i),add(var(i),const(1))) <> 
                assign(var(p),add(var(p),var(i)))).

:- >>> 'define our sum operation'.
:- dynamic sum/2.
:- define sum(0,0).
:- define (sum(X,Y) :- 
            T1 is X-1,
            sum(T1,T2), 
            Y is X+T2).

:- >>> 'first proof obligation'.
:- >>> 'assume precondition'.
:- assume state(s):: lookup(var(n)) >--> vn.
:- >>> 'proof the invariant'.
:- show 
        init(I),
        state(s):: I ==> Q,
        Q:: lookup(var(p)) >--> VP,
        Q:: lookup(var(i)) >--> VI, 
        sum(VI,VP).

:- >>> 'second  proof obligation'.
:- >>> 'assume invariant on s'.
:- assume state(s):: lookup(var(p)) >-->  vp.
:- assume state(s):: lookup(var(i)) >--> vi.
:- assume sum(vi,vp).
% implies
:- assume sum(vi+1,vp+(vi+1)).
:- >>> 'assume guard on s'.
:- assume state(s):: not(eq(var(i),var(n))) ==> true.
:- >>> 'proof the invariant on Q'.
:- show 
        body(B),
        state(s):: B ==> Q,
        Q:: lookup(var(p)) >--> VP,
        Q:: lookup(var(i)) >--> VI, 
        sum(VI,VP).

:- >>> 'third  proof obligation'.
:- >>> 'assume the invariant on s'.
:- assume state(s):: lookup(var(p)) >-->  vp.
:- assume state(s):: lookup(var(i)) >--> vi.
:- assume sum(vi,vp).
:- >>> 'assume NOT guard on s'.
:- assume state(s):: not(eq(var(i),var(n))) ==> not(true).
% implies
:- assume state(s):: eq(var(i),var(n)) ==> true.
% implies
:- assume sum(vn,vp).
:- >>> 'prove postcondition on s'.
:- show 
        state(s):: lookup(var(p)) >--> VP,
        sum(vn,VP).

:- qed.

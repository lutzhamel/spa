# SPA - Semantics Proof Assistant

SPA is a domain specific proof assistant based on Prolog.  It was
designed to support proofs in Natural Semantics [1].  SPA provides a layer on top
of Prolog enabling readable proofs in first-order logic.  In addition to providing functionality
for proofs in Natural Semantics all the functionality of Prolog is still available to the
user.  In particular, Prolog's debugging and tracing facilities are available within the SPA framework.  Our approach shares many of the same conviction as the approach given in this paper [2].

SPA provides two key additions to the Prolog reasoning framework in order to support
proofs in Natural Semantics:

1. SPA provides the predicate `xis` which works just like the built-in predicate `is` but has
been eXtended to deal with indeterminate values such as `n+1`.  The built-in predicate would
simply fail here but the extended predicate will return this value as a term.  This ability 
to deal with indeterminates is crucial when constructing proofs.

2. SPA uses universal generalization to provide univerally quantified variables in queries
and proof scores.

SPA was designed to work with the SWI Prolog system (swi-prolog.org).  An early description of the
system appears in [3].

[1] Kahn, G. (1987, February). Natural semantics. In Annual symposium on theoretical aspects of computer science (pp. 22-39). Berlin, Heidelberg: Springer Berlin Heidelberg.

[2] Christiansen, H. (2004, September). Prolog as description and implementation language in computer science teaching. In Proceedings of The First International Workshop on Teaching Logic Programming-TeachLP (pp. 43-54).

[3] Hamel, L. (2016). Formal Methods: A First Introduction using Prolog to specify Programming Language Semantics. In Proceedings of the International Conference on Foundations of Computer Science (pp. 70-76).

## A Specification of a Simple Expression Language

Here is a specification of a simple expression language that 
illustrates SPA.  

```
% exp.pl
% Version 1.0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% abstract syntax of a simple expression language

exp ::= const(n)       % any integer
    | var(x)           % any variable
    | add(exp,exp)
    | sub(exp,exp)
    | mult(exp,exp)
    | let(var(x),exp,exp)
    .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% load our state representation
:- consult("state.pl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% semantics of expressions

_:: const(C) ==> C :-              
    int(C),!.

State:: var(X) ==> Val :-               
    State:: lookup(var(X)) >--> Val,!.

State:: add(A,B)  ==> Val :-       
    State:: A ==> ValA,
    State:: B ==> ValB,
    Val xis ValA + ValB,!.

State:: sub(A,B) ==> Val :-       
    State:: A ==> ValA,
    State:: B ==> ValB,
    Val xis ValA - ValB,!.

State:: mult(A,B) ==> Val :-     
    State:: A ==> ValA,
    State:: B ==> ValB,
    Val xis ValA * ValB,!.

State:: let(var(X),A,B) ==> Val :-
    State:: A ==> ValA,
    State:: put(var(X),ValA) >--> SNew,
    SNew:: B ==> Val,!.
```
There are two parts to any specification.  The first part is
the syntax specification using a context free grammar in BNF 
notation. 

The second part consists of the specification of the semantic
rules for the language.  Here each rule is structured as 
follows,
```
<state_info>:: <syntax> ==> <semantic_value> :-
      <condition_1>,
      ...
      <condition_n>,!.
```
Each rule states that the syntactic structure `<syntax>` evaluates to the semantic value `<semantic_value>` in the context of state `<state_info>` if the conditions 1 through n are
satisfied.  Notice that each rule terminates with a cut.  This is necessary since we view
these rules as implementing an abstract interpreter and as that we do not allow backtracking 
to run the interpreter in reverse.  For example the rule,
```
State:: add(A,B)  ==> Val :-       
    State:: A ==> ValA,
    State:: B ==> ValB,
    Val xis ValA + ValB,!.
```
states that the syntactic structure `add(A,B)` evaluates to the semantic value `Val is ValA + ValB` in the 
context of `State` if the subexpressions `A` and `B` evaluate to `ValA` and `ValB` in the
context of `State`, respectively.

The rule specifying the behavior of variable references in expressions,
```
State:: var(X) ==> Val :-               
    State:: lookup(var(X)) >--> Val,!.
```
states that the interpretation is simply a lookup of the variable in the current state.
The predicate `<state_info>:: <structure> >--> <value>` is the evaluation of any structure in 
the context of a state.  Note that here we are **not** computing a semantic value of the structure, we are simply 
computing a value for the structure in the context of a state.
In our case the bahavior of this predicate is defined in 
the specification for the state `state.pl`.

We can now use the expression language specification in proofs.  Here
is a SPA proof score to shows that `mult(2,3)` is equivalent to
`add(3,3)` assuming that our expression language specification is 
in file `exp.pl`,
```
% exp-equiv.pl

% load our specification
:- ['exp.pl'].

% echo a brief message about the proof to the output
:- >>> 'prove that mult(2,3) ~ add(3,3)'.

% Quick outline of the proof:
% show that
% (forall s)(exists V)
%         [s:: mult(2,3)==>V ^ s::add(3,3)==>V]

% execute the proof
:- show
      state(s):: mult(const(2),const(3)) ==> V, 
      state(s):: add(const(3),const(3)) ==> V.
% all done
:- qed.
```
Assuming that we have a terminal open at the top folder of
our git repository then executing the proof score can be done as follows,
```
$ cd examples 
$ swipl -q -f ../spa/spa.pl exp-equiv.pl 
SPA - Semantics Proof Assistant Version 4.0
(C) University of Rhode Island
type 'help(spa)' for additional help
type 'halt' to exit
>>>  prove that mult(2,3) ~ add(3,3)
Show: 
     state(s)::mult(const(2), const(3))==>A,
     state(s)::add(const(3), const(3))==>A.

     ***Success***

     ***removing assumptions***

     ***qed***
$ 
```
Here is a proof that demonstrates that all expressions in our language terminate.
This is done via structural induction over the expression syntax,
```
% exp-induction.pl

:- ['exp.pl'].

:- >>> 'show that all expressions terminate'.
:- >>> '  (forall exp)(forall s)(exists K)[s::exp==>K]'.
:- >>> 'we prove this by structural induction on expressions'.

:- >>> 'Here are the grammar rules for expressions:'.
:- listing(exp::=_).

:- >>> 'base case: integer values'.
% assumption: n is an integer
:- assume int(n).
:- show 
      state(s):: const(n) ==> n.

:- >>> 'base case: variables'.
% assumption: a variable lookup will always produce a value in any state _S
:- assume _S:: lookup(var(x)) >--> vx.  
:- show 
      state(s):: var(x) ==> vx.

:- >>> 'inductive step: add(e0,e1)'.
% inductive hypotheses
:- assume syntax(e0).       % e0 is considered a syntactic structure
:- assume syntax(e1).       % e1 is considered a syntactic structure
:- assume _S:: e0 ==> ve0.  % for any state _S the expression e0 terminates
:- assume _S:: e1 ==> ve1.  % for any state _S the expression e1 terminates
% induction step
:- show 
      state(s):: add(e0,e1) ==> ve0+ve1.

:- >>> 'the remaining arithmetic operators can be proved similarly'.

:- >>> 'inductive step: let(var(x),e0,e1)'.
% inductive hypotheses
:- assume syntax(e0).          % e0 is considered a syntactic structure
:- assume syntax(e1).          % e1 is considered a syntactic structure
:- assume _S:: e0 ==> ve0.     % for any _S the expression e0 terminates
:- assume (S:: e1 ==> ve1 :-   % for any S the expression e1 terminates if x is defined
      S:: lookup(var(x)) >--> _V).
% induction step
:- show 
      state(s):: let(var(x),e0,e1) ==> ve1.

:- qed.
```
The comments in the proof score should be self-explanatory.
When executing this proof score we get,
```
SPA - Semantics Proof Assistant Version 4.0
(C) University of Rhode Island
type 'help(spa)' for additional help
type 'halt' to exit
>>> show that all expressions terminate
>>>   (forall exp)(forall s)(exists K)[s::exp==>K]
>>> we prove this by structural induction on expressions
>>> Here are the grammar rules for expressions:
:- dynamic (::=)/2.
:- multifile (::=)/2.

exp::=const(n)| var(x)| add(exp, exp)| sub(exp, exp)| mult(exp, exp)| let(var(x), exp, exp).

>>> base case: integer values
Assume: 
     int(n).
Show: 
     state(s)::const(n)==>n.

     ***Success***

     ***removing assumptions***

>>> base case: variables
Assume: 
     _::lookup(var(x))>-->vx.
Show: 
     state(s)::var(x)==>vx.

     ***Success***

     ***removing assumptions***

>>> inductive step: add(e0,e1)
Assume: 
     syntax(e0).
Assume: 
     syntax(e1).
Assume: 
     _::e0==>ve0.
Assume: 
     _::e1==>ve1.
Show: 
     state(s)::add(e0, e1)==>ve0+ve1.

     ***Success***

     ***removing assumptions***

>>> the remaining arithmetic operators can be proved similarly
>>> inductive step: let(var(x),e0,e1)
Assume: 
     syntax(e0).
Assume: 
     syntax(e1).
Assume: 
     _::e0==>ve0.
Assume: 
     A::e1==>ve1 :-
         A::lookup(var(x))>-->_.
Show: 
     state(s)::let(var(x), e0, e1)==>ve1.

     ***Success***

     ***removing assumptions***

     ***qed***
```
What's is interesting and important here is that the built-in
assumption manager automatically removes the assumptions at each proof step so that they don't polute the next proof step.

## Predicates

SPA introduces a number of predicates that facilitates setting up
proof scores and executing them in the Prolog environment,
```
assume A  -- state an assumption A under assumption management
define A  -- state an assumption A NOT under assumption management
remove A  -- remove an assumption manually
semantics -- list all the semantics rules
show G    -- show that a goal G holds (G can be compound)
syntax/0  -- list grammar rules
syntax/1  -- flag an atom as a syntactic structure (see inductive proof above)
qed       -- flag the end of your proof
```
The proofs in the previous section illustrate the use of these predicates.

## System Parameters

The following predicates allow you to tailor the behavior of SPA,

* `set_remove_assumptions(true/false)` when set to false the assumption manager will not remove assumptions between proof steps.
The default is true.

* `set_hard_break(true/false)` is flag indicating whether to put you in break mode at a show-goal failure.  Default is true.

* `set_halt_on_qed(true/false)` is a flag indicating whether or not to exit Prolog once proof execution is complete. Default is true.

## Installation and Running

Make sure that you have SWI-Prolog installed (swi-prolog.org).  Then copy the files `spa.pl`
and `xis.pl` from the `spa` folder of the SPA github project (github.com/lutzhamel/spa) 
to a known location, for example `spa-folder`.  Then you can start SPA in interactive mode as
```
swipl -q -f ~/spa-folder/spa.pl
```
or you can run a proof score with
```
swipl -q -f ~/spa-folder/spa.pl <proof_score>.pl
```


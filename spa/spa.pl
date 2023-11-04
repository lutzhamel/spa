% spa.pl
% version 4.0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SPA -- Semantics Proof Assistant
% (C) 2015-  Lutz Hamel, University of Rhode Island

:- writeln('SPA - Semantics Proof Assistant Version 4.0').
:- writeln('(C) University of Rhode Island').
:- writeln('type \'help(spa)\' for additional help').
:- writeln('type \'halt\' to exit').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% make sure our unification algorithm is sound
:- set_prolog_flag(occurs_check,true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates that are defined in the DB to support SPA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% assumption manager
:- asserta(assumptions([])).           % list of assumptions
:- asserta(remove_assumptions(true)).  % automatically remove assumptions true/false

set_remove_assumptions(Val) :-         % toggle the remove assumptions flag
	member(Val,[true,false]),
	remove_assumptions(A),              % get current value of flag
	retract(remove_assumptions(A)),
	asserta(remove_assumptions(Val)).

% flag indicating whether to break at show-goal failure
:- assert(hard_break(true)).

set_hard_break(Val) :-                % toggle flag
	member(Val,[true,false]),
	hard_break(A),                     % get current value of flag
	retract(hard_break(A)),
	asserta(hard_break(Val)).

% flag indicating whether or not to exit once proof is complete
:- assert(halt_on_qed(true)).

set_halt_on_qed(Val) :-               % toggle flag
	member(Val,[true,false]),
	halt_on_qed(A),                    % get current value of flag
	retract(halt_on_qed(A)),
	asserta(halt_on_qed(Val)).

% make sure our resolution algorithm is sound - negation of non-ground
% terms can lead to unsound refutations.  Therefore we should never
% negate a goal that is not ground.  Always use 'not' instead of
% '\+' to insure soundness.
%
% The predicate 'not(+Goal)' checks whether the Goal is ground,
% if it is then it negates it otherwise it will throw an exception.
:- dynamic not/1.
not(G) :- 
	ground(G),
	!,
	\+(G).
not(G) :- 
	term_string(G,SG),
	string_concat('literal is not ground: ',SG,SC),
	throw(SC).

% type checkers
:- dynamic int/1.                
int(X) :- integer(X).

:- dynamic bool/1.
bool(true).
bool(false).

% TODO: expand this to make sure that the atom conforms to variable
% name conventions.
:- dynamic name/1.
name(X) :- atom(X).

% predicate for the definition of grammars
% example: a ::= b | c.
:- op(1200,xfx,::=).
:- dynamic (::=)/2.                
:- multifile (::=)/2.

% functor for the definition of State::Structure configurations
:- op(800,xfx,::).
:- dynamic (::)/2.                
:- multifile (::)/2.

% the structure State :: Syntax ==> SemanticValue is interpreted
% as: the Syntax maps to the SemanticValue under the assumptioin of
% State.  The structure (State :: Syntax) is called a configuration.
:- op(900,xfx,==>).
:- dynamic (==>)/2.                
:- multifile (==>)/2.

% the structure State :: Structure ==> Value is interpreted
% as: the Structure maps to  a Value under the assumptoin of
% State.
:- op(900,xfx,>-->).
:- dynamic (>-->)/2.                
:- multifile (>-->)/2.

% the following op is a tag for type rules.
:- op(950,fy,type).
:- dynamic (type)/1.                
:- multifile (type)/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the following predicates constitute our proofscore meta-language
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- >>> 'this will print to the terminal...'.
:- op(1150,fy,>>>).
>>> T :- write('>>> '), writeln(T).

% declare an assumption that is managed by the assumption manager
:- op(1150,fy,assume).
assume E :- 
	writeln('Assume: '),
	current_output(OUT),
	portray_clause(OUT,E,[indent(5)]),
	check(E),
	remember_assumption(E),
	asserta(E).

% declare an assumption that is NOT managed by the assumption manager
:- op(1150,fy,define).
define E :- 
	writeln('Define: '),
	current_output(OUT),
	portray_clause(OUT,E,[indent(5)]),
	check(E),
	assertz(E). % put the definitions in the database in the order we see them

syntax :-
	listing(_::=_).

semantics :-
	listing(_==>_).

% remove an assumption
:- op(1150,fy,remove).
remove E :-               % if assumption is managed then remove it from assumption manager
	assumptions(A),
	member(E,A),
	writeln('Remove: '),
	current_output(OUT),
	portray_clause(OUT,E,[indent(5)]),
	% delete from list
	retract(assumptions(A)),
	delete(A,E,ANew),
	asserta(assumptions(ANew)),
	retract(E).

remove E :- 
	writeln('Remove: '),
	current_output(OUT),
	portray_clause(OUT,E,[indent(5)]),
	retract(E).

% show that something actually holds
:- op(1150,fy,show).
show G :- 
	writeln('Show: '),
	current_output(OUT),
	portray_clause(OUT,G,[indent(5)]),
	check(G),
	call(G),
	nl,
	writeln("     ***Success***"),
	nl,
	remove_assumptions,
	!.

show _ :-                    % either check or call failed
	hard_break(true),
	nl,
	writeln('     ***Show Goal failed***'),
	nl,
	break. 

show _ :- 
	nl,
	writeln('     ***Show Goal failed***'),
	nl. 

% flag the end of the proof
qed :- 
	halt_on_qed(true),
	writeln("     ***qed***"),
	halt.

qed :- 
	writeln("     ***qed***").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% The following set of rules constitute a simple sanity
% checker that makes sure that the (_::Syntax==>_) and (_::Val>-->_)
% functors have the proper arguments for syntax and values, respectively.
%
% NOTE: we use the grammar definition for this, grammar rules 
%       defined with the (::=) functor.

:- op(1150,fy,check).

% procedure for checking semantic mappings
check(_::Syntax ==> _) :- 
	var(Syntax),
	!.

check(_::Syntax ==> _) :- 
	% see if we have an assumption about the syntax in our db
	clause(_::Syntax ==>_,true), 
	!.

check(_::Syntax ==> _) :- 
	check_syntax(Syntax),
	!.

check(_::Syntax ==> _) :- 
	writeln("     ***unknown syntax term detected***"),
	current_output(OUT),
	portray_clause(OUT,Syntax,[indent(5)]),
	!.

% procedure for checking value computations
check(_::Val >--> _) :- 
	var(Val),
	!.

% this needs to fail because this is not a semantic mapping.
check(_::Val >--> _) :- 
	check_syntax(Val),
	writeln("     ***invalid value detected***"),
	current_output(OUT),
	portray_clause(OUT,val,[indent(5)]),
	!.

% general recursion over proof structures
check(G) :-
	functor(G,_Name,0),  % nullary functor, base case
	%writeln(G),
	!.

check(G) :-
	%functor(G,Name,_Arity), writeln(Name),
	forall(arg(_,G,Child), check(Child)),
	!.

check(G) :-
	writeln("***check: functor not recognized:"),
	portray_clause(G),
	!,
	fail.

% see if the toplevel functor of term S is valid syntax
% NOTE: this is not a complete syntax check, only the toplevel is checked
check_syntax(S) :-
	functor(S,Name,_Arity),
	clause(syntax(Name),true).  % see if we have a syntax assumption in the DB

check_syntax(S) :- 
	grammar_functors(Functors),
	functor(S,Name,_Arity),
	member(Name,Functors). % if the toplevel functor is a grammar term

grammar_functors(Functors) :- 
	findall(B,_::=B,Bs),
	functors_in_bodies(Bs,Functors).

functors_in_bodies([],[]).

functors_in_bodies([B|T],Functors) :-
	functors_in_body(B,FB),
	functors_in_bodies(T,FT),
	append(FB,FT,Functors).

functors_in_body(B,FB) :-
	functor(B,Name,_Arity),
	Name \= '|',
	FB = [Name].

functors_in_body(F'|'Rest,FB) :-
	functor(F,Name,_Arity),
	functors_in_body(Rest,FR),
	FB = [Name|FR].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% assumption manager

remember_assumption(E) :- 
	remove_assumptions(true),  % only do this if remove_assumption is true
	assumptions(A),
	retract(assumptions(A)),
	asserta(assumptions([E|A])).
remember_assumption(_).

remove_assumptions :-
	remove_assumptions(true), % only do this if remove_assumption is true
	writeln("     ***removing assumptions***"),
	nl,
	assumptions(A),           % get the list of assumptions
	retract(assumptions(A)),  % delete list from db
	assert(assumptions([])),  % assert an empty assumption list
	remove_assumption(A).     % remove each assumption on list from db
remove_assumptions.  % do this if remove_assumptions(false)

remove_assumption([]).
remove_assumption([A|AT]) :-
	retract(A),
	remove_assumption(AT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% load the definition of the 'xis' predicate

:- consult('xis.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
help(spa) :-
	writeln('In addition to all Prolog predicates the following SPA predicates are available:'),
	writeln('assume A  -- state an assumption A under assumption management'),
	writeln('define A  -- state an assumption A NOT under assumption management'),
	writeln('remove A  -- remove an assumption manually'),
	writeln('semantics -- list all the semantics rules'),
	writeln('show G    -- show that a goal G holds (G can be compound)'),
	writeln('syntax    -- list grammar rules'),
	writeln('qed       -- flag the end of your proof').

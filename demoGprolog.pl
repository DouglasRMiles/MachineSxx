% Copyright: (C) Heike Stephan,  Stefan Brass, 2012
% Copying: Permitted under the GNU General Public License.
% This program is free software: you can redistribute it and/or  
% modify it under the terms of the GNU General   
% Public License as published by the Free Software      
% Foundation, either version 3 of the License, or (at   
% your option) any later version.                       
                                                       
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied    
% warranty of MERCHANTABILITY or FITNESS FOR A          
% PARTICULAR PURPOSE. See the GNU General Public 
% License for more details.                                                       
%       http://www.gnu.org/licenses/  

%:- license(gpl).

writeln(X):- write(X),nl.

ground(X):- nonvar(X),ground_0(X).
ground_0(X):- \+ compound(X),!.
ground_0([H|T]):- !,ground(H),ground(T).
ground_0(X):- X=..[_|Args],!,ground(Args).

term_variables(X,[X]):- var(X),!.
term_variables(X,[]):- \+ compound(X),!.
term_variables([H|T],HTV):- !,term_variables(H,HV),term_variables(T,TV),append(HV,TV,HTV).
term_variables(X,AV):- X=..[_|Args],!,term_variables(Args,AV).

get_single_char(X):- get0(X).

assert(_).
=@=(X,Y):- \+ \+ (numbervars(X+Y),X==Y). 
maplist(_,_).
maplist(_,_,_).
maplist(_,_,_,_).


%==============================================================================
% Parser for Input Programs.
% The parser uses code from:
%
% sldmagic.pl
% Copyright:	(C) 1996-2000  Stefan Brass
% Copying:	Permitted under the GNU General Public License.
%==============================================================================
%:- license(gpl).

%------------------------------------------------------------
% Operator '<-' for rule declaration; substitutes the Prolog ':-'.
%------------------------------------------------------------
:- op(1200, xfx, '<-').

%------------------------------------------------------------------------------
% List of Facts:
%------------------------------------------------------------------------------

:- dynamic(fact/1).

%------------------------------------------------------------------------------
% List of IDB-Predicates:
% First argument is the predicate name,
% second argument is its arity.
%------------------------------------------------------------------------------

:- dynamic(idb_pred/2).

%------------------------------------------------------------------------------
% List of EDB-Predicates:
% First argument is the predicate name,
% second argument is its arity.
%------------------------------------------------------------------------------

:- dynamic(edb_pred/2).

%------------------------------------------------------------------------------
% List of Program Rules:
% The first argument is the rule head, the second a list of its body literals.
%------------------------------------------------------------------------------

:- dynamic(rule/2).

%------------------------------------------------------------------------------
% load_program(+Filename):
%------------------------------------------------------------------------------

% This predicate reads the input file and stores all its information in
% the dynamic database.
% It fails on a syntax error.

load_program(Filename) :-
	retractall(fact(_)),
	retractall(edb_pred(_,_)),
	retractall(idb_pred(_,_)),
	retractall(rule(_,_)),
	open(Filename, read, In_Stream),
	(read_rules(In_Stream) ->
		close(In_Stream), write('Program loaded.'), nl
	;
		close(In_Stream),
		fail).

%------------------------------------------------------------------------------
% read_rules(+In_Stream):
%------------------------------------------------------------------------------

% This predicate reads each line until the end of file.
% (More precisely, not lines are read, but Prolog terms.)
% For every line, process_term is called.
% It fails on a syntax error (i.e. when process_term fails).

read_rules(In_Stream) :-
	read(In_Stream, Term),
	(Term == end_of_file ->
		true
	;
		process_term(Term),
		!,
		read_rules(In_Stream)).

%------------------------------------------------------------------------------
% process_term(+Term):
%------------------------------------------------------------------------------

% This predicate is called for every input line (more precisely Prolog term).
% Its task is to process the input line, i.e. parse it and save the result
% in the dynamic database.
% The real work is done by parse_term_save_result.
% If something goes wrong (e.g. a syntax error) 
% this predicate shows the input line to the user and tells him/her that
% it contains an error.
% That is a very rudimentary error message, but better than nothing.
% It should be improved in a future version.

process_term(Term) :-
	parse_term_save_result(Term), !.

process_term(Term) :-
	write('Error at parsing:  '),
	write(Term),
	write('.'),
	nl,
	fail.

%------------------------------------------------------------------------------
% parse_term_save_result(+Term):
%------------------------------------------------------------------------------

% This predicate mainly distinguishes between different types of input lines
% and delegates the work to the appropriate predicate.
% All processing predicates fail upon a syntax error and print an error message.
% The calling predicate process_term will print a small error message.

% Edb predicates may be declared by the term db(Pred, Arity)
% where Pred is the predicate name and Arity its arity.
parse_term_save_result(db(Pred,Arity)) :-
	!, save_edb(Pred,Arity).

% Recognizes a program rule.
parse_term_save_result((Head <- Body)) :-
	!,
	parse_head(Head),
	parse_body(Body, BodyList),
	save_rule(Head, BodyList).

% Recognizes a query, which is ignored. Queries have to be put in separately.
parse_term_save_result((:- _)) :-
	!,
	write('Queries in program text are ignored.'),
	nl.

% Recognizes a ground fact.
parse_term_save_result(Fact) :-
	callable(Fact),
	parse_literal(Fact),
	ground(Fact),!,
	save_fact(Fact), 
	!. 
	
%------------------------------------------------------------------------------
% save_fact(+Fact):
%------------------------------------------------------------------------------

% This predicate is called for every fact in the input program.
% If the predicate of this fact is not yet defined either as idb or as edb
% predicate, it is implicitly stored as edb predicate. For a predicate 
% identifier only one arity is allowed. 
% Idb predicates and edb predicates are ensured to be disjoint.

save_fact(Fact) :-
	fact(Fact),
	!.

save_fact(Fact) :-
	functor(Fact, Pred,Arity),
	edb_pred(Pred,Arity),!,
	assert(fact(Fact)).

save_fact(Fact) :-
	functor(Fact, Pred, Arity),
	\+ idb_pred(Pred, _),
	\+ edb_pred(Pred, _),!,
	assert(edb_pred(Pred,Arity)),
	assert(fact(Fact)).

save_fact(Fact) :-
	write('Error at storing fact '),
	write(Fact),
	nl,
	fail.
	
%------------------------------------------------------------------------------
% save_edb(+Edb):
%------------------------------------------------------------------------------

% This predicate processes the declarations of database (edb) predicates.
% It stores the declaration in the dynamic database if it is not already there.
% For a predicate identifier only one arity is allowed. 
% Idb predicates and edb predicates are ensured to be disjoint.

save_edb(Pred, Arity) :-
	edb_pred(Pred, Arity),
	!.

save_edb(Pred, Arity) :-
	\+ idb_pred(Pred, _),
	\+ edb_pred(Pred, _),!,
	assert(edb_pred(Pred,Arity)).
	
save_edb(Pred, Arity) :-
	write('Error at storing EDB-Predicate '),
	write(Pred, Arity),
	nl,
	fail.
	
%------------------------------------------------------------------------------
% save_idb(+Pred):
%------------------------------------------------------------------------------

% This predicate is called for every predicate in the input program that
% occurs in the head of a rule, which is therefore stored as idb predicate.
% For a predicate identifier only one arity is allowed. 
% Idb predicates and edb predicates are ensured to be disjoint.

save_idb(Idb) :-
	functor(Idb, Pred, Arity),
	idb_pred(Pred, Arity),
	!.

save_idb(Idb) :-
	functor(Idb, Pred, Arity),
	\+ edb_pred(Pred,_),	
	\+ idb_pred(Pred,_),!,
	assert(idb_pred(Pred,Arity)).

save_idb(Idb) :-
	write('Error at storing IDB-Predicate '),
	write(Idb),
	nl,
	fail.

%------------------------------------------------------------------------------
% save_rule(+Internal_Head, +Internal_Body):
%------------------------------------------------------------------------------

% This predicate stores a rule from the input program in internal format
% in the dynamic database if it is not already there.

% This version should do the check for an existing rule.
save_rule(Internal_Head, Internal_Body) :-
	functor(Internal_Head, FunctorHead, ArityHead), 
	maplist(functor, Internal_Body, FunctorBody, ArityBody),
	functor(NewHead, FunctorHead, ArityHead),
	maplist(functor, NewBody, FunctorBody, ArityBody),
	
	rule(NewHead, NewBody),
	'=@='((Internal_Head, Internal_Body) , (NewHead, NewBody)),
	!.

save_rule(Internal_Head, Internal_Body) :-
	assert(rule(Internal_Head, Internal_Body)), !.
	
save_rule(Internal_Head, Internal_Body) :-
	write('Error at storing rule '),
	write(Internal_Head), write(' :- '), write(Internal_Body),
	nl,
	fail.

%------------------------------------------------------------------------------
% parse_head(+Head):
%------------------------------------------------------------------------------

% This predicate checks a head of an input rule for syntactical correctness.
% The head predicate is stored as idb predicate.

parse_head(Head) :-
	callable(Head),!,
	parse_literal(Head),	
	save_idb(Head).

parse_head(Head) :-
	write('Error at parsing rule head: '),
	write(Head),
	nl,
	fail.
%------------------------------------------------------------------------------
% parse_body(+Body, -BodyList):
%------------------------------------------------------------------------------

% This predicate checks the body of an input rule for correctness
% and translates it into an internal format, which is a list of literals.
% This list is returned in BodyList.

parse_body(','(Atom,Body), [Atom | BodyList]) :-
	!,
	parse_literal(Atom),
	parse_body(Body, BodyList).

parse_body(Atom, [Atom]) :-
	parse_literal(Atom), !.

parse_body(Term, _) :-
	write('Error at parsing rule body: '),
	write(Term),
	nl,
	fail.

%------------------------------------------------------------------------------
% parse_query(+Query, -InternalQuery):
%------------------------------------------------------------------------------

% Parsing the query. Parsing is done by calling parse_body. InternalQuery returns
% the parsed query in the format (answer(VarPairs), QueryList) where QueryList
% is a list of literals.

parse_query(Query, (answer(VarList), QueryList)) :-
	term_variables(Query,VarList),
	parse_body(Query, QueryList), !.
	
parse_query(Query, _) :-
	write('Error at parsing query: '),
	write(Query),
	nl, fail.

%------------------------------------------------------------------------------
% parse_literal(+Lit):
%------------------------------------------------------------------------------

% This predicate checks a literal from a body of an input rule for correctness.
% The literal should be callable and its arguments not compound.

parse_literal(Lit) :-
	callable(Lit),
	Lit =.. [_|Args],
	maplist(parse_arg, Args), !.
	
parse_literal(Lit) :-
	write('Error at parsing literal: '),
	write(Lit),
	nl,
	fail.

%------------------------------------------------------------------------------
% parse_arg(?Arg):
%------------------------------------------------------------------------------

% We do not allow structured terms, so every argument in the input program
% must be atomic or a variable.

parse_arg(Arg) :-
	atomic(Arg), !.

parse_arg(Arg) :-
	var(Arg), !.

parse_arg(Arg) :-
	write('Error: No structured terms like '),
	write(Arg),
	write(' allowed.'),
	nl, fail.

%===============================================
% User interface.
%===============================================

ui :-
	nl,nl,
	write('Demo of Datalog deduction based on adapted Earley Algorithm. '),nl,nl,
	write('Use \'load_file(File)\' to load and parse a program/data file.'),nl,
	write('The syntax for rules is: RuleHead <- Term1, Term2, ..., TermN.'),nl,
	write('Use \'clean\' to clean the dynamic database.'),nl,
	write('Use query((Query)) to enter a query where Query is a'), nl,
	write('conjunction of terms.'),nl,
	write('By entering \'set_sld(no)\' or \'set_sld(yes)\' the last literal'),nl,
	write('resolution optimization for deriving items can be switched off and on;'),nl,
	write('default is sld(yes).'),nl,
	write('(Copyright: (C) Heike Stephan,  Stefan Brass, 2012'),nl,
	write('Copying: Permitted under the GNU General Public License.)'), nl,nl.

%------------------------------------------------------------------------------
% List of states to be processed.
% A state has a unique number, a list of rules, and a list of fact calls.
% A rule has a unique number, a head literal, and a list of body literals.
%------------------------------------------------------------------------------

:- dynamic(state_list/1).

%------------------------------------------------------------------------------
% Answer relation. The argument is a list of Variables occurring in the query.
% Currently not used.
%------------------------------------------------------------------------------

:- dynamic(answer/1).

%------------------------------------------------------------------------------
% Next number to assign to a state.
%------------------------------------------------------------------------------
:- dynamic(next_state/1).

%------------------------------------------------------------------------------
% Next number to assign to a derived rule.
%------------------------------------------------------------------------------
:- dynamic(next_rule/1).

%------------------------------------------------
% sld/1
%------------------------------------------------
% Dynamic database predicate that holds information 
% whether evaluation should use Last Literal Resolution. 
% Is set via set_sld/1.
%------------------------------------------------
:- dynamic(sld/1).

%-------------------------------
% clean:
%
% Cleans the dynamic database.
%-------------------------------

clean :-
	!,
	retractall(sld(_)),
	retractall(fact(_)),
	retractall(idb_pred(_,_)),
	retractall(edb_pred(_,_)),
	retractall(rule(_,_)),
	%retractall(derived(_,_,_)),
	retractall(state_list(_)),
	retractall(answer(_)),
	retractall(next_rule(_)),
	retractall(next_state(_)).

%--------------------------------
% load_file(+File)
%--------------------------------
% Loads and parses a source file.
%--------------------------------

load_file(File) :-
	!,
	load_program(File), !.
	
%--------------------------------------------------
% set_sld(+Opt)
%--------------------------------------------------
% Determines whether SLD Resolution should be used.
% Opt is either 'yes' or 'no'.
%--------------------------------------------------
set_sld(yes) :-
	!,
	retractall(sld(_)),
	assert(sld(yes)).
	
set_sld(no) :-
	!,
	retractall(sld(_)),
	assert(sld(no)).
	
%---------------------------------------------------
% query(+Query)
%---------------------------------------------------
% Enter query Query and start evaluating it with the
% adapted Earley Algorithm.
%---------------------------------------------------
query(Query) :-
	parse_query(Query, InternalQuery),
	initDatabase(InternalQuery),
%running adapted Earley algorithm
	repeat,
	exec_loop,!, 
	cleanUp,!.
	
%-----------------------------------------------------------------------
% initDatabase(+InternalQuery)
%-----------------------------------------------------------------------
% After a query has been read and parsed to the internal query format,
% this predicate removes all data from the previous query evaluation and
% inits the dynamic database for a new evaluation.
%-----------------------------------------------------------------------
initDatabase(InternalQuery) :-
	cleanUp,
%initialisation
	assert(next_state(2)),
	assert(next_rule(2)),
	assert(state_list([(0, [(1, (InternalQuery))], _)])).
	
%--------------------------------------------------
% cleanUp
%--------------------------------------------------
% Removes all data from previous query evaluations.
%--------------------------------------------------

cleanUp:-
	retractall(state_list(_)),
	retractall(answer(_)),
	retractall(next_rule(_)),
	retractall(next_state(_)).
	
%==============================================================================
% Interpreter:
%==============================================================================

%==============================================================================
% Implementation of adapted Earley Algorithm.
%==============================================================================

%-------------------------------------------------------------------------------
% exec_loop
%-------------------------------------------------------------------------------
% The execution loop for query evaluation. Executes the algorithm until
% no states are left for processing, the user has stopped evaluation or an error
% has occurred. At each loop, the user is asked to continue.
% Forcing the predicate to fail leads to backtracking on the query execution.
% The successful return of the predicate leads to execution of the code after
% the call to "exec_loop" in the predicate query/1. 
%-------------------------------------------------------------------------------
	
exec_loop :-
	derive, !, 
	retractall(answer(_)),
	ask_more('n'),
	retractall(state_list(_)),
	retractall(next_state(_)),  
	retractall(next_rule(_)).
exec_loop :-
	 write('Fail.'), nl.	

ask_more(Atom) :-
	write('More? (y/n) '),
	get_single_char(Char),
	put_code(Char),nl,nl,
	atom_codes(Atom, [Char]).	

%---------------------------------------------------------------------------
% derive
%---------------------------------------------------------------------------
% Takes one state out of the state list, completes the initial state (if present),
% creates the following states, adds them to the state list and prints all relevant
% information on this process.
%---------------------------------------------------------------------------
derive :-
	state_list([]),
	retractall(state_list(_)),
	retractall(next_state(_)),  
	retractall(next_rule(_)),
	!, fail.

derive :-
	state_list([(StateNr, RulesIn, FactCalls)|Others]),!,
	constructInitialState(StateNr, RulesIn, RulesOut, FactCalls),!,
	
	next_state(NextStateNr),
	initNextStates(NextStateNr, NextStateNrOut, RulesOut, FactCalls, FactAnswers, NextInits),!,
	retract(next_state(_)),
	assert(next_state(NextStateNrOut)),
	constructNextStates(RulesOut, NextInits, NextStates),!,
	normalize(RulesOut, NormRules),
	normalize(FactCalls, NormCalls),
	write('State '), write(StateNr), writeln(':'),nl,
	maplist(printItem, NormRules),
	nl,
	writeln('Fact Calls: '), maplist(printFactCall,NormCalls),nl,
	%writeln('Fact Calls: '), maplist(printFactCall,FactCalls),nl,
	maplist(printFactAnswer,FactAnswers, NextStates),!,
	
	append(Others, NextStates, StateList),
	retract(state_list(_)),
	assert(state_list(StateList)),
	!.
	
%-----------------------------------------------------------------
% constructInitialState(+StateNr, +RulesIn, -RulesOut, -FactCalls)
%-----------------------------------------------------------------
% If the state currently processed is the initial state,
% this predicate completes it.
% StateNr identifies the state, RulesIn are the incoming rules 
% from which to derive, RulesOut are the rules of the state
% after derivation is complete, and FactCalls are the calls in
% for EDB predicates in this state.
%-----------------------------------------------------------------
constructInitialState(0, RulesIn, RulesOut, FactCalls) :-
	!,
	instantiate([], RulesIn, RulesOut, [], FactCalls).
constructInitialState(_,RulesIn,RulesIn,_).
	
%--------------------------------------------------------------------------
% constructNextStates(+PreStateRules, +NextInits, -NextStates)
%--------------------------------------------------------------------------
% After performing state transitions and initializing the successor states,
% this predicate completes the successor states.
% PreStateRules are the rules of the preceding state,
% NextInits is the list of initialized states,
% and NextStates is the list of complete successor states.
%--------------------------------------------------------------------------
constructNextStates(_, [], []) :-
	!.
	
constructNextStates(PreStateRules, [(StateNr, RulesIn, FactCalls)|NextInits], [(StateNr, RulesOut, FactCalls)|NextStates]) :-
	constructState(PreStateRules, RulesIn, RulesOut, FactCalls),
	constructNextStates(PreStateRules, NextInits, NextStates), !.

%------------------------------------------------------------------------------
% constructState(+PreStateRules, +RulesIn, -RulesOut, -FactCalls)
%------------------------------------------------------------------------------
% Completes the construction of the input state, containing RulesIn, with use of
% the rules of the preceding state, PreStateRules. Returns a list of its rules, 
% Rules Out, and the fact calls FactCalls from which state transitions are created.
%------------------------------------------------------------------------------
	
constructState(PreStateRules, RulesIn, RulesOut, FactCalls) :-
	reduce(PreStateRules, [], RulesIn, RulesOut1),
	instantiate([], RulesOut1, RulesOut2, [], FactCalls),
	copyRules(PreStateRules, [], RulesOut2, RulesOut),
	!.
	
%------------------------------------------------------------------------------------------
% instantiate(+RulesDone, +RulesToDo, -RulesOut, +FactCallsIn, -FactCallsOut)
%------------------------------------------------------------------------------------------
% Recursive predicate that performs the instantiation and possibly last literal
% reduction steps on the input rules RulesToDo. Processed rules are contained in RulesDone.
% The resulting rules are stored in RulesOut. Generated calls to EDB predicates
% are output in FactCallsOut, the calls from previous predicate iterations are kept
% in FactCallsIn. 
%------------------------------------------------------------------------------------------
instantiate(RulesDone, [], RulesDone, FactCalls, FactCalls) :- !.
 
instantiate(RulesDone, [(Id, (answer(Args), []))|RulesToDo], RulesOut, FactCallsIn, FactCallsOut) :-
	!,
	assert(answer(Args)),
	append(RulesDone, [(Id, (answer(Args), []))], RulesDone2),
	instantiate(RulesDone2, RulesToDo, RulesOut, FactCallsIn, FactCallsOut),!.
	
instantiate(RulesDone, [(Id, (Head, []))|RulesToDo], RulesOut, FactCallsIn, FactCallsOut) :- !,
	append(RulesDone, [(Id, (Head, []))], RulesDone2),
	instantiate(RulesDone2, RulesToDo, RulesOut, FactCallsIn, FactCallsOut),!.

instantiate(RulesDone, [(RuleNr, (Head, [First|Body]))|RulesToDo], RulesOut, FactCallsIn, FactCallsOut) :-
	functor(First, Fun, Ar),
	edb_pred(Fun, Ar),!,
	insertFactCalls([First], FactCallsIn, FactCallsOut2),
	append(RulesDone, [(RuleNr, (Head, [First|Body]))], RulesDone2),
	instantiate(RulesDone2, RulesToDo, RulesOut, FactCallsOut2, FactCallsOut), !.

% In any case, if there is more than one body literal use instantiation
instantiate(RulesDone, [(RuleNr, (Head, [First, Second|Body]))|RulesToDo], RulesOut, FactCallsIn, FactCallsOut) :-
	functor(First, Fun, Ar),
	idb_pred(Fun, Ar),!,
	findInstances(First, Instances),
	processDerived(RulesDone, RulesDone2, [(RuleNr, (Head, [First, Second|Body]))|RulesToDo], RulesToDo2, Instances),
	instantiate(RulesDone2, RulesToDo2, RulesOut, FactCallsIn, FactCallsOut),!.
	
% if sld option is chosen, and as default, use resolution for the last literal.
instantiate(RulesDone, [(RuleNr, (Head, [First|[]]))|RulesToDo], RulesOut, FactCallsIn, FactCallsOut) :-
	functor(First, Fun, Ar),
	idb_pred(Fun, Ar), !,
	(sld(no) -> 
		findInstances(First, DerivedRules)
	;
		findResolves(Head, First, DerivedRules)
	),
	processDerived(RulesDone, RulesDone2, [(RuleNr, (Head, [First|[]]))|RulesToDo], RulesToDo2, DerivedRules),
	instantiate(RulesDone2, RulesToDo2, RulesOut, FactCallsIn, FactCallsOut),!.

% Auxiliary predicates for instantiate/5.
findInstances(Lit, Instances) :-
	findall((_,(Lit, Body)), rule(Lit, Body),Instances).
	
findResolves(Head, First, Derived) :-
	findall((_,(Head, Body)), rule(First, Body), Derived).
	
%----------------------------------------------------------
% reduce(+PreStateRules, +RulesDone, +RulesToDo, -RulesOut)
%----------------------------------------------------------
% PreStateRules: Rule set of previous state
% RulesDone: Rules of current state already processed by reduction
% RulesToDo: Rules of current state to be processed by reduction
% RulesOut: All rules of current state after reduction phase
%----------------------------------------------------------
reduce(_, RulesOut, [], RulesOut) :- !.

% processed rule is a fact.
reduce(PreStateRules, RulesDone, [(RuleNr, (Fact, []))|RulesIn], RulesOut) :- !,
% beware of unwanted unification problems!
	findReducts(PreStateRules, Fact, Reducts),
	processDerived(RulesDone, RulesDone2, [(RuleNr, (Fact, []))|RulesIn], RulesIn2, Reducts),
	reduce(PreStateRules, RulesDone2, RulesIn2, RulesOut), !.

% processed rule is not a fact.
reduce(PreStateRules, RulesDone, [(RuleNr, (Head, [First|Body]))|RulesIn], RulesOut) :- !,
	append(RulesDone, [(RuleNr, (Head, [First|Body]))], RulesOut2),
	reduce(PreStateRules, RulesOut2, RulesIn, RulesOut),!.
	
% Auxiliary predicate for reduce/4.
findReducts(PreStateRules, Fact, Reducts):-
	sld(no) ->
	findall((_,(Head, BodyTail)), member((_, (Head, [Fact|BodyTail])), PreStateRules), Reducts);
	findall((_,(Head, [Lit2|BodyTail])), member((_, (Head, [Fact, Lit2|BodyTail])), PreStateRules), Reducts).

% ---------------------------------------------------------------------
%copyRules(+PreStateRules, +RulesDone, +RulesToDo, -RulesOut)
% ---------------------------------------------------------------------
% Copies rules necessary for later reduction from the preceding state.
% PreStateRules: Rules from the preceding state
% RulesDone: Rules processed by copyRules
% RulesToDo: Rules still to be processed
% RulesOut: All resulting rules
% ---------------------------------------------------------------------
copyRules(_, RulesDone, [], RulesDone):-
	!.

copyRules(PreStateRules, RulesDone, [(RuleNum, (Head, [First|Body]))|RulesToDo], RulesOut) :-
	(sld(no) -> 
		findall((_, (HeadPre, [FirstPre|BodyPre])), (member((_, (HeadPre, [FirstPre|BodyPre])), PreStateRules),unifiable(FirstPre, Head, _)), ToCopy)
		;
		findall((_, (HeadPre, [FirstPre, Second|BodyPre])), (member((_, (HeadPre, [FirstPre,Second|BodyPre])), PreStateRules), unifiable(FirstPre, Head, _)), ToCopy)
	),
	processDerived(RulesDone, RulesDoneOut, [(RuleNum, (Head, [First|Body]))|RulesToDo], RulesToDoOut, ToCopy),
	copyRules(PreStateRules, RulesDoneOut, RulesToDoOut, RulesOut),!.
	
copyRules(PreStateRules, RulesDone, [(RuleNum, (Head, []))|RulesToDo], RulesOut) :-
	append(RulesDone, [(RuleNum, (Head, []))], RulesDone2),
	copyRules(PreStateRules, RulesDone2, RulesToDo, RulesOut),!.

%-------------------------------------------------------------------------------
% detectNewRules(+NewRules, +RuleListIn, -NewRulesOut)
%-------------------------------------------------------------------------------
% Detects which of the rules in NewRules have already equal (with variable renaming)
% rules in RuleListIn. The remaining new rules are returned in NewRulesOut.
%-------------------------------------------------------------------------------

detectNewRules([], _, []) :- !.
 
detectNewRules([Item1|NewItems], ItemList, NewItemsOut) :-
	detectNew(Item1, ItemList, NewOut),	
	detectNewRules(NewItems, ItemList, NewItemsOutTemp),
	append(NewOut, NewItemsOutTemp, NewItemsOut),
	!.
	
%-------------------------------------------------------------------------------
% detectNew(+CanditateRule, +RuleListIn, -NewRules)
%-------------------------------------------------------------------------------	
% Checks if the single rule CandidateRule is equal (with variable renaming) to
% a rule in RuleListIn. If not it is returned in the list NewRules.
%-------------------------------------------------------------------------------
	
detectNew(Item, [], [Item]):- 
	!.	

detectNew((_, Clause1), [(_, Clause2)|_], []) :-
	%%%%%%%%%%%%%%%%
	% For the comparison of clauses '=@=' cannot be used since comparisons like
	% p(A,B) =@= p(C,A) fail which should not be the case. The test for unification
	% is too weak since p(A,A) \= p(C,B) fails which should not be the case.
	%%%%%%%%%%%%%%%%% 
	equalClause(Clause1, Clause2),!.
	
detectNew((Id1, Clause1), [_|ItemListIn], NewItems) :-
	detectNew((Id1, Clause1), ItemListIn, NewItems),!.

%--------------------------------------------------------------------------------------
% processDerived(+RulesDone, -RulesDoneOut, +[Rule|RulesToDo], -RulesToDoOut, +DerivedRules)
%--------------------------------------------------------------------------------------
% Auxiliary predicate. Moves the currently processed rule Rule to RulesDone, returning
% RulesDoneOut, and checks if the Rules in DerivedRules are new. New Rules are
% appended to RulesToDo, returning RulesToDoOut.
%--------------------------------------------------------------------------------------
processDerived(RulesDone, RulesDoneOut, [Rule|RulesToDo], RulesToDoOut, DerivedRules) :-
	append(RulesDone, [Rule], RulesDoneOut),
	append(RulesDoneOut, RulesToDo, AllRules),
	%write('derived: '), writeln(DerivedRules),
	detectNewRules(DerivedRules, AllRules, NewRules),
	%writeln(3),
	next_rule(NextId),
	%write('new: '), writeln(NewRules),
	newRuleNumbers(NextId, NewRules, NextId2),
	%writeln(a4),
	retract(next_rule(_)),
	assert(next_rule(NextId2)),
	append(RulesToDo, NewRules, RulesToDoOut),!.
	
% Auxiliary predicate; gives new numbers to new derived rules.
newRuleNumbers(NextId, [], NextId) :- !.
	
newRuleNumbers(NextIdIn, [(NextIdIn, (_,_))|Rules], NextIdOut) :-
	NextId2 is NextIdIn +1,
	newRuleNumbers(NextId2, Rules, NextIdOut),!.

%------------------------------------------------------------------------------------------------------
% insertFactCalls(+FactCalls, +FactCallsIn, -FactCallsOut)
%------------------------------------------------------------------------------------------------------
% Inserts the list of FactCalls into the list FactCallsIn and returns the result in FactCallsOut.
%------------------------------------------------------------------------------------------------------

insertFactCalls(FactCalls, [], FactCalls) :- !.

insertFactCalls([], FactCallsIn, FactCallsIn) :- !.
	
insertFactCalls([Literal1], [Literal2|FactCallsIn], [Literal2|FactCallsIn]) :-
	%%%%%%%%%%%%%%%%
	% For the comparison of literals '=@=' cannot be used since comparisons like
	% p(A,B) =@= p(C,A) fail which should not be the case. The test for unification
	% is too weak since p(A,A) \= p(C,B) fails which should not be the case.
	%%%%%%%%%%%%%%%%%
	equalClause(Literal1, Literal2),!.
	
insertFactCalls([Literal1], [Literal2|FactCallsIn], [Literal2|FactCallsOut]) :-
	insertFactCalls([Literal1], FactCallsIn, FactCallsOut), !.

%-------------------------------
% equalClause(Clause1, Clause2)
%-------------------------------
% Checks whether Clause1 and Clause2 are equal.
%-------------------------------

equalClause(Clause1, Clause2) :-
	copy_term(Clause1, ClauseCopy1),
	copy_term(Clause2, ClauseCopy2),
	'=@='(ClauseCopy1 , ClauseCopy2).


% Initialize next states.
%%%%%%%%%%%%%%%%%%%%%%%%%

%------------------------------------------------------
% initNextStates(+NextStateNrIn, -NextStateNrOut, +Rules, +FactCalls, -FactAnswers, -NextStates)
%------------------------------------------------------
% Performs a state transition and initializes the successor states.
% NextStateNrIn: Number to give to the next new state.
% NextStateNrOut: New state number to be saved for the next state construction.
% Rules: Rule set of the current state.
% FactCalls: Calls to EDB predicates of the preceding state.
% FactAnswers: Facts unified with the FactCalls.
% NextStates: Initialized successor states.
%------------------------------------------------------

initNextStates(NextStateNrIn, NextStateNrIn, _, [], [], []) :- 
!.

initNextStates(NextStateNrIn, NextStateNrOut, Rules, [Literal|FactCalls], FactAnswersOut, NextStatesOut) :-
	findall(Literal, fact(Literal), FactAnswersTemp),
	initNextResult(NextStateNrIn, NextStateNrOut1, Rules, FactAnswersTemp, NextInits),
	initNextStates(NextStateNrOut1, NextStateNrOut, Rules, FactCalls, FactAnswers2, NextStates2),
	append(FactAnswersTemp, FactAnswers2, FactAnswersOut),
	append(NextInits, NextStates2, NextStatesOut),
	!.
	
%------------------------------------------------------
% initNextResult(+StateNr, -NextStateOut, +Rules, +Results, -NextStates)
%------------------------------------------------------
% For every element in the list of EDB facts Results a successor state is initialized.
% StateNr: Number to assign to the next state
% NextStateOut: Number to be saved for the next initialization phase
% Rules: Rule set of the current state.
% NextStates: Initialized successor states.
%------------------------------------------------------
	
initNextResult(NextStateNrIn, NextStateNrIn, _, [], []) :- !.
	
initNextResult(StateNr, NextStateOut, Rules, [Fact|Results], [(StateNr, Derived, _)|NextStates]) :-
	next_rule(Id),
	findall((_,(Head, Body)), member((_, (Head, [Fact|Body])), Rules), Derived),
	newRuleNumbers(Id, Derived, IdOut),
	retract(next_rule(_)),
	assert(next_rule(IdOut)),
	NextStateNr is StateNr + 1,
	initNextResult(NextStateNr, NextStateOut, Rules, Results, NextStates),!.
	
%-------------------------------------------------------------------------------
% normalizeRules(+Rules, -NormRules)
%-------------------------------------------------------------------------------
% Normalizes a list of rules Rules and returns the normalized list in NormRules.
% The rules are copied before normalization.
% Uses the built-in predicate numbervars/3.
%-------------------------------------------------------------------------------
	
normalize([], []).
	
normalize([Rule|Rules], [Rule2|NormRules]) :-
	copy_term(Rule, Rule2),
	numbervars(Rule2, 0, _),
	normalize(Rules, NormRules).
	
%------------------------------------------------------------------------------
% Printing stuff.
%------------------------------------------------------------------------------

printItem((ItemNr, (answer(VarList), []))) :- 
	write('['), write(ItemNr), write('] '), 
	Answer =.. [answer|VarList], write(Answer),writeln('.'),!.

printItem((ItemNr, (Head, []))) :-!,
	write('['), write(ItemNr), write('] '), 
	write_term(Head, [numbervars(true)]),
	writeln('.').
	
printItem((ItemNr, (answer(VarList), Body))) :-
	write('['), write(ItemNr), write('] '), 
	Answer =.. [answer|VarList], write_term(Answer, [numbervars(true)]),
	write(' <- '), printList(Body),
	writeln('.'),!.

printItem((ItemNr, (Head, Body))) :-
	write('['), write(ItemNr), write('] '), 
	write_term(Head, [numbervars(true)]),
	write(' <- '), printList(Body), 
	writeln('.').
	
printFactCall(Lit) :-
	write_term(Lit, [numbervars(true)]),
	nl.
	
printFactAnswer(Lit, (StateNr, _, _)) :-
	write_term(Lit, [numbervars(true)]),
	write(' -> State '), write(StateNr), nl.
	
printList([]):- !.

printList([Only]) :-
	write_term(Only, [numbervars(true)]), !.
	
printList([First, Second|List]) :-
	write_term(First, [numbervars(true)]), write(', '),
	printList([Second|List]).

:- initialization(ui).

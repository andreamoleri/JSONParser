% Matricola: 902011
% Studente:  Moleri Andrea

% --------------------
% AUXILIARY PREDICATES

% The predicate checks that a term is a String. 
% If it is, the String gets converted to single chars or vice versa. 
strconversion(String, Chars) :- string(String), string_chars(String, Chars).
strconversion(String, Chars, _) :- string_chars(String, Chars). 
strconversion(String) :- string(String), string_chars(String).

% The predicate verifies that a term is an atom. 
% If it is, the atom is converted to a string or vice versa
atmconversion(Atom, String) :- atom(Atom), atom_string(Atom, String).
atmconversion(Atom, String, _) :- atom_string(Atom, String). 
atmconversion(Atom) :- atom(Atom), atom_string(Atom).

% The predicate tests that a term is a term. 
% If it is, the term is converted to a string or vice versa
trmconversion(Term, String) :- atom(Term), term_string(Term, String).
trmconversion(Term, String, _) :- term_string(Term, String).
trmconversion(Term) :- atom(Term), term_string(Term).

% The predicate converts a list whose first element is an atom, 
% provided the atom is non-numeric, into a term and vice versa. 
% Specifically, the lstconversion/4 version is a forced execution of 
% the UNIV built-in operator on a list with a compound head,
% while the lstconversion/3 version is a project-specific version
lstconversion(Term, [Name | Args]) :-
    countargs(Args, Arity),
    functor(Term, Name, Arity), 
    fillargs(Term, Args).

lstconversion(Term, Name1, Name2, Args) :- 
    errorhandling(Term =.. [Name1, Name2 | Args], _, _).

% The following predicates are auxiliary to lstconversion
countargs([], Num, Num) :- !.
countargs([_ | Xs], Num, Y) :- Z is Y + 1, countargs(Xs, Num, Z).
countargs(Args, Num) :- countargs(Args, Num, 0).

fillargs(Term, Args) :- fillargs(Term, Args, 1).
fillargs(_, [], _) :- !.
fillargs(Term, [Value | Xs], Arg) :- 
    arg(Arg, Term, Value), 
    N is Arg + 1, 
    fillargs(Term, Xs, N).

% Given a sequence of characters, the predicate is used to clean up 
% apostrophes, superscripts and quotation marks, all expressed with 
% the international character encoding standard UTF-16
qtscleanup(['\u0027' | Xs], ['\u0022' | Ys]) :- qtscleanup(Xs, Ys).
qtscleanup([X | Xs], [X | Ys]) :- qtscleanup(Xs, Ys).
qtscleanup([], []).

% Given a sequence of characters, the predicate is used to clean
% up parentheses, commas and add whitespaces, all expressed with
% the international standard of character encoding UTF-16
brkcleanup(['\u003a' | Xs], [' ', '\u003a', ' ' | Ys]) :- brkcleanup(Xs, Ys).
brkcleanup(['\u007b', '\u005b' | Xs], ['\u007b' | Ys]) :- brkcleanup(Xs, Ys).
brkcleanup(['\u005b', '\u007b' | Xs], ['\u007b' | Ys]) :- brkcleanup(Xs, Ys).
brkcleanup(['\u002c' | Xs], ['\u002c', ' ' | Ys]) :- brkcleanup(Xs, Ys).
brkcleanup([X | Xs], [X | Ys]) :- brkcleanup(Xs, Ys).
brkcleanup(['\u005d', '\u007d'], ['\u007d']) :- !.
brkcleanup(['\u007d', '\u005d'], ['\u007d']) :- !.

% The predicate offers a modular and expandable 
% version of the built-in catch predicate 
% which may be needed for future expansions
errorhandling(Goal, _, Recover) :- catch(Goal, _, Recover). 

% The predicate checks that the number 'X' is greater than 
% or equal to zero. It will be used later for jsonaccess/3
domain(X) :- X >= 0.

% The predicate combines auxiliary predicates to clean up the JSON
% file before the jsondump/2 predicate sends it to the output stream
jsoncleanup(Term, JSONString) :-
    trmconversion(Term, String, NoCheck),
    strconversion(String, Chars),
    brkcleanup(Chars, AnalyzedChars),
    strconversion(JSONString, AnalyzedChars, NoCheck).

% The following predicate is used to 
% parse a JSON member (i.e. a key-value pair).
jsonmember(JSONMember, (AnalyzedProperty, AnalyzedValue)) :-
    lstconversion(JSONMember, ['\u003a', Property, Value]),
    jsonpair(Property, AnalyzedProperty, Value, AnalyzedValue),
    string(Property),           
    string(AnalyzedProperty).  

% The following predicate is auxiliary to jsonmember/2
jsonpair(SameProperty, SameProperty, PropertyValue, AnalyzedPropertyValue) :-
    jsonvalue(PropertyValue, AnalyzedPropertyValue).

% The predicate checks whether Value1 is a number or a string, and if not,
% it attempts to parse Value1 as a JSON value using the jsonparse/2 predicate
jsonvalue(Value1, Value1) :- number(Value1); string(Value1). 
jsonvalue(Value1, Value2) :- jsonparse(Value1, Value2).
jsonvalue([], []) :- !.

% The predicate parses a list of JSON objects. It therefore serves to 
% represent a JSON object. lstconversion/4 recursively splits the list into 
% individual objects. Given the variables, the 'X' represents the Input 
% Members, while the 'Y' represents Processed Members
jsonobj([Object], [Y | Ys]) :-
    lstconversion(Object, '\u002c', X, Xs),
    jsonmember(X, Y),
    jsonobj(Xs, Ys).

jsonobj([X | Xs], [Y | Ys]) :- jsonmember(X, Y), jsonobj(Xs, Ys).
jsonobj([], []) :- !.

% The predicate processes JSON arrays by calling the jsonvalue/2 predicate 
% on each value, and then, recursively, itself. Note that Value2 and 
% Elements2 are processed, while Value1 and Elements1 are not
jsonarray([], []) :- !.

jsonarray([Value1 | Elements1], [Value2 | Elements2]) :-
    is_list([Value1 | Elements1]),
    jsonvalue(Value1, Value2), 
    jsonarray(Elements1, Elements2).

% The predicate handles the open, write, and close processes, thereby
% effectively managing the program's I/O. Note that, according to the 
% documentation, SrcDest denotes the file name. Furthermore, the iomanager/4 
% version handles writing, while the iomanager/5 version handles reading
iomanager(SrcDest, Mode, Stream, Term) :-
    atom(SrcDest),
    errorhandling(open(SrcDest, Mode, Stream), _, false), 
    write(Stream, Term),
    close(Stream).

iomanager(SrcDest, Mode, Stream, Term, _) :-
    atom(SrcDest),
    errorhandling(open(SrcDest, Mode, Stream), Catcher, false), 
    read_string(Stream, _, String),
    atmconversion(Atom, String, _),
    errorhandling(jsonparse(Atom, Term), Catcher, false), 
    close(Stream).

% The predicate converts a JSON value into a string representation of 
% that same value by recursively processing the elements of the JSON 
% value. It is therefore an auxiliary predicate to jsondump/2
jsonconvert(jsonobj([]), {}) :- !.
jsonconvert([], []) :- !.

jsonconvert(jsonarray(Array), ArrayString) :- 
    jsonparse(ArrayString, jsonarray(Array)).

jsonconvert(jsonobj(Object), ObjectString) :-
    jsonconvert(Object, KVPairs),
    lstconversion(ObjectString, ['\u007b\u007d', KVPairs]).

jsonconvert(([(Key, jsonarray(Value)) | Objects]), [KVPair | KVPairs]) :-
    jsonparse(Array, jsonarray(Value)),
    lstconversion(KVPair, ['\u003a', Key, Array]),
    jsonconvert(Objects, KVPairs).

jsonconvert(([(Key, Value) | Objects]), [KVPair | KVPairs]) :-
    lstconversion(KVPair, ['\u003a', Key, Value]), 
    jsonconvert(Objects, KVPairs).

jsonconvert([], [AnalyzedKVPair]) :-
    lstconversion(JSONString, ['\u007b\u007d', AnalyzedKVPair]),
    jsonconvert([], JSONString).

% ---------------
% MAIN PREDICATES

% The predicate jsonparse(JSONString, Object) is true if JSONString, 
% i.e. an atom or Prolog string can be parsed as a string,
% number, or in the compound terms jsonobj and jsonarray
jsonparse(JSONString, Object) :-
    atmconversion(JSONString, String),
    jsonparse(String, Object),
    !.

jsonparse(JSONString, jsonobj(Object)) :-   
    lstconversion(JSONString, ['\u007b\u007d', X]),
    jsonobj([X], Object).

jsonparse(JSONString, jsonobj(Object)) :-
    strconversion(JSONString, Chars),
    qtscleanup(Chars, CleanChars),
    strconversion(CleanString, CleanChars, _),
    lstconversion(JSON, ['\u007b\u007d', X]),
    errorhandling(term_string(JSON, CleanString), catcher, false),
    jsonobj([X], Object),
    !.

jsonparse(JSONString, jsonarray(Object)) :-
    errorhandling(term_string(Array, JSONString), catcher, false), 
    strconversion(JSONString, _),
    jsonarray(Array, Object).

jsonparse({}, jsonobj([])) :- !. 
jsonparse([], jsonobj([])) :- !.
jsonparse(JSONString, jsonarray(Object)) :- jsonarray(JSONString, Object).

% The jsondump(JSON, FileName) predicate writes JSON to FileName. An 
% alternative version of the predicate is provided in case the first one 
% fails due to an Array (instead of an Object) being given as Input
jsondump(JSON, FileName) :-
    jsonconvert(JSON, Term),
    jsoncleanup(Term, JSONAtom),
    iomanager(FileName, write, _, JSONAtom).

jsondump(JSON, FileName) :-
    jsonconvert(JSON, JSONString),
    iomanager(FileName, write, _, JSONString).

% The jsonread(FileName, JSON) predicate opens FileName and succeeds if it can
% construct a JSON object. If FileName does not exist, the predicate fails
jsonread(FileName, JSON) :- iomanager(FileName, read, _, JSON, _).

% The predicate jsonaccess(Jsonobj, Fields, Result) is true when Result 
% is recoverable by following the chain of fields present in Fields (a 
% list) starting from Jsonobj. The field represented by 'N' (with 
% 'N' >= 0) corresponds to an index of a JSON array
jsonaccess(jsonobj(JSONObj), Fields, Result) :-
    is_list(JSONObj),
    string(Fields),
    member((Fields, Result), JSONObj),  
    !.

jsonaccess(jsonarray(JSONObj), [X | Xs], Result) :-
    is_list(JSONObj),
    jsonaccess(jsonarray(JSONObj), X, Y),
    jsonaccess(Y, Xs, Result), 
    !.

jsonaccess(jsonarray(JSONObj), N, Result) :-
    is_list(JSONObj),
    number(N),
    domain(N),
    nth0(N, JSONObj, Result).

jsonaccess(jsonobj(JSONObj), [X | Xs], Result) :-
    is_list(JSONObj),
    jsonaccess(jsonobj(JSONObj), X, Y),
    jsonaccess(Y, Xs, Result),  
    !.

jsonaccess(jsonobj(JSONObj), [], jsonobj(JSONObj)).
jsonaccess(JSONObj, [], JSONObj) :- JSONObj \= jsonarray(_).

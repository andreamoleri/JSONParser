% -*- Mode: Prolog -*-
% Moleri Andrea 902011

% AUXILIARY PREDICATES

% The predicate checks that a term is a String. 
% If it is, the String gets converted to single chars or vice versa. 
str_conversion(String, Chars) :- string(String), string_chars(String, Chars).
str_conversion(String, Chars, _) :- string_chars(String, Chars). 
str_conversion(String) :- string(String), string_chars(String).

% The predicate verifies that a term is an atom. 
% If it is, the atom is converted to a string or vice versa
atm_conversion(Atom, String) :- atom(Atom), atom_string(Atom, String).
atm_conversion(Atom, String, _) :- atom_string(Atom, String). 
atm_conversion(Atom) :- atom(Atom), atom_string(Atom).

% The predicate tests that a term is a term. 
% If it is, the term is converted to a string or vice versa
trm_conversion(Term, String) :- atom(Term), term_string(Term, String).
trm_conversion(Term, String, _) :- term_string(Term, String).
trm_conversion(Term) :- atom(Term), term_string(Term).

% The predicate converts a list whose first element is an atom, 
% provided the atom is non-numeric, into a term and vice versa. 
% Specifically, the lst_conversion/4 version is a forced execution of 
% the UNIV built-in operator on a list with a compound head,
% while the lst_conversion/3 version is a project-specific version
lst_conversion(Term, [Name | Args]) :-
    count_args(Args, Arity),
    functor(Term, Name, Arity), 
    fill_args(Term, Args).

lst_conversion(Term, Name1, Name2, Args) :- 
    error_handling(Term =.. [Name1, Name2 | Args], _, _).

% The following predicates are auxiliary to lst_conversion
count_args([], Num, Num) :- !.
count_args([_ | Xs], Num, Y) :- Z is Y + 1, count_args(Xs, Num, Z).
count_args(Args, Num) :- count_args(Args, Num, 0).

fill_args(Term, Args) :- fill_args(Term, Args, 1).
fill_args(_, [], _) :- !.
fill_args(Term, [Value | Xs], Arg) :- 
    arg(Arg, Term, Value), 
    N is Arg + 1, 
    fill_args(Term, Xs, N).

% Given a sequence of characters, the predicate is used to clean up 
% apostrophes, superscripts and quotation marks, all expressed with 
% the international character encoding standard UTF-16. The predicate
% also has the extra ability to clean up newlines, tabs, etc.
qts_cleanup(['\u0027\u0027' | Xs], ['\u0022' | Ys]) :- qts_cleanup(Xs, Ys).
qts_cleanup(['\u0027' | Xs], ['\u0022' | Ys]) :- qts_cleanup(Xs, Ys).
qts_cleanup(['\t' | Xs], ['\u0020' | Ys]) :- qts_cleanup(Xs, Ys).
qts_cleanup(['\n' | Xs], ['\u0020' | Ys]) :- qts_cleanup(Xs, Ys).
qts_cleanup(['\r' | Xs], ['\u0020' | Ys]) :- qts_cleanup(Xs, Ys).
qts_cleanup([X | Xs], [X | Ys]) :- qts_cleanup(Xs, Ys).
qts_cleanup([], []).

% Given a sequence of characters, the predicate is used to clean
% up parentheses, commas and add whitespaces, all expressed with
% the international standard of character encoding UTF-16
brk_cleanup(['\u007b', '\u005b' | Xs], ['\u007b' | Ys]) :- brk_cleanup(Xs, Ys).
brk_cleanup(['\u005b', '\u007b' | Xs], ['\u007b' | Ys]) :- brk_cleanup(Xs, Ys).
brk_cleanup(['\u002c' | Xs], ['\u002c', '\u0020' | Ys]) :- brk_cleanup(Xs, Ys).
brk_cleanup(['\u003a' | Xs], ['\u0020', '\u003a', '\u0020' | Ys]) :- 
    brk_cleanup(Xs, Ys).
brk_cleanup([X | Xs], [X | Ys]) :- brk_cleanup(Xs, Ys).
brk_cleanup(['\u005d', '\u007d'], ['\u007d']) :- !.
brk_cleanup(['\u007d', '\u005d'], ['\u007d']) :- !.

% The predicate combines auxiliary predicates to clean up the JSON
% file before the jsondump/2 predicate sends it to the output stream
dmp_cleanup(Input, Output) :-
    trm_conversion(Input, String, NoCheck),
    str_conversion(String, Chars, NoCheck),
    is_list(Chars),
    brk_cleanup(Chars, AnalyzedChars),
    is_list(AnalyzedChars),
    str_conversion(Output, AnalyzedChars, NoCheck),
    str_conversion(Output, _);
    fail.

% The predicate offers a modular and expandable 
% version of the built-in catch predicate 
% which may be needed for future expansions
error_handling(Goal, _, Recover) :- catch(Goal, _, Recover). 

% The predicate checks that the number 'X' is greater than 
% or equal to zero. It will be used later for jsonaccess/3
domain(X) :- X >= 0.

% The predicate handles the open, write, and close processes, thereby
% effectively managing the program's I/O. Note that, according to the 
% documentation, SrcDest denotes the file name. Furthermore, the io_manager/4 
% version handles writing, while the io_manager/5 version handles reading
io_manager(SrcDest, Mode, Stream, Term) :-
    error_handling(open(SrcDest, Mode, Stream), _, false),
    error_handling(write(Stream, Term), _, true),
    close(Stream).

io_manager(SrcDest, Mode, Stream, Term, _) :-
    error_handling(open(SrcDest, Mode, Stream), _, false),
    read_string(Stream, _, String),
    str_conversion(String, _),
    close(Stream),
    atm_conversion(Atom, String, _),
    str_conversion(String, _),
    jsonparse(Atom, Term).

% The predicate converts a JSON value into a string representation of 
% that same value or v.v. by recursively processing the elements of the 
% JSON value. It is therefore an auxiliary predicate to jsondump/2
stringify(jsonobj([]), "{}").
stringify(jsonarray([]), "[]").

stringify(jsonobj(Object), ObjectString) :-
    kvp_manager(Object, KVPs),
    format(string(ObjectString), "{~s}", [KVPs]).

stringify(jsonarray(Array), ArrayString) :-
    arr_manager(Array, Elements),
    format(string(ArrayString), "[~s]", [Elements]).

% The following predicate manages Key:Value 
% Pairs and is auxiliary to stringify/2
kvp_manager([], "").

kvp_manager([(K, V)], KVPs) :-
    format(string(KVP), "\"~s\" : \"~s\"", [K, V]), KVPs = KVP.

kvp_manager([(K, V) | Objects], KVPs) :-
    kvp_manager(Objects, Z),
    format(string(KVP), "\"~s\" : \"~s\",", [K, V]),
    atomic_list_concat([KVP, '\u0020', Z], '', KVPs).

% The following predicate manages Array 
% Elements and is auxiliary to stringify/2
arr_manager([], "").

arr_manager([E], Elements) :-
    format(string(ElementString), "~w", [E]),
    Elements = ElementString.

arr_manager([E | Es], Elements) :-
    arr_manager(Es, Z),
    format(string(ElementString), "~w,", [E]),
    atomic_list_concat([ElementString, '\u0020', Z], '', Elements).

% The following predicate is auxiliary to jsonkvpair/2
property_match(Twin, Twin, Value1, Value2) :- validate(Value1, Value2).

% The predicate validates whether Input1 is a number or a string, and if not,
% it attempts to parse Input1 as a jvalue using the jsonparse/2 predicate
validate(Input1, Input1) :- number(Input1); string(Input1). 
validate(Input1, Input2) :- jsonparse(Input1, Input2).

% JSON STRUCTURES

% The predicate parses a list of JSON objects. It therefore serves to 
% represent a JSON object. lst_conversion/4 recursively splits the list into 
% individual objects. Given the variables, the 'X' represents the Input 
% Members, while the 'Y' represents Processed Members
jsonobj([], []).
jsonobj([X | Xs], [Y | Ys]) :-
    (lst_conversion(X, '\u002c', Z, Zs), 
     jsonkv(Z, Y), 
     jsonobj(Zs, Ys); 
     jsonkv(X, Y), 
     jsonobj(Xs, Ys)).

% The following predicate is used to parse a JSON key-value 
% pair. If the process is unsuccessful, it is forced to 
% return 'false' using the appropriate command
jsonkv(JSONKVPair, (AnalyzedProperty, AnalyzedValue)) :-
    lst_conversion(JSONKVPair, ['\u003a', Property, Value]),
    str_conversion(Property, _), 
    property_match(Property, AnalyzedProperty, Value, AnalyzedValue),          
    str_conversion(AnalyzedProperty, _);
    fail.

% The predicate processes JSON arrays by calling the validate/2  
% predicate on each value, and then, recursively, itself. Note that 
% Xs and Ys are the processed versions of X and Y 
jsonarray([], []).
jsonarray([X | Xs], [Y | Ys]) :-
    (is_list([X | Xs]),
     validate(X, Y),
     jsonarray(Xs, Ys);
     validate(X, Y),
     jsonarray(Xs, Ys)).

% MAIN PREDICATES

% The predicate jsonparse(JSONString, Object) is true if JSONString, 
% i.e. an atom or Prolog string can be parsed as a string,
% number, or in the compound terms jsonobj and jsonarray
jsonparse(JSONString, Object) :-
    atm_conversion(JSONString, String),
    str_conversion(String, _),
    jsonparse(String, Object),
    !.

jsonparse(JSONString, jsonobj(Object)) :-
    ((lst_conversion(JSONString, ['\u007b\u007d', X]),
      jsonobj([X | []], Object), !; 
      str_conversion(JSONString, Chars),
      qts_cleanup(Chars, CleanChars),
      str_conversion(CleanString, CleanChars, _),
      lst_conversion(JSON, ['\u007b\u007d', X]),
      error_handling(term_string(JSON, CleanString), catcher, false),
      str_conversion(CleanString, _),
      jsonobj([X | []], Object), !); 
     (JSONString = {}; JSONString = []),
     Object = [], !).

jsonparse(JSONString, jsonarray(Object)) :- 
    jsonarray(JSONString, Object);
    error_handling(term_string(Array, JSONString), catcher, false),
    str_conversion(JSONString, _),
    jsonarray(Array, Object).

% The predicate jsonaccess(JSONObj, Fields, Result) is true when Result 
% is recoverable by following the chain of fields present in Fields (a 
% list) starting from JSONObj. The field represented by 'N' (with 
% 'N' >= 0) corresponds to an index of a JSON array. 'Field' 
% (sing.) is a special case that represents a Prolog String
jsonaccess(jsonarray(JSONObj), Fields, Result) :-
    Fields = N,
    is_list(JSONObj),
    (number(N),
     domain(N),
     nth0(N, JSONObj, Result); 
     is_list(Fields),
     [X | Xs] = Fields,
     jsonaccess(jsonarray(JSONObj), X, Z),
     jsonaccess(Z, Xs, Result),
     !; 
     fail).

jsonaccess(jsonobj(JSONObj), Fields, Result) :-
    is_list(JSONObj),
    (str_conversion(Fields, _),
     member((Fields, Result), JSONObj),
     !; 
     Fields = [],
     Result = jsonobj(JSONObj)).

jsonaccess(jsonobj(JSONObj), Fields, Result) :-
    is_list(JSONObj),
    [X | Xs] = Fields,
    jsonaccess(jsonobj(JSONObj), X, Z),
    jsonaccess(Z, Xs, Result),  
    !.

jsonaccess(JSONObj, [], JSONObj) :- 
    JSONObj \= jsonarray(_);
    fail.

% The jsonread(FileName, JSON) predicate opens FileName and succeeds if it can
% construct a JSON object. If FileName does not exist, the predicate fails
jsonread(FileName, JSON) :- io_manager(FileName, read, _, JSON, _).

% The jsondump(JSON, FileName) predicate writes JSON to FileName. An 
% alternative version of the predicate is provided via ; in case the first
% one fails due to an Array (instead of an Object) being given as Input
jsondump(JSON, FileName) :-
    (stringify(JSON, Term),
     dmp_cleanup(Term, JSONAtom),
     io_manager(FileName, write, _, JSONAtom);
     stringify(JSON, JSONString),
     io_manager(FileName, write, _, JSONString)).

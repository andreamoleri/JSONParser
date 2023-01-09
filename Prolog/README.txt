PROJECT INFO
----------------------------------------
Università degli Studi di Milano Bicocca
Linguaggi di Programmazione
Studente Moleri Andrea
Matricola 902011
A.A. 2022/2023
Made in Prolog

PROGRAM FEATURES
----------------------------------------
1. The program can parse JSON Strings
2. The program can read and write JSON files to disk
3. The program can build data structures that represent JSON objects
4. The program can retrieve user-requested values given a JSON object literal
5. The program can detect incorrect JSON Syntax, returning false whenever it happens

MAIN FUNCTIONS
----------------------------------------
JSONPARSE
The predicate jsonparse(JSONString, Object) is true if JSONString, 
i.e. an atom or Prolog string can be parsed as a string, number, or 
in the compound terms jsonobj and jsonarray. This predicate returns 
true if it can construct a data structure representing a JSON object

JSONACCESS
The predicate jsonaccess(JSONObj, Fields, Result) is true when 
Result is recoverable by following the chain of fields present in 
Fields (a list) starting from JSONObj. The field represented by 
'N' (with 'N' >= 0) corresponds to an index of a JSON array. 
'Field' (sing.) is a special case that represents a Prolog String

JSONREAD
The jsonread(FileName, JSON) predicate opens FileName and succeeds if it can 
construct a JSON object. If FileName does not exist, the predicate fails

JSONDUMP
The jsondump(JSON, FileName) predicate writes JSON to FileName. 
An alternative version of the predicate is provided in case the 
first one fails due to an Array (instead of an Object) being 
given as Input. This predicate returns true if it was possible 
to write JSON inside the file. Obviously this assumes that JSON 
has the correct syntax, otherwise the process fails

SAMPLE QUERIES
----------------------------------------

?- jsonparse(’[]’, X). 
X = jsonarray([]).

?- jsonparse(’{}’, X). 
X = jsonobj([]).

?- jsonparse(’[}’, X). 
false

?- jsonparse(’[1, 2, 3]’, A), jsonaccess(A, [3], E). 
false

?- jsondump(jsonobj([/* stuff */]), 'myfile.json').
true

?- jsondump(jsonobj([("name", "Aretha"), ("surname", "Aretha")]), 'myfile.json').
true

?- jsonread('myfile.json', JSON).
JSON = jsonobj([("name", "Aretha"), ("surname", "Franklin")]).

?- jsondump(jsonarray([1, 2, 3, 4, 5]), 'myfile.json').
true

?- jsonread('myfile.json', JSON).
JSON = jsonarray([1, 2, 3, 4, 5]).

?- jsonparse('{"name" : "Aretha", "surname" : "Franklin"}', O), jsonaccess(O, ["name"], R).
O = jsonobj([("name", "Aretha"), ("surname", "Franklin")]),
R = "Aretha".

?- jsonparse('{"coin" : "Nickel", "side" : ["Heads", "Tails"]}', Z), jsonaccess(Z, ["side", 1], R).
Z = jsonobj([("coin", "Nickel"), ("side", jsonarray(["Heads", "Tails"]))]),
R = "Tails".

?- jsonparse('{"name" : "Aretha", "surname" : "Franklin"}', JSONObj), jsonaccess(JSONObj, ["surname"], R).
JSONObj = jsonobj([("name", "Aretha"), ("surname", "Franklin")]),
R = "Franklin".








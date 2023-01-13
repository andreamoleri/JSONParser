PROJECT INFO
----------------------------------------
Università degli Studi di Milano Bicocca
Linguaggi di Programmazione
Studente Moleri Andrea
Matricola 902011
A.A. 2022/2023
Made in Lisp

PROGRAM FEATURES
----------------------------------------
1. The program can parse JSON Strings
2. The program can read and write JSON files to disk
3. The program can build data structures that represent JSON objects
4. The program can retrieve user-requested values given a JSON object literal
5. The program can detect incorrect JSON Syntax, returning errors whenever it happens

MAIN FUNCTIONS
----------------------------------------
JSONPARSE
The function takes a string as an argument and checks whether 
it is a string or a stream. In both cases it calls the json 
function, but if in the first it calls it with make-string-
input-stream as an argument, in the second it calls it 
without. Simply put, it builds a data structure that 
represents a JSON object. If the string does not comply with 
the JSON standards, an error will be reported

JSONACCESS
The function accepts a JSON object and a series of fields 
from which to retrieve the corresponding object. There is a 
field, represented by N (with N >= 0) which represents the 
index of a JSON array. The software reports an error in cases 
where the Index is too large or small, or if it is not 
possible to retrieve the requested object. From project 
specifications it is asked to create a field represented by N 
to represent an index of a JSON array. The field has been 
modularized and placed in the array-index-access function, 
and is used when find-jsonvalue is called within this function

JSONREAD
The jsonread function opens the file filename and returns a JSON 
object. If filename does not exist, the function throws an error

JSONDUMP
In case it is possible to carry out the writing process, the 
function returns an error. One of the cases where this can 
happen is when a syntactically incorrect JSONObject is provided

SAMPLE QUERIES 
----------------------------------------
PROMPT > (defparameter x (jsonparse "{\"name\" : \"Aretha\", \"surname\" : \"Franklin\"}"))
ANSWER > x

PROMPT > x
ANSWER > (JSONOBJ ("name" "Aretha") ("surname" "Franklin"))

PROMPT > (jsonaccess x "surname") 
ANSWER > "Franklin"

PROMPT > (jsondump x "/Users/username/myfile.json")
ANSWER > "/Users/username/myfile.json"

PROMPT > (jsonread "/Users/username/myfile.json")
ANSWER > (JSONOBJ ("name" "Aretha") ("surname" "Franklin"))

PROMPT > (jsonread "/Users/username/doesnotexist.json")
ANSWER > Error: the file named /Users/username/doesnotexist.json was not found

PROMPT > (jsonaccess (jsonparse "{\"coin\" : \"Nickel\", \"side\" : [[\"Heads\"], [\"Tails\"]]}") "side" 1 0)
ANSWER > "Tails"

PROMPT > (jsonparse "[1, 2, 3, 4, 5]") 
ANSWER > (JSONARRAY 1 2 3 4 5)

PROMPT > (jsonparse "{}") 
ANSWER > (JSONOBJ)

PROMPT > (jsonparse "[]") 
ANSWER > (JSONARRAY)

PROMPT > (jsonparse "{]") 
ANSWER > Error: syntax error

PROMPT > (jsonaccess (jsonparse " [1, 2, 3] ") 0)
ANSWER > 1

PROMPT > (jsonaccess (jsonparse " [1, 2, 3] ") 3) 
ANSWER > Error: Out of range - index too large

PROMPT > (jsonaccess (jsonparse " [1, 2, 3] ") -1)
ANSWER > Error: Out of range - index too small
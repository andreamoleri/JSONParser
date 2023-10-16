# JSONParser

## Overview

This project was developed as part of the "Linguaggi di Programmazione" course at the Università degli Studi di Milano Bicocca for the academic year 2022/2023. It is authored by Andrea Moleri, and is implemented in the Lisp programming language.

## Project Description

This program is a JSON parser and manipulator with the following features:

1. Parsing JSON Strings
2. Reading and writing JSON files to disk
3. Building data structures that represent JSON objects
4. Retrieving user-requested values from a JSON object
5. Detecting incorrect JSON syntax and reporting errors

## Main Functions

### JSONPARSE
The **'jsonparse'** function takes a string as an argument and checks whether it is a string or a stream. It then builds a data structure that represents a JSON object. If the string does not comply with JSON standards, an error is reported.

### JSONACCESS
The **'jsonaccess'** function accepts a JSON object and a series of fields to retrieve the corresponding object. It can retrieve values from JSON arrays using an index (N). The function reports an error if the index is out of range or if it is not possible to retrieve the requested object.

### JSONREAD
The **'jsonread'** function opens a JSON file and returns a JSON object. If the file does not exist, the function throws an error.

### JSONDUMP
The **'jsondump'** function writes a JSON object to a file. It returns an error if the object is not syntactically correct.

## Sample Queries

```lisp
; Parse a JSON string
(defparameter x (jsonparse "{\"name\" : \"Aretha\", \"surname\" : \"Franklin\"}"))

; Access a specific field
(jsonaccess x "surname") ; Returns: "Franklin"

; Read from a JSON file
(jsonread "/Users/username/myfile.json")

; Write to a JSON file
(jsondump x "/Users/username/myfile.json")

; Handle errors
(jsonread "/Users/username/doesnotexist.json")
(jsonaccess (jsonparse " [1, 2, 3] ") 3) ; Returns an error: "Out of range - index too large"
(jsonaccess (jsonparse " [1, 2, 3] ") -1) ; Returns an error: "Out of range - index too small"
```

## Example JSON Structures

### Parsing JSON strings:
```lisp
; Returns a JSON object
jsonparse "{\"name\" : \"Aretha\", \"surname\" : \"Franklin\"}"
```

### Parsing JSON arrays:
```lisp
; Returns a JSON array
jsonparse "[1, 2, 3, 4, 5]" 
```
### Handling syntax errors:
```lisp
; Returns a syntax error
jsonparse "{]" 
```

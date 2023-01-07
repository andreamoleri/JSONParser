; Matricola: 902011
; Studente:  Moleri Andrea

; -------------------
; AUXILIARY FUNCTIONS

; The following functions take any character as input and return T if that 
; character complies with the request made. For greater compatibility, the 
; UTF-16 standard has been used to represent characters
(defun is-opening-bracket (char) (char= char #\u005b))
(defun is-closing-bracket (char) (char= char #\u005d))
(defun is-opening-brace (char) (char= char #\u007b))
(defun is-closing-brace (char) (char= char #\u007d))
(defun is-opening-quote (char) (char= char #\u0022))
(defun is-comma (char) (char= char #\u002c))
(defun is-colon (char) (char= char #\u003a))
(defun is-space (char) (char= char #\u0020))
(defun is-minus (char) (char= char #\u002d))
(defun is-plus (char) (char= char #\u002b))
(defun is-dot (char) (char= char #\u002e))

; The following functions are the logical 'or' version of the 
; previous functions. They will be useful for saving lines of 
; code later, as they will be used often, i.e. in readnumber
(defun openingp (char)
  (or (is-opening-brace char)
      (is-opening-quote char)
      (is-opening-bracket char)))

(defun closingp (char)
  (or (is-closing-brace char) 
      (is-closing-bracket char)))

(defun separationp (char)
  (or (closingp char) 
      (is-space char) 
      (is-comma char)))

; The function, auxiliary to jsonvalue, checks if the input list has an 
; occurrency of an arithmetic value (or symbol). If it does, it returns 
; 'true'. In a nutshell, it's a condition checking that expands the built-
; in 'typep' function. The values it checks for are 0...9, +, -, *, /
(defun arithmeticp (input)
  (member input '(#\u0030 #\u0031 #\u0032 #\u0033 #\u0034 #\u0035 #\u0036
                  #\u0037 #\u0038 #\u0039 #\u002b #\u002d #\u002a #\u002f)))

; The function is auxiliary to spccleanup and, as can be understood from 
; the -p suffix, it is used to understand if the 'character' character given 
; as input is a space, matching it with the ASCII representation of the most 
; common whitespace characters that can be found at https://ascii.cl
(defun spcp (character)
  (let ((code (char-code character)))
    (and (<= code char-code-limit)
         (member code '(9 10 13 32))))) 

; Given an Input, the function cleans its whitespaces 
; by checking them with the spcp function
(defun spccleanup (input)
  (let ((character (read-char input T)))
    (if (spcp character)
        (spccleanup input)
        (let ((unread-char-result (unread-char character input)))
          input))))

; The following functions modularize casting 
; processes that will be used a lot in the code
(defun stringcasting (input) (coerce input 'string))
(defun numbercasting (input) (coerce input 'number))

; The following functions, with the exception of outputkvp, take 
; an input and an output as arguments. The input is formatted 
; using the Format function according to the request expressed by 
; the function name (for example outputchr transforms the input 
; into a string of characters), and is then written to the output.
(defun outputchr (input output) (format output "~C" input))
(defun outputstr (input output) (format output "~S" input))
(defun outputint (input output) (format output "~D" input))
(defun outputflt (input output) (format output "~F" input))
(defun outputkvp (key value) (format NIL "~S : ~A" key value)) 
(defun outputarr (input output) (format output "[~{~A~^, ~}]" input))
(defun outputobj (input output) (format output "{~{~A~^, ~}}" input))

; The function receives a sequence of characters and 
; tries to convert it to a string. If the conversion was 
; successful, then the function returns the string
(defun jsonstring (input)
  (let ((result (stringcasting (charactercollection input))))
    (cond ((typep result 'string) result)
          (T (error "Unable to cast characters")))))

; The function parses the Key:Value pairs from a jsonstring
(defun jsonmember (jsonstring)
  (setq nextcharacter (read-char (spccleanup jsonstring) T))
  (if (is-opening-quote nextcharacter)
      (cons (jsonpair jsonstring) (createlist jsonstring))
      (error "The jsonmember function is unable to parse 
      Key:Value pairs from the given jsonstring")))

; The function takes a stream of characters as input and tries to convert 
; it into a valid JSON value. The function reads the first character of 
; the input and performs a series of checks on this first character, thus 
; differentiating the cases of jsonstring, jsonnumber and json
(defun jsonvalue (input)
  (let ((nextcharacter (read-char input T)))
    (cond
     ((is-opening-quote nextcharacter) (jsonstring input))
     ((openingp nextcharacter) 
      (unread-char nextcharacter input) (json input))
     ((arithmeticp nextcharacter) 
      (unread-char nextcharacter input) (jsonnumber input))
     (T (error "Unable to execute value conversion")))))

; The function takes a series of characters as 
; input and tries to extract a valid key-value pair
(defun jsonpair (input)
  (let ((key (jsonstring input)))
    (when (is-colon (read-char (spccleanup input) T))
      (list key (jsonvalue (spccleanup input))))))

; Auxiliary to jsonnumber. The function could also 
; be written without using Lambda Expressions but I 
; wanted to try using them for practice
(defun readnumber (input)
  ((lambda (symbol) (cond ((separationp symbol)
    (unread-char symbol input))
      ((cons symbol (readnumber input)))))
    (read-char input T)))

; The function takes an input and tries to find a number 
; in it. If the number is found, it is converted to an 
; int or float value depending on its type
(defun jsonnumber (input)
  (let ((numberstring (readnumber input)))
  (cond ((find #\u002e numberstring)
         (parse-float (stringcasting numberstring)))
        (T (parse-integer (stringcasting numberstring))))))

; The function reads a single character from the stream and 
; checks whether it is a comma or a closing brace. If it's a 
; comma, the function calls itself recursively to parse the key-
; value pair. If it is a closing brace, the function returns NIL. 
; The function is auxiliary to jsonmember and the two are used 
; for parsing the JSON object and its Key:Value pairs
(defun createlist (input)
  (let ((nextcharacter (read-char (spccleanup input) T)))
    (cond ((is-comma nextcharacter) (jsonmember (spccleanup input)))
          ((is-closing-brace nextcharacter) NIL)
          (T (error "Unable to create a list from the given input")))))

; This function, auxiliary to jsonstring, reads characters from 
; an input stream until the character " is encountered, expressed with 
; the UTF-16 standard. When the character is encountered, the function
; returns the list of accumulated characters, as per 'Common Lisp 
; the Language, 2nd Edition', 26.8. - Value Accumulation
(defun charactercollection (input)
  (loop for character = (read-char input T)
        while (not (is-opening-quote character))
        collect character))

; The following functions are auxiliary to jsonobj, in 
; particular to handle cases where the parser encounters 
; a quote or a closing key inside a JSON object
(defun closingbracehandling () (list 'jsonobj))
(defun openingquotehandling (jsonstring char-obj)
  (unread-char char-obj jsonstring)
  (append '(jsonobj) (jsonmember jsonstring)))

; The following code reports an error in the jsonaccess 
; procedure. It has been modularized because otherwise it 
; would have been repeated multiple times within the function
(defun jsonaccesserror () (error "The jsonaccess procedure failed"))

; The function takes a stream of characters as 
; input and tries to extract a JSON object from it
(defun jsonobj (input) 
  (setq nextcharacter (read-char (spccleanup input) T))
  (if (is-closing-brace nextcharacter) 
  (closingbracehandling)
  (if (is-opening-quote nextcharacter) 
  (openingquotehandling input nextcharacter)
      (error "syntax error"))))

; The function takes a jsonobject and a key as input, and 
; searches for the key in the object. If the key is indeed 
; found, the function returns the value associated with the key
(defun findobjectvalue (JSONObject key)
  (loop for i in JSONObject
        do (cond
             ((equal (car i) key) (return (getvalue i)))
             (T NIL))
        finally (error "Key '~A' was not found in the JSON object" key)))

; Auxiliary to findobjectvalue
(defun getvalue (kvpair) (car (cdr kvpair)))

; The function takes as input a JSON object represented as a list 
; and a sequence of indexes called 'sequence' and returns the 
; value within the JSON object to which the 'sequence' refers to
(defun findjsonvalue (json sequence)
  (if (typep (car sequence) 'integer)
      (if (null (cdr sequence))
          (arrayindexaccess (cdr json) (car sequence))
          (findjsonvalue (arrayindexaccess 
            (cdr json) (car sequence)) (cdr sequence)))
      (if (null (cdr sequence))
          (findobjectvalue (cdr json) (car sequence))
          (findjsonvalue (findobjectvalue 
            (cdr json) (car sequence)) (cdr sequence)))))

; The function takes a stream of characters as 
; input and tries to extract a JSON array from it
(defun jsonarray (input)
  (cond 
    ((or (typep input 'character) (typep input 'number)) 
     (error "Input type is invalid"))
    (T
     ((lambda (nextcharacter)
      (cond ((is-closing-bracket nextcharacter) (quote (jsonarray)))
            (T
             (unread-char nextcharacter input)
             (cons 'jsonarray (buildcomponentslist input)))))
             (read-char (spccleanup input) T)))))

; Auxiliary to jsonarray
(defun buildcomponentslist (input)
  (append (list (jsonvalue input))
    ((lambda (nextcharacter)
      (if (is-closing-bracket nextcharacter) NIL
      (if (is-comma nextcharacter) (buildcomponentslist (spccleanup input))
    (error "Invalid character found, unable to create JSON element list"))))  
    (read-char (spccleanup input)))))

; The function takes as input an array and an integer N (with 
; N >= 0) and returns the element of the array at position N
(defun arrayindexaccess (array N)
  (cond ((< N (length array))
         (cond ((>= N 0)
                (loop for i from 0 for value in array
                      when (= i N)
                      return value))
               (t (error "Out of Range: index too small"))))
        (t (error "Out of Range: index too large"))))

; The function accepts a jsonstring and, in addition to stripping 
; any whitespace characters, it reads the first character from the 
; given jsonstring. The two possible cases are then distinguished: 
; curly bracket and square bracket, and on the basis of these the 
; jsonobj or jsonarray functions are called
(defun json (jsonstring)
  (let ((char (read-char (spccleanup jsonstring) T)))
    (cond
     ((is-opening-brace char) (jsonobj jsonstring))
     ((is-opening-bracket char) (jsonarray jsonstring))
     (T NIL))))

; The function takes an input and an output and writes the input value 
; to the output stream in JSON format after performing a series of type 
; checks with the help of the outputstructure function
(defun writevalue (input output)
  (cond
   ((typep input 'number) (cond 
                    ((typep input 'float) (outputflt input output))
                    ((typep input 'integer) (outputstr input output))
                    (T (error "error write number to file"))))
   ((typep input 'string) (outputstr input output))
   ((and (typep input 'list) (equal (car input) 'jsonarray)) 
    (outputstructure input output))
   ((and (typep input 'list) (equal (car input) 'jsonobj)) 
    (outputstructure input output))
   (T (error "Cannot write value:~@ 
   the input passed is not a number, string, array, or object"))))

; Auxiliary to writevalue
(defun outputstructure (input output)
  (cond
   ((and (typep input 'list) (equal (car input) 'jsonarray))
    (outputarr (mapcar 
      (lambda (x) (writevalue x NIL)) (cdr input)) output))
   ((and (typep input 'list) (equal (car input) 'jsonobj))
    (outputobj (mapcar 
      (lambda (y) (outputkvp (car y) (writevalue (second y) NIL))) 
        (cdr input)) output))
   (T (error "Cannot write structure:~@
   the input passed is not an array or object"))))

; --------------
; MAIN FUNCTIONS

; The function takes a string as an argument and checks if it is a 
; string or a stream. In both cases, it calls the json function, but 
; if in the former it calls it with make-string-input-stream as an 
; argument, in the latter it is called without. If string does not 
; fit both cases, the software reports an error
(defun jsonparse (JSONString)
  (if (typep JSONString 'stream) (json JSONString)
      (if (typep JSONString 'string)
          (with-input-from-string (input JSONString) (json input))
          (error "Invalid input in jsonparse function: 
          The error occurs because the input parameter 
          is neither a stream nor a string"))))

; The function accepts a JSON object and a series of fields from 
; which to retrieve the corresponding object. From project 
; specifications it is asked to create a field represented by N
; (with N >= 0) to represent an index of a JSON array. The field has 
; been modularized and placed in the arrayindexaccess function, and 
; is used when findjsonvalue is called within this function
(defun jsonaccess (JSONObject &rest fields)
  (cond ((not (listp JSONObject)) (jsonaccesserror))
        ((not fields) (jsonaccesserror))
        ((null JSONObject) (jsonaccesserror))
        (T (findjsonvalue JSONObject fields))))

; The jsonread function opens the file filename and returns a JSON 
; object. If filename does not exist, the function throws an error. For 
; the implementation of Exception Handling I based the code on what I 
; found in The Common Lisp Cookbook in the Error Handling section
; https://lispcookbook.github.io/cl-cookbook/error_handling
(defun jsonread (filename)
  (handler-case
      (let ((stream (open filename :direction :input
                                   :if-does-not-exist :error)))
            (jsonparse stream)
          (close stream)))
    (error ()
     (error (format NIL "The reading process could not be completed:~@
     the file named ~a was not found" filename)))))

; The jsondump function writes the JSON object to file 
; filename in JSON syntax. If filename does not exist, it is 
; created and if it does it is overwritten. Again, for the 
; implementation of Exception Handling best practices, I 
; based the code on what I found in The Common Lisp Cookbook
(defun jsondump (JSON filename)
  (handler-case
      (let ((stream (open filename :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)))
        (unwind-protect
            (outputstructure JSON stream)
          (close stream)))
    (error ()
     (error (format NIL "Writing process failed:~@
     the file ~a could not be opened or created" filename)))) filename)

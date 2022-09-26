(require 'clojure.set)
(ns clojure.set)

(defn makeCompressDict []
  (let [content (range 0 256)]
    (zipmap (map (comp #'list #'char) content) content)))

(defn makeDecompressDict []
  (map-invert (makeCompressDict)))

(defn compress [text] ; Take arbitrary text as input
  (loop [t (seq text) ; Loop through input sequence char by char
    r '() ; Current character
    w '() ; Next character
    dict (makeCompressDict) ; Dictionary of ASCII chars (0-255)
    s 256] ; Index
  (let [c (first t)] ; Set c to first character of the sequence of chars
    (if c ; If c (first char) exists
      (let [wc (cons c w)] ; Set wc to combination of current character and next character
        (if (get dict wc) ; If dictionary has the current combination
          (recur (rest t) r wc dict s) ; Call this function recursively to complete the buffer
          (recur (rest t) (cons (get dict w) r) (list c) (assoc dict wc s) (inc s)))) ; Else continue looping and add the current char to the dictionary
        (reverse (if w (cons (get dict w) r) r)))))) ; If first character doesnt exist reverse

(defn decompress [bytes] ; Takes a list of bytes as input
  (let [f (char (first bytes))] ; Used as first character in output
    (loop [word (rest (seq bytes)) ; Contains content of bytes parameter without first value
            previousCharacter () ; Used as buffer to keep track of previous character in function
            dict (makeDecompressDict) ; Dictionary that contains all ASCII characters
            index 256 ; Index for new values in dictionary
            out ()] ; Output of function
      (let [currentCharacter (first word)] ; Current character that contains first value of word variable
        (if currentCharacter ; Checks if currentCharacter is not nil
          (if (contains? dict currentCharacter) ; Check if currentCharacter is already in dict
            (recur (rest word) (apply str (get dict currentCharacter 0)) ; If in dictionary, recursively call function with currentCharacter
                    (assoc dict index (str previousCharacter (first (apply str (get dict currentCharacter 0)))))
                    (inc index) (cons (apply str (get dict currentCharacter 0)) out))
            nil) ; If not in dictionary, return nil
          (apply str f (reverse out))))))) ; If currentCharacter is nil, outputs firstCharacterOutput variable with the reversed output of the out variable as a string, both in combination contain the complete sentence

; (print (makeDecompressDict))

; (defn decompress [#^String text]
;   ()

(print (decompress (compress "TOBEORNOTTOBEORTOBEORNOT")))
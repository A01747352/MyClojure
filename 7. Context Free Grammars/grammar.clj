;----------------------------------------------------------
; Problem Set #8: Context-Free Grammars
; Date: May 30, 2025.
; Authors:
;          A01747352 Diego Carreón Aguirre
;          A01800593 Emilio De León Vives
;----------------------------------------------------------
(ns grammar
    (:require [clojure.test :refer [deftest is run-tests]])
    (:require [instaparse.core :refer [parser]])
    (:import (instaparse.gll Failure)))

(defn fails? [r] (instance? Failure r))
(defn succeeds? [r] (not (fails? r)))

;----------------------------------------------------------
;;Ejemplo
;(def example (parser "
;    A = 'a' A 'b' (* un comentario *)
;  | epsilon
;"))
;
;(succeeds? (example "aaabbb"))

;----------------------------------------------------------
;Problem 1
(def start-and-end (parser "
  S = '#'
    | '$'
    | '#' T '#'
    | '$' T '$'
  T = '#' T
    | '$' T
    | epsilon
"))

(deftest test-start-and-end
         (is (succeeds? (start-and-end "$")))
         (is (succeeds? (start-and-end "#")))
         (is (succeeds? (start-and-end "$$")))
         (is (succeeds? (start-and-end "##")))
         (is (succeeds? (start-and-end "$$$$$$$#$$#$$#$##$")))
         (is (succeeds? (start-and-end "#$$$#$$$$#$$$$#$####")))
         (is (fails? (start-and-end "")))
         (is (fails? (start-and-end "$#")))
         (is (fails? (start-and-end "#$")))
         (is (fails? (start-and-end "###$$#$#$$$#$$$####$")))
         (is (fails? (start-and-end "$#$#$#$$$$#$$$#$$$$#$$#")))
         (is (fails? (start-and-end "#######################$"))))

;----------------------------------------------------------
;Problem 2
(def palindrome (parser "
  P = epsilon
  | '0'
  | '1'
  | '0' P '0'
  | '1' P '1'

"))

(deftest test-palindrome
         (is (succeeds? (palindrome "")))
         (is (succeeds? (palindrome "0")))
         (is (succeeds? (palindrome "1")))
         (is (succeeds? (palindrome "11")))
         (is (succeeds? (palindrome "00")))
         (is (succeeds? (palindrome "010")))
         (is (succeeds? (palindrome "1111111")))
         (is (succeeds? (palindrome "000010000")))
         (is (succeeds? (palindrome "01001110101110010")))
         (is (fails? (palindrome "01")))
         (is (fails? (palindrome "10")))
         (is (fails? (palindrome "1010")))
         (is (fails? (palindrome "10000000")))
         (is (fails? (palindrome "00010001")))
         (is (fails? (palindrome "1010011010")))
         (is (fails? (palindrome "111111111111111111110"))))

;----------------------------------------------------------
;Problem 3
(def balanced-parentheses (parser "
  P = epsilon
  |   '(' P ')' P
"))

(deftest test-balanced-parentheses
         (is (succeeds? (balanced-parentheses "")))
         (is (succeeds? (balanced-parentheses "()")))
         (is (succeeds? (balanced-parentheses "((((()))))")))
         (is (succeeds? (balanced-parentheses "()()()()()")))
         (is (succeeds? (balanced-parentheses
                          "(()()())((())(()()()))(((())))")))
         (is (fails? (balanced-parentheses "(")))
         (is (fails? (balanced-parentheses ")")))
         (is (fails? (balanced-parentheses "((((())))")))
         (is (fails? (balanced-parentheses "))))((((")))
         (is (fails? (balanced-parentheses
                       "(()()())((())(()()()))((((())))"))))

;----------------------------------------------------------
;Problem 4
(def o-in-the-middle (parser "
  S = 'o'
  |   R S R
  R = 'x'
  |   'o'
"))

(deftest test-o-in-the-middle
         (is (succeeds? (o-in-the-middle "o")))
         (is (succeeds? (o-in-the-middle "xox")))
         (is (succeeds? (o-in-the-middle "oooooxxxx")))
         (is (succeeds? (o-in-the-middle "oxxoooxooxoxoxx")))
         (is (succeeds? (o-in-the-middle "ooooooooooooooo")))
         (is (succeeds? (o-in-the-middle "xxxxxxxxxxoxxxxxxxxxx")))
         (is (fails? (o-in-the-middle "")))
         (is (fails? (o-in-the-middle "ox")))
         (is (fails? (o-in-the-middle "oxo")))
         (is (fails? (o-in-the-middle "oxxoooxxoxoxoxx")))
         (is (fails? (o-in-the-middle "xxxxxxxxxxxxxxxxxxxxx")))
         (is (fails? (o-in-the-middle "oooooooooooooooooooooo"))))

;----------------------------------------------------------
;Problem 5
(def twice (parser "
  S = 'x' 'x' 'y'
  |   'x' 'x' S 'y'
"))

(deftest test-twice
         (is (succeeds? (twice "xxy")))
         (is (succeeds? (twice "xxxxyy")))
         (is (succeeds? (twice "xxxxxxxxyyyy")))
         (is (succeeds? (twice "xxxxxxxxxxxxxxxxxxxxyyyyyyyyyy")))
         (is (fails? (twice "")))
         (is (fails? (twice "xy")))
         (is (fails? (twice "yxx")))
         (is (fails? (twice "xyy")))
         (is (fails? (twice "xxxyyy")))
         (is (fails? (twice "xyyxyy")))
         (is (fails? (twice "xxxxyyx")))
         (is (fails? (twice "yyyxxxxxx")))
         (is (fails? (twice "xxxxxxxxxxyy")))
         (is (fails? (twice "xyxyxyxyxxxx")))
         (is (fails? (twice "xxxxxxxxxxyyyy"))))

;----------------------------------------------------------
;Problem 6
(def a-plus-b-equals-c (parser "
  P = epsilon
  |   'a' P 'c'
  |   'b' P 'c'
"))

(deftest test-a-plus-b-equals-c
         (is (succeeds? (a-plus-b-equals-c "")))
         (is (succeeds? (a-plus-b-equals-c "abcc")))
         (is (succeeds? (a-plus-b-equals-c "ac")))
         (is (succeeds? (a-plus-b-equals-c "bc")))
         (is (succeeds? (a-plus-b-equals-c "aaaaabbbcccccccc")))
         (is (succeeds? (a-plus-b-equals-c "aaaaaaaaabcccccccccc")))
         (is (succeeds? (a-plus-b-equals-c
                          "bbbbbbbbbbbbbccccccccccccc")))
         (is (succeeds? (a-plus-b-equals-c
                          "abbbbbbbbbbbbccccccccccccc")))
         (is (succeeds? (a-plus-b-equals-c
                          "aaaabbbbbbbbbbbccccccccccccccc")))
         (is (fails? (a-plus-b-equals-c "a")))
         (is (fails? (a-plus-b-equals-c "b")))
         (is (fails? (a-plus-b-equals-c "c")))
         (is (fails? (a-plus-b-equals-c "ab")))
         (is (fails? (a-plus-b-equals-c "abc")))
         (is (fails? (a-plus-b-equals-c "abbcc")))
         (is (fails? (a-plus-b-equals-c "cccccc")))
         (is (fails? (a-plus-b-equals-c "ccccaabb")))
         (is (fails? (a-plus-b-equals-c "aaaaaaaa")))
         (is (fails? (a-plus-b-equals-c "bbbbbbbbb")))
         (is (fails? (a-plus-b-equals-c "aaaaaabbbbbb")))
         (is (fails? (a-plus-b-equals-c "aabbcccccccccccccccccc"))))

;----------------------------------------------------------

(run-tests)
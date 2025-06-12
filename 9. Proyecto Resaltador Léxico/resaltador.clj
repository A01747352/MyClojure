;----------------------------------------------------------
; Proyecto: Resaltador de léxico de BASIC
; Fecha: 11 de junio, 2025.
; Autores:
;          A01747352 Diego Carreón Aguirre
;          A01800593 Emilio De León Vives
;----------------------------------------------------------
(ns resaltador
  (:require [clojure.string :as str])
  (:import [java.io FileWriter]))

(def palabras-reservadas
  #{"PRINT" "INPUT" "IF" "THEN" "GOTO"
    "GOSUB" "RETURN" "FOR" "TO" "STEP"
    "NEXT" "DIM" "REM" "END" "LET" "READ"
    "DATA" "RESTORE" "DEF" "STOP" "ON"
    "WAIT" "CALL" "CLS"})

(def funciones
  #{"ABS" "ASC" "ATN" "COS" "EXP"
    "INT" "LOG" "RND" "SGN" "SIN"
    "SQR" "TAN" "CHR$" "LEFT$" "RIGHT$"
    "MID$" "LEN" "VAL" "STR$" "TAB"})

(def simbolos
  {"(" :parIzq, ")" :parDer, ":" :dosPuntos, "," :coma, ";" :puntoComa,
   "+" :suma, "-" :resta, "*" :multi, "/" :div, "^" :potencia,
   "=" :igual, "<>" :diferente, "<=" :menorIgual, ">=" :mayorIgual,
   "<" :menor, ">" :mayor})

(defn conflicto-html [text]
  (-> text
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

(def token-regex
  (re-pattern
    (str "(?x)
      (\\s+)                                              # Espacios
      |(REM .*?$)                                         # Comentarios
      |(\"[^\"]*\")                                       # Cadenas
      |(-?(?:\\d+(?:\\.\\d+)?|\\.\\d+)(?:[eE][+-]?\\d+)?)|([a-zA-Z][a-zA-Z0-9]*\\$?)  # Números
      |\\b(" (str/join "|" palabras-reservadas) ")\\b     # Palabras clave
      |\\b[A-Z]{1,2}\\d?\\$?\\b                           # Variables
      |\\b(" (str/join "|" funciones) ")\\b               # Funciones
      |(<>|<=|>=|[=<>:+\\-*/^(),;])                       # Símbolos
      |(\\.)                                              # Punto solo (inválido)
      |([^\\s])                                           # Inválido
    ")))

(defn clasifica-token [token]
  (cond
    (re-matches #"^\s+$" token) [:espacio token]
    (re-matches #"^REM\s+.*" token) [:comentario token]
    (= token "REM") [:reservada token]
    (re-matches #"^\"[^\"]*\"$" token) [:cadena token]
    (re-matches #"^-?(?:\d+(?:\.\d+)?|\.\d+)(?:[eE][+-]?\d+)?$" token) [:numero token]
    (palabras-reservadas token) [:reservada token]
    (re-matches #"^[A-Z]{1,2}\d?\$?$" token) [:variable token]
    (funciones token) [:funcion token]
    (= token ".") [:invalido token]
    (simbolos token) [(get simbolos token) token]
    :else [:invalido token]))

(defn tokeniza-linea [linea]
  (let [tokens (re-seq token-regex linea)]
    (mapcat
      (fn [grupos]
        (let [token (first (remove nil? grupos))]
          (if (and (str/starts-with? token "REM")
                   (not= token "REM"))
            [[:reservada "REM"]
             [:comentario (subs token 3)]]
            [(clasifica-token token)])))
      tokens)))

(defn token->html [[tipo token]]
  (if (= tipo :espacio)
    token
    (str "<span class=\"" (name tipo) "\">" (conflicto-html token) "</span>")))

(defn procesar-linea [linea]
  (let [tokens (tokeniza-linea linea)
        html (apply str (map token->html tokens))]
    html))

(defn basic->html [archivo]
  (let [lineas (str/split-lines (slurp archivo))
        contenido-html (->> lineas
                            (map procesar-linea)
                            (str/join "\n"))
        salida (str (str/replace archivo #"\.bas$" "") ".html")
        html (str "<!DOCTYPE html>
                  <html>
                    <head>
                      <style>
                        body { background: #1e1e1e; color: white; font-family: monospace; }
                        span.numero { color: cyan; }
                        span.cadena { color: gold; }
                        span.comentario { color: gray; font-style: italic; }
                        span.variable { color: lightgreen; }
                        span.reservada { color: deepskyblue; font-weight: bold; }
                        span.funcion { color: violet; }
                        span.invalido { color: red; background: black; }
                        span.parIzq, span.parDer, span.dosPuntos, span.coma, span.puntoComa,
                        span.suma, span.resta, span.multi, span.div, span.potencia,
                        span.igual, span.diferente, span.menor, span.menorIgual, span.mayor, span.mayorIgual { color: orange; }
                        </style>
                      </head>
                      <body>
                      <pre>" contenido-html "</pre>
                   </body>
                 </html>")]
    (spit salida html)
    (println "Archivo generado:" salida)))

(basic->html "C:\\Users\\carre\\IdeaProjects\\MyClojure\\9. Proyecto Resaltador Léxico\\sinewave.bas")
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

;; Tabla de palabras reservadas (statements) y funciones (functions) de BASIC
(def palabras-reservadas
  #{"PRINT" "INPUT" "IF" "THEN" "GOTO" "GOSUB" "RETURN" "FOR" "TO" "STEP" "NEXT" "DIM" "REM" "END" "LET" "READ" "DATA" "RESTORE" "DEF" "STOP" "ON" "WAIT" "CALL" "CLS"})

(def funciones
  #{"ABS" "ASC" "ATN" "COS" "EXP" "INT" "LOG" "RND" "SGN" "SIN" "SQR" "TAN" "CHR$" "LEFT$" "RIGHT$" "MID$" "LEN" "VAL" "STR$" "TAB"})

(def simbolos
  {"(" :par-izq, ")" :par-der, ":" :dos-puntos, "," :coma, ";" :punto-coma,
   "+" :suma, "-" :resta, "*" :multi, "/" :div, "^" :potencia,
   "=" :igual, "<>" :diferente, "<=" :menor-igual, ">=" :mayor-igual,
   "<" :menor, ">" :mayor})

;; Escapar caracteres HTML
(defn escape-html [text]
  (-> text
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

;; Tokenizer regex con agrupación
(def token-regex
  (re-pattern
    (str
      "(?x)
      (REM .*?$)                               # Comentarios
      |(\"[^\"]*\")                            # Cadenas
      |\\b\\d+(\\.\\d+)?([eE][-+]?\\d+)?\\b    # Números (int, float, científico)
      |\\b[A-Z]{1,2}\\d?\\$?\\b                # Variables
      |\\b(" (str/join "|" palabras-reservadas) ")\\b # Palabras clave
      |\\b(" (str/join "|" funciones) ")\\b    # Funciones
      |(<>|<=|>=|[=<>:+\\-*/^(),;])            # Símbolos
      |([^\\s])                                # Token inválido
    ")))

;; Clasifica el token con base en coincidencia
(defn clasifica-token [token]
  (cond
    (re-matches #"^REM .*" token) [:comentario token]
    (re-matches #"^\"[^\"]*\"$" token) [:cadena token]
    (re-matches #"^\d+(\.\d+)?([eE][-+]?\d+)?$" token) [:numero token]
    (re-matches #"^[A-Z]{1,2}\d?\$?$" token) [:variable token]
    (palabras-reservadas token) [:reservada token]
    (funciones token) [:funcion token]
    (simbolos token) [(get simbolos token) token]
    :else [:invalido token]))

;; Tokeniza una línea a pares de [tipo token]
(defn tokeniza-linea [linea]
  (->> (re-seq token-regex linea)
       (map #(first (remove nil? %))) ; limpiamos nils
       (map clasifica-token)))

;; Formatea un token con span y clase CSS
(defn token->html [[tipo token]]
  (str "<span class=\"" (name tipo) "\">" (escape-html token) "</span>"))

;; Procesa cada línea del archivo BASIC
(defn procesar-linea [linea]
  (let [tokens (tokeniza-linea linea)
        html (apply str (map token->html tokens))]
    html))

;; Función principal que convierte archivo BASIC a HTML
(defn basic->html
  "Convierte un archivo BASIC a HTML resaltado léxicamente."
  [archivo]
  (let [lineas (str/split-lines (slurp archivo))
        contenido-html (->> lineas
                            (map procesar-linea)
                            (str/join "\n"))
        salida (str (str/replace archivo #"\.bas$" "") ".html")
        html (str "<!DOCTYPE html>
                  <html>
                    <head>
                      <meta charset=\"UTF-8\">
                      <style>
                        body { background: #1e1e1e; color: white; font-family: monospace; }
                        span.numero { color: cyan; }
                        span.cadena { color: gold; }
                        span.comentario { color: gray; font-style: italic; }
                        span.variable { color: lightgreen; }
                        span.reservada { color: deepskyblue; font-weight: bold; }
                        span.funcion { color: violet; }
                        span.invalido { color: red; background: black; }
                        span.par-izq, span.par-der, span.dos-puntos, span.coma, span.punto-coma,
                        span.suma, span.resta, span.multi, span.div, span.potencia,
                        span.igual, span.diferente, span.menor, span.menor-igual, span.mayor, span.mayor-igual {
                          color: orange;
                        }
                        </style>
                      </head>
                      <body>
                      <pre>" contenido-html
                     "</pre>
                      </body>
                    </html>")]
    (spit salida html)
    (println "Archivo generado:" salida)))

(resaltador/basic->html "9. Proyecto Resaltador Léxico/sinewave.bas")
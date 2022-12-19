(ns keystone
  (:require [instaparse.core :as insta]))

(def as-and-bs
  (insta/parser
   "S = expr
    expr = term (('+' | '-') term)*
    term = fact (('*' | '/') fact)*
    fact = ws* <'('> ws* expr ws* <')'> ws* | ws* ('+' | '-') ws* fact ws* | ws* number ws*
    <number> = #'[0-9]+'
    whitespace = (' ' | '\t')
    <ws> = <whitespace>"))

(defn run [args]
  (println args)
  (print (as-and-bs (:code args))))
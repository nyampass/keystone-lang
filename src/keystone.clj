(ns keystone
  (:require [instaparse.core :as insta]
            [clojure.core :refer [parse-long]]))


(def parser
  (insta/parser
   "s ::= block
  block ::= {stat}    
  <stat> ::= <space> ( define | op | loop | functioncall | label | goto name | do block end | while exp do block end | repeat block until exp | if | for name '=' exp ',' exp [',' exp] do block end | for namelist in explist do block end | function funcname funcbody | local function name funcbody | local namelist ['=' explist] ) <'\n'*>
  <space> ::= #\"\\s*\"    
  op ::= ( print | move ) <space> exp <space> 
  print ::= 'print'
  move ::= 'move'
	define ::=  name <space> '=' <space> exp
  loop ::= 'loop' <space> exp <space> <'\n'*> block <'\n'*> <end>
  if ::= 'if' <space> exp <space> <'\n'*> block <'\n'*> <end>

  goto ::= 'goto'
  do ::= 'do'
  end ::= 'end'
  while ::= 'white'
  repeat ::= 'repeat'
  until ::= 'until'
  then ::= 'then'
  elseif ::= 'elseif'
  else ::= 'else'
  for ::= 'for'
  in ::= 'in'
  function ::= 'function'
  local ::= 'local'
  and ::= 'and'
  or ::= 'or'
  nil ::= 'nil'
  true ::= 'true'
  false ::= 'false'
  not ::= 'not'
  return ::= 'return'

	label ::= '::' name '::'
  
  numeral ::= #\"\\d\"

	funcname ::= name {'.' name} [':' name]

	namelist ::= name {',' name}

	explist ::= exp {',' exp}

	exp ::=  exp <space> binop <space> exp | unop exp | name | nil | false | true | numeral | literal-string | '...' | functiondef
 
  literal-string ::= '\"' #\"[^\\\"]+\" '\"'
 
  name ::= #\"[a-zA-Z]\\w*\"
 
functioncall ::=  name args

	args ::=  '(' [explist] ')' | literal-string

	functiondef ::= function funcbody

	funcbody ::= '(' [parlist] ')' block end

	parlist ::= namelist [',' '...'] | '...'

	binop ::=  '+' | '-' | '*' | '/' | '//' | '^' | '%' | '&' | '~' | '|' | '>>' | '<<' | '..' | '<' | '<=' | '>' | '>=' | '==' | '~=' | and | or

	unop ::= '-' | not | '#' | '~'"))

(defn parse [str]
  (let [[_ & [stats]] (parser str)]
    stats))

(defn -op [[op-name] & args]
  {:op op-name :args args})

(defn -define [& args]
  (let [[[_ name] _ val] args]
    (prn :-define args name val)
    {:op :define :args (list name val)}))

(defn -name [& args]
  [:name (keyword (first args))])

(defn -exp [& args]
  (prn :-exp args)
  (if (= (count args) 1)
    (first args)
    (if (and (= (count args) 3)
             (= (-> args second first) :binop))
      {:op (-> args second second keyword) :args (list (first args) (nth args 2))}
      {:exp args})))

(defn -literal-string [& [_ args _]]
  args)

(defn -numeral [& [val]]
  (parse-long val))

(defn -loop [_ cond & [args]]
  {:op :loop  :condition cond :args args})

(defn -if [_ cond & args]
  (prn :-if cond args)
  {:op :if :condition cond :args args})

(defn transform [stats]
  (insta/transform {:block (fn [& args] args)
                    :op -op
                    :define -define
                    :name -name
                    :exp -exp
                    :literal-string -literal-string
                    :numeral -numeral
                    :loop -loop
                    :if -if} stats))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn run [{:keys [code]}]
  (print (parse code)))
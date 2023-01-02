(ns keystone
  (:require [instaparse.core :as insta]
            [clojure.core :refer [parse-long]]))


(def parser
  (insta/parser
   "s ::= block
    block ::= {stat}
    
    <stat> ::= <space> ( define | op | loop | functioncall | label | goto name | do block end | while exp do block end | repeat block until exp | if exp then block {elseif exp then block} [else block] end | for name '=' exp ',' exp [',' exp] do block end | for namelist in explist do block end | function funcname funcbody | local function name funcbody | local namelist ['=' explist] ) <'\n'*>
  
  <space> ::= #\"\\s*\"
    
  op ::= ( print | move ) <space> exp <space> 
  print ::= 'print'
  move ::= 'move'
	define ::=  name <space> '=' <space> exp
  loop ::= 'loop' <space> exp <space> <'\n'*> block  <'\n'*> end  <'\n'*>

  goto ::= 'goto'
  do ::= 'do'
  end ::= 'end'
  while ::= 'white'
  repeat ::= 'repeat'
  until ::= 'until'
  if ::= 'if'
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

	<exp> ::=  name | nil | false | true | numeral | literal-string | '...' | functiondef | tableconstructor | exp binop exp | unop exp
 
  literal-string ::= '\"' #\"[^\\\"]+\" '\"'
 
  name ::= #\"[a-zA-Z]\\w*\"
 
functioncall ::=  name args

	args ::=  '(' [explist] ')' | tableconstructor | literal-string

	functiondef ::= function funcbody

	funcbody ::= '(' [parlist] ')' block end

	parlist ::= namelist [',' '...'] | '...'

	tableconstructor ::= '{' [fieldlist] '}'

	fieldlist ::= field {fieldsep field} [fieldsep]

	field ::= '[' exp ']' '=' exp | name '=' exp | exp

	fieldsep ::= ',' | ';'

	binop ::=  '+' | '-' | '*' | '/' | '//' | '^' | '%' | '&' | '~' | '|' | '>>' | '<<' | '..' | '<' | '<=' | '>' | '>=' | '==' | '~=' | and | or

	unop ::= '-' | not | '#' | '~'"))

(defn parse [str]
  (prn (parser str))
  (let [[_ & [stats]] (parser str)]
    (print :parse parse)
    stats))

(defn -op [[op-name] & args]
  {:op op-name :args args})

(defn -define [& [[_ name] _ val]]
  {:op :define :args (list name val)})

(defn -name [& args]
  [:name (keyword (first args))])

(defn -literal-string [& [_ args _]]
  args)

(defn -numeral [& [val]]
  (parse-long val))

(defn transform [stats]
  (insta/transform {:block (fn [& args] args)
                    :op -op
                    :define -define
                    :name -name
                    :literal-string -literal-string
                    :numeral -numeral} stats))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn run [{:keys [code]}]
  (print (parse code)))
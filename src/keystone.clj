(ns keystone
  (:require [instaparse.core :as insta]))

(def parser
  (insta/parser
   "s ::= block
    block ::= {stat}
    
    <stat> ::= ( var <space> '=' <space> exp | op | functioncall | label | goto name | do block end | while exp do block end | repeat block until exp | if exp then block {elseif exp then block} [else block] end | for name '=' exp ',' exp [',' exp] do block end | for namelist in explist do block end | function funcname funcbody | local function name funcbody | local namelist ['=' explist] ) '\n'*
  
  <space> ::= #\"\\s*\"
    
  op ::= ( print | move ) <space> exp <space> 
  print ::= 'print'
  move ::= 'move'

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
  false ::= 'failse'
  not ::= 'not'
  return ::= 'return'

	label ::= '::' name '::'
  
  Numeral ::= #\"\\d\"

	funcname ::= name {'.' name} [':' name]

	varlist ::= var {',' var}

	var ::=  name | prefixexp '[' exp ']' | prefixexp '.' name

	namelist ::= name {',' name}

	explist ::= exp {',' exp}

	<exp> ::=  nil | false | true | Numeral | literal-string | '...' | functiondef | prefixexp | tableconstructor | exp binop exp | unop exp
 
  literal-string ::= '\"' #\"[^\\\"]+\" '\"'
 
  name ::= #\"[a-zA-Z]\\w*\"
 
	prefixexp ::= var | functioncall | '(' exp ')'

	functioncall ::=  prefixexp args | prefixexp ':' name args

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
  (let [[_ & [stats]] (parser str)]
    stats))

(defn -op [[op-name] & args]
  ;; (prn :op args)
  {:op op-name :args args})

(defn -literal-string [& [_ args _]]
  (prn :literal :args args)
  args)

(defn transform [stats]
  (insta/transform {:block (fn [& args] args)
                    :op -op :literal-string -literal-string} stats))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn run [{:keys [code]}]
  (print (parse code)))
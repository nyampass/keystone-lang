(ns keystone
  (:require [instaparse.core :as insta]))

(def parser
  (insta/parser
   "S ::= block
  block ::= {stat}

  <stat> ::= ( varlist <space> '=' <space> explist | op | functioncall | label | break | goto Name | do block end | while exp do block end | repeat block until exp | if exp then block {elseif exp then block} [else block] end | for Name '=' exp ',' exp [',' exp] do block end | for namelist in explist do block end | function funcname funcbody | local function Name funcbody | local namelist ['=' explist] ) '\n'*
  
  <space> ::= #\"\\s*\"
    
  op ::= print <space> exp <space> 
  print ::= 'print'

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

	label ::= '::' Name '::'
  
  Numeral ::= #\"\\d\"

	funcname ::= Name {'.' Name} [':' Name]

	varlist ::= var {',' var}

	var ::=  Name | prefixexp '[' exp ']' | prefixexp '.' Name

	namelist ::= Name {',' Name}

	explist ::= exp {',' exp}

	<exp> ::=  nil | false | true | Numeral | literal-string | '...' | functiondef | prefixexp | tableconstructor | exp binop exp | unop exp
 
  literal-string ::= '\"' #\"[^\\\"]+\" '\"'
 
  Name ::= #\"[a-zA-Z]\\w*\"
 
  break ::= '\n'

	prefixexp ::= var | functioncall | '(' exp ')'

	functioncall ::=  prefixexp args | prefixexp ':' Name args

	args ::=  '(' [explist] ')' | tableconstructor | literal-string

	functiondef ::= function funcbody

	funcbody ::= '(' [parlist] ')' block end

	parlist ::= namelist [',' '...'] | '...'

	tableconstructor ::= '{' [fieldlist] '}'

	fieldlist ::= field {fieldsep field} [fieldsep]

	field ::= '[' exp ']' '=' exp | Name '=' exp | exp

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
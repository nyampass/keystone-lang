(ns keystone
  (:require [instaparse.core :as insta]))

(def as-and-bs
  (insta/parser
   "chunk ::= block

	block ::= {stat}

  stat ::=  ( varlist <space> '=' <space> explist | functioncall | label | break | goto Name | do block end | while exp do block end | repeat block until exp | if exp then block {elseif exp then block} [else block] end | for Name '=' exp ',' exp [',' exp] do block end | for namelist in explist do block end | function funcname funcbody | local function Name funcbody | local namelist ['=' explist] ) '\n'
    
  <space> ::= #\"\\s*\"

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

	exp ::=  nil | false | true | Numeral | LiteralString | '...' | functiondef | prefixexp | tableconstructor | exp binop exp | unop exp
 
  LiteralString ::= 'hoge' | 'def'
 
  Name ::= #\"[a-zA-Z]\\w*\"
 
  break ::= '\n'

	prefixexp ::= var | functioncall | '(' exp ')'

	functioncall ::=  prefixexp args | prefixexp ':' Name args

	args ::=  '(' [explist] ')' | tableconstructor | LiteralString

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
  (prn str)
  (as-and-bs str))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn run [{:keys [code]}]
  (print (as-and-bs code)))
(ns keystone.core
  (:require [instaparse.core :as insta]
            [clojure.core :refer [parse-long]]
            [ring.adapter.jetty9 :as jetty]
            [clojure.data.json :as json])
  (:gen-class))

(def parser
  (insta/parser "
  s ::= block
  block ::= {stat}
  <stat> ::= <space> ( define | op | loop | functioncall | label | if | for name '=' exp ',' exp [',' exp] do block end | for namelist in explist do block end | function funcname funcbody | local function name funcbody | local namelist ['=' explist] ) <'\n'*>
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
  numeral ::= #\"\\-?\\d+\"
	funcname ::= name {'.' name} [':' name]
	namelist ::= name {',' name}
	explist ::= exp {',' exp}
	exp ::=  exp <space> binop <space> exp | unop exp | <'('> exp <')'> | name | nil | false | true | numeral | literal-string | '...' | functiondef
  literal-string ::= '\"' #\"[^\\\"]+\" '\"'
  name ::= #\"[a-zA-Z]\\w*\"
  functioncall ::=  name args
	args ::=  '(' [explist] ')' | literal-string
	functiondef ::= function funcbody
	funcbody ::= '(' [parlist] ')' block end
	parlist ::= namelist [',' '...'] | '...'
	binop ::=  '+' | '-' | '*' | '/' | '//' | '^' | '%' | '&' | '~' | '|' | '>>' | '<<' | '..' | '<' | '<=' | '>' | '>=' | '==' | '!=' |  and | or
	unop ::= '-' | not | '#' | '~'"))

(defn parse [str]
  (let [ret (parser str)]
    (if (not (insta/failure? ret))
      (let [[_ & [stats]] ret]
        stats)
      (let [failure (insta/get-failure ret)]
        (throw (ex-info (prn-str failure) {:failure ret}))))))

(defn -op [[op-name] & args]
  {:op op-name :args args})

(defn -define [& args]
  (let [[[_ name] _ val] args]
    {:op :define :args (list name val)}))

(defn -name [& args]
  [:name (keyword (first args))])

(defn -binop [op]
  [:binop (keyword (if (sequential? op)
                     (second op)
                     op))])

(defn -exp [& args]
  (if (= (count args) 1)
    (let [v (first args)]
      (if (sequential? v)
        (condp = (first v)
          :true true :false false :nil nil v)
        v))
    (if (and (= (count args) 3)
             (= (-> args second first) :binop))
      {:op (-> args second second) :args (list (first args) (nth args 2))}
      {:exp args})))

;; (-exp {:op :!=, :args (list 1 2)} [:binop [:and "and"]] {:op :==, :args (list 3 3)})

(defn -literal-string [& [_ args _]]
  args)

(defn -numeral [& [val]]
  (parse-long val))

(defn -loop [_ cond & [args]]
  {:op :loop  :condition cond :args args})

(defn -if [_ cond & [args]]
  {:op :if :condition cond :args args})

(defn transform [stats]
  #_{:clj-kondo/ignore [:unresolved-var]}
  (insta/transform
   {:block (fn [& args] args)
    :op -op
    :define -define
    :name -name
    :binop -binop
    :exp -exp
    :literal-string -literal-string
    :numeral -numeral
    :loop -loop
    :if -if}
   stats))

(defn -eval-binop [op-keyword [left right]]
  (prn :-exp-binop op-keyword left right)
  (if-let [op ({:+ + :- - :* * :> > :< <
                :== = :!= not=} op-keyword)]
    (cond-> (op left right)
      (#{> < = not=} op) boolean)
    (condp = op-keyword
      :and (boolean (and left right))
      :or (boolean (or left right))
      nil)))

(defn -eval-exp [exp env]
  ;; (prn :-eval-exp exp env)
  (cond
    (and (vector? exp)
         (=  (count exp) 2)
         (= (first exp) :name))
    ((second exp) env)
    (:op exp)
    (let [[left right] (:args exp)]
      (-eval-binop (:op exp) [(-eval-exp left env) (-eval-exp right env)]))
    :else exp))

(defn -eval-exps [args env]
  (map #(-eval-exp % env)
       args))

(defn -cond [{:keys [op args]} env]
  (let [[left right] [(-eval-exp (first args) env)
                      (-eval-exp (second args) env)]]
    (condp = op
      :> (> left right)
      :< (< left right)
      :== (= left right))))

;; (eval-cond {:op :< :args (list [:name :a] 3)} {:a {:op :-, :args ([:name :a] 2)}})

(defn- -eval [[code & rest] env]
  (when code
    (let [{:keys [op condition args]} code]
      (condp = op
        :define (let [[name val] args]
                  (swap! env assoc name (-eval-exp val @env))
                  (-eval rest env))
        :if (if (-cond condition @env)
              (concat (-eval args env) (-eval rest  env))
              (-eval rest env))
        :loop (if (number? condition)
                (for [_ (range condition)]
                  (-eval args env))
                (-eval rest env))
        (if (contains? #{:print :move} op)
          (concat [{:op op :args (-eval-exps args @env)}] (-eval rest env))
          (concat [code] (-eval rest env)))))))

(defn run [str]
  (let [env (atom {})]
    (prn (-> str parse))
    (-> str parse transform (-eval env) flatten)))

(def api-key (System/getenv "api-key"))

(defn ring-handler
  [{:keys [request-method uri query-string body]}]
  (if (and (= request-method :post)
           (= uri "/api/eval")
           (= query-string (str "api-key=" api-key)))
    {:status 200
     :body (json/write-str (run (slurp body)))}
    {:status 404}))

(defn -main [& _args]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "8080"))]
    (jetty/run-jetty #'ring-handler {:port port :join? false})))

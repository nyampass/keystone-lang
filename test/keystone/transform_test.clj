(ns keystone.transform-test
  (:require [keystone :refer :all]
            [clojure.test :refer :all]))

(testing "hello"
  (let [stats (parse (slurp "./resources/01_hello.ks"))]
    (is (= (transform stats) [{:op :print :args '("hello")}]))))

(testing "move"
  (let [stats (parse (slurp "./resources/02_move.ks"))]
    (is (= (transform stats)
           [{:op :move :args '("right")} {:op :move :args '("right")} {:op :move :args '("left")} {:op :move :args '("up")} {:op :move :args '("down")}]))))

(testing "variable"
  (let [stats (parse (slurp "./resources/03_variable.ks"))]
    (is (= (transform stats)
           [{:op :define :args (list :a 1)}
            {:op :define :args (list :b "def")}
            {:op :print :args (list [:name :a])}
            {:op :print :args (list [:name :b])}
            {:op :define :args (list :c "left")}
            {:op :move :args (list [:name :c])}]))))

(testing "loop"
  (let [stats (parse (slurp "./resources/04_loop.ks"))]
    (is (= (transform stats)
           [{:op :loop :condition 3 :args (list {:op :print :args '("hoge")})}]))))

(testing "if"
  (let [stats (parse (slurp "./resources/05_if.ks"))]
    (is (= (transform stats)
           [{:op :define :args (list :a 5)}
            {:op :if :condition {:op :> :args (list [:name :a] 3)} :args
             (list {:op :print :args (list "abc")}
                   {:op :define :args (list :a {:op :- :args (list [:name :a] 2)})})}
            {:op :if :condition {:op :< :args (list [:name :a] 3)} :args
             (list {:op :print :args (list "def")})}]))))

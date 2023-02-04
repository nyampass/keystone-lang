(ns keystone.transform-test
  (:require [keystone :refer :all]
            [clojure.test :refer :all]))

(testing "hello"
  (let [stats (parse (slurp "./resources/hello.ks"))]
    (is (= (transform stats) [{:op :print :args '("hello")}]))))

(testing "move"
  (let [stats (parse (slurp "./resources/move.ks"))]
    (is (= (transform stats)
           [{:op :move :args '("right")} {:op :move :args '("right")} {:op :move :args '("left")} {:op :move :args '("up")} {:op :move :args '("down")}]))))

(testing "variable"
  (let [stats (parse (slurp "./resources/variable.ks"))]
    (is (= (transform stats)
           [{:op :define :args (list :a 1)}
            {:op :define :args (list :b "def")}
            {:op :print :args (list [:name :a])}
            {:op :print :args (list [:name :b])}
            {:op :define :args (list :c "left")}
            {:op :move :args (list [:name :c])}]))))

(testing "exp"
  (let [stats (parse (slurp "./resources/exp.ks"))
        [l1 l2 l3 l4 l5 l6 l7 l8 l9 l10] (transform stats)]
    (is (= l1 {:op :print :args (list {:op :+, :args (list 1 2)})}))
    (is (= l2 {:op :print :args (list {:op :-, :args (list {:op :*, :args (list 3 4)} {:op :*, :args (list 2 8)})})}))
    (is (= l3 {:op :print :args (list {:op :==, :args (list 1 2)})}))
    (is (= l4 {:op :print :args (list {:op :==, :args (list 1 1)})}))
    (is (= l5 {:op :print :args (list true)}))
    (is (= l6 {:op :print :args (list false)}))
    (is (= l7 {:op :print :args (list {:op :and, :args (list 1 2)})}))
    (is (= l8 {:op :print :args (list {:op :and, :args (list 2 false)})}))
    (is (= l9 {:op :print :args (list {:op :and, :args (list {:op :!=, :args (list 1 2)} {:op :==, :args (list 3 3)})})}))
    (is (= l10 {:op :print :args (list {:op :or, :args (list {:op :!=, :args (list 1 2)} {:op :==, :args (list 3 3)})})}))))

(testing "loop"
  (let [stats (parse (slurp "./resources/loop.ks"))]
    (is (= (transform stats)
           [{:op :loop :condition 3 :args (list {:op :print :args '("hoge")})}]))))

(testing "if"
  (let [stats (parse (slurp "./resources/if.ks"))
        [l1 l2 l3] (transform stats)]
    (is (= l1 {:op :define :args (list :a 5)}))
    (is (= l2 {:op :if :condition {:op :> :args (list [:name :a] 3)} :args
               (list {:op :print :args (list "abc")}
                     {:op :define :args (list :a {:op :- :args (list [:name :a] 2)})})}))
    (is (= l3 {:op :if :condition {:op :< :args (list [:name :a] 3)} :args
               (list {:op :print :args (list "def")})}))))

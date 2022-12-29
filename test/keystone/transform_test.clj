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
    (print stats)
    (is (= (transform stats)
           [{:op :define :args (list :a 1)}
            {:op :define :args (list :b "def")}
            {:op :print :args (list [:name :a])}
            {:op :print :args (list [:name :b])}
            {:op :define :args (list :c "left")}
            {:op :move :args (list [:name :c])}]))))

(testing "if and loop"
  (let [stats (parse (slurp "./resources/04_if_and_loop.ts"))]
    (print stats)
    (is (= (transform stats)
           [{:op :loop :condition 3 :args '[{:op :print :args '("hoge")}]}
            {:op :define :args '(:a 1)}
            {:op :if :condition '([:op :> '(args :a 3)]) :args '(:a)}]))))
(ns keystone.transform-test
  (:require [keystone :refer :all]
            [clojure.test :refer :all]))

(testing "hello"
  (let [stats (parse (slurp "./resources/01_hello.ks"))]
    (prn stats)
    (is (= (transform stats) [{:op :print :args '("hello")}]))))

(testing "move"
  (let [stats (parse (slurp "./resources/02_move.ks"))]
    (prn stats)
    (print (parse (slurp "./resources/02_move.ks")))
    (is (= (transform stats)
           [{:op :move :args '("right")} {:op :move :args '("right")} {:op :move :args '("left")} {:op :move :args '("up")} {:op :move :args '("down")}]))))
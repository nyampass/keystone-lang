(ns keystone.eval-test
  (:require [keystone.core :refer :all]
            [clojure.test :refer :all]))

(testing "hello"
  (is (= (run (slurp "./resources/hello.ks")) [{:op :print :args '("hello")}])))

(testing "move2"
  (is (= (run (slurp "./resources/move2.ks")) 
      [{:op :move :args '("right")} {:op :move :args '("test")} {:op :move :args '("left")} {:op :move :args '("up")} {:op :move :args '("down")} {:op :move :args '("back")}])))

(testing "move"
  (is (= (run (slurp "./resources/move.ks"))
        [{:op :move :args '("right")} {:op :move :args '("right")} {:op :move :args '("left")} {:op :move :args '("up")} {:op :move :args '("down")}])))

(testing "turn"
  (is (= (run (slurp "./resources/turn.ks"))
        [{:op :turn :args '("left")} {:op :turn :args '("right")} {:op :turn :args '("forward")} {:op :turn :args '("left")} {:op :turn :args '("left")} {:op :turn :args '("right")}])))

(testing "variable"
  (is (= (run (slurp "./resources/variable.ks"))
         [{:op :print :args (list 1)}
          {:op :print :args (list "def")}
          {:op :move :args (list "left")}])))

(testing "exp"
  (let [[l1 l2 l3 l4 l5 l6 l7 l8 l9 l10] (run (slurp "./resources/exp.ks"))]
    (is (= l1 {:op :print :args '(3)}))
    (is (= l2 {:op :print :args '(-4)}))
    (is (= l3 {:op :print :args '(false)}))
    (is (= l4 {:op :print :args '(true)}))
    (is (= l5 {:op :print :args '(true)}))
    (is (= l6 {:op :print :args '(false)}))
    (is (= l7 {:op :print :args '(true)}))
    (is (= l8 {:op :print :args '(false)}))
    (is (= l9 {:op :print :args '(true)}))
    (is (= l10 {:op :print :args '(true)}))))

(testing "loop"
  (is (= (run (slurp "./resources/loop.ks"))
         [{:op :print :args '("hoge")}
          {:op :print :args '("hoge")}
          {:op :print :args '("hoge")}])))

(testing "if"
  (is (= (run (slurp "./resources/if.ks"))
         [{:op :print :args (list "abc")}])))

(testing "mix"
  (is (= (run (slurp "./resources/mix.ks"))
         [{:op :print, :args (list "1: hogehoge == 1")}
          {:op :print, :args (list "2: hogehoge > 1")}
          {:op :print, :args (list 989)}
          {:op :print, :args (list 989)}
          {:op :print, :args (list "あいうえお")}
          {:op :move, :args (list "left")}
          {:op :print, :args (list 989)}
          {:op :print, :args (list "あいうえお")}
          {:op :move, :args (list "left")}])))

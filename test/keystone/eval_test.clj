(ns keystone.eval-test
  (:require [keystone :refer :all]
            [clojure.test :refer :all]))

(testing "hello"
  (is (= (run (slurp "./resources/01_hello.ks")) [{:op :print :args '("hello")}])))

(testing "move"
  (is (= (run (slurp "./resources/02_move.ks"))
         [{:op :move :args '("right")} {:op :move :args '("right")} {:op :move :args '("left")} {:op :move :args '("up")} {:op :move :args '("down")}])))

(testing "variable"
  (is (= (run (slurp "./resources/03_variable.ks"))
         [{:op :print :args (list 1)}
          {:op :print :args (list "def")}
          {:op :move :args (list "left")}])))

(testing "loop"
  (is (= (run (slurp "./resources/04_loop.ks"))
         [{:op :print :args '("hoge")}
          {:op :print :args '("hoge")}
          {:op :print :args '("hoge")}])))

(testing "if"
  (is (= (run (slurp "./resources/05_if.ks"))
         [{:op :print :args (list "abc")}])))

(testing "mix"
  (is (= (run (slurp "./resources/06_mix.ks"))
         [{:op :print, :args (list "1: hogehoge == 1")}
          {:op :print, :args (list "2: hogehoge > 1")}
          {:op :print, :args (list 989)}
          {:op :print, :args (list 989)}
          {:op :print, :args (list "あいうえお")}
          {:op :move, :args (list "left")}
          {:op :print, :args (list 989)}
          {:op :print, :args (list "あいうえお")}
          {:op :move, :args (list "left")}])))

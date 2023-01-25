(ns keystone.eval-test
  (:require [keystone :refer :all]
            [clojure.test :refer :all]))

;; (testing "hello"
;;   (let [res (-> (slurp "./resources/01_hello.ks") parse transform eval)]
;;     (is (= res [{:op :print :args '("hello")}]))))

;; (testing "move"
;;   (let [res (-> (slurp "./resources/02_move.ks") parse transform eval)]
;;     (is (= res
;;            [{:op :move :args '("right")} {:op :move :args '("right")} {:op :move :args '("left")} {:op :move :args '("up")} {:op :move :args '("down")}]))))

;; (testing "variable"
;;   (let [res (-> (slurp "./resources/03_variable.ks") parse transform eval)]
;;     (prn :res res)
;;     (is (= res
;;            [{:op :print :args (list 1)}
;;             {:op :print :args (list "def")}
;;             {:op :move :args (list "left")}]))))

(testing "loop"
  (let [res (-> (slurp "./resources/04_loop.ks") parse transform eval)]
    (is (= res
           [{:op :print :args '("hoge")}
            {:op :print :args '("hoge")}
            {:op :print :args '("hoge")}]))))

;; (testing "if"
;;   (let [stats (parse (slurp "./resources/05_if.ks"))]
;;     (prn :stats stats)
;;     (is (= (transform stats)
;;            [{:op :define :args (list :a 1)}
;;             {:op :if :condition {:op :> :args (list [:name :a] 3)} :args
;;              (list {:op :print :args (list "abc")}
;;                    {:op :define :args (list :a {:op :- :args (list [:name :a] 2)})})}]))))


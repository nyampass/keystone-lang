(ns keystone.transform-test
  (:require [keystone :refer :all]
            [clojure.test :refer :all]))

(testing "foo works"
  (let [stats (parse (slurp "./resources/01_hello.ks"))]
    (prn stats)
    (is (= (transform stats) [{:op :print :args '("hello")}]))))
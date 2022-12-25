(ns keystone.parse-test
  (:require [keystone :refer :all]
            [clojure.test :refer :all]))

(testing "foo works"
  (is (= (parse (slurp "./resources/01_hello.ks")) [{:op :print :args "hello"}])))
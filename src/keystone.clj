(ns keystone
  (:require [instaparse.core :as insta]))

(def as-and-bs
  (insta/parser
   "S = AB*
     AB = A B
     A = 'a'+
     B = 'b'+"))

(defn run [args]
  (println args)
  (print (as-and-bs (:code args))))
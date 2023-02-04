(ns cql.core-test
  (:require
   [clojure.test :refer :all]
   [cql.core :as cql]))

(def data
  (atom {:birds [{:id 1, :name "Parroty", :type "parakeet"}
                 {:id 2, :name "Egg", :type "parakeet"}
                 {:id 3, :name "Rasta", :type "toucan"}
                 {:id 4, :name "Lucky", :type "pigeon"}]}))

(deftest ^:parallel query-test
  (is (= (:birds @data)
         (cql/query data "SELECT * FROM birds" nil)))
  (is (= [{:id 1, :name "Parroty", :type "parakeet"}
          {:id 2, :name "Egg", :type "parakeet"}]
         (cql/query data "SELECT * FROM birds WHERE type = 'parakeet'" nil))))

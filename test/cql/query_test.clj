(ns cql.query-test
  (:require
   [clojure.test :refer :all]
   [cql.query :as query]))

(deftest ^:parallel error-test
  (testing "Error on clauses that aren't implemented yet."
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"No implementation of cql.util/query-xform for :order-by"
         (query/query {} "SELECT * FROM birds WHERE type = 'toucan' ORDER BY true")))))

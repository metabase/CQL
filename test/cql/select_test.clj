(ns cql.select-test
  (:require
   [clojure.test :refer :all]
   [cql.select :as select]))

(defn select [what]
  (transduce
   (select/select-xform what)
   conj
   [{:id 1, :name "Cam"}
    {:id 2, :name "Cam"}]))

(deftest ^:parallel select-test
  (are [what expected] (= expected (select what))
    [[:star]]
    [{:id 1, :name "Cam"}
     {:id 2, :name "Cam"}]

    [[:identifier "id"] [:identifier "name"]]
    [{:id 1, :name "Cam"}
     {:id 2, :name "Cam"}]

    [[:identifier "id"]]
    [{:id 1}
     {:id 2}])

  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Column not found: :does-not-exist"
       (select [[:identifier "does-not-exist"]]))))

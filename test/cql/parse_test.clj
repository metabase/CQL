(ns cql.parse-test
  (:require
   [clojure.test :refer :all]
   [cql.parse :as parse]))

(deftest ^:parallel string-literal-test
  (is (= {:select [[:literal "abc"]]}
         (parse/parse "SELECT 'abc'")))
  (testing "escaped"
    (is (= {:select [[:literal "ab''c"]]}
           (parse/parse "SELECT 'ab''c'")))))

(deftest ^:parallel jdbc-placeholder-test
  (is (= {:select [[:star]]
          :from   [[:identifier "table"]]
          :where  [:= [:jdbc-placeholder] 100]}
         (parse/parse "SELECT * FROM table WHERE ? = 100"))))

(deftest ^:parallel function-call-no-args-test
  (is (= {:select [[:fn [:identifier "now"]]]}
         (parse/parse "SELECT now()"))))

(deftest ^:parallel conditions-test
  (doseq [condition [">" "<" "=" ">=" "<="]]
    (testing condition
      (is (= {:select [[:star]]
              :from   [[:identifier "TABLE"]]
              :where  [(keyword condition) [:identifier "x"] 1]}
             (parse/parse (format "SELECT * FROM TABLE WHERE x%s1" condition)))))))

(deftest ^:parallel between-test
  (is (= {:select [[:star]]
          :from   [[:identifier "table"]]
          :where  [:between [:identifier "x"] 1 2]}
         (parse/parse "SELECT * FROM table WHERE x BETWEEN 1 AND 2"))))

(deftest ^:parallel compound-condition-test
  (doseq [[k s] {:or "OR"
                 :and "AND"}]
    (testing s
      (is (= {:select [[:star]]
              :from   [[:identifier "table"]]
              :where  [k [:= [:identifier "x"] 1] [:= [:identifier "y"] 2]]}
             (parse/parse (format "SELECT * FROM table WHERE (x = 1 %s y = 2)" s))))))

  (is (= {:select [[:star]]
          :from [[:identifier "table"]]
          :where [:and
                  [:= [:identifier "x"] 1]
                  [:or
                   [:= [:identifier "y"] 2]
                   [:= [:identifier "z"] 3]]]}
         (parse/parse "SELECT * FROM table WHERE (x = 1 AND (y = 2 OR z = 3))"))))

(deftest ^:parallel group-by-test
  (is (= {:select   [[:star]]
          :from     [[:identifier "ORDERS"]]
          :group-by [[:fn
                      [:identifier "parsedatetime"]
                      [:fn [:identifier "year"] [:identifier "source" "CREATED_AT"]]
                      [:literal "yyyy"]]]}
         (parse/parse "SELECT * FROM ORDERS GROUP BY parsedatetime(year(source.CREATED_AT), 'yyyy')")))
  (is (= {:select   [[:star]],
          :from     [[:identifier "table"]]
          :where    [:= [:identifier "x"] 1]
          :group-by [[:identifier "x"]]}
         (parse/parse "SELECT * FROM table WHERE x = 1 GROUP BY x"))))

(deftest ^:parallel nested-function-calls-test
  (is (= {:select [[:as
                    [:fn
                     [:identifier "parsedatetime"]
                     [:fn [:identifier "year"] [:identifier "source" "CREATED_AT"]]
                     [:literal "yyyy"]]
                    [:identifier "CREATED_AT"]]]}
         (parse/parse "SELECT parsedatetime(year(source.CREATED_AT), 'yyyy') AS CREATED_AT")))
  (is (= {:select [[:fn
                    [:identifier "parsedatetime"]
                    [:fn
                     [:identifier "year"]
                     [:fn [:identifier "now"]]]
                    [:literal "yyyy"]]]}
         (parse/parse "SELECT parsedatetime(year(now()), 'yyyy')"))))

(deftest ^:parallel wrapped-condition-test
  (is (= {:select [[:star]]
          :where  [:= [:identifier "x"] [:identifier "y"]]}
         (parse/parse "SELECT * WHERE (x = y)"))))

(deftest ^:parallel cast-test
  (is (= {:select [[:cast -2 [:identifier "long"]]]}
         (parse/parse "SELECT CAST(-2 AS long)"))))

(deftest ^:parallel order-by-test
  (is (= {:select   [[:star]]
          :from     [[:identifier "table"]]
          :order-by [[[:fn
                       [:identifier "parsedatetime"]
                       [:fn [:identifier "year"] [:identifier "source" "CREATED_AT"]]
                       [:literal "yyyy"]]
                      :asc]]}
         (parse/parse "SELECT * FROM table ORDER BY parsedatetime(year(source.CREATED_AT), 'yyyy') ASC"))))

(deftest ^:parallel arithmetic-expression-test
  (is (= {:select [[:+ 1 2]]}
         (parse/parse "SELECT 1 + 2")))
  (is (= {:select [[:+ 1 2]]}
         (parse/parse "SELECT (1 + 2)")))
  (testing "order of operations"
    (is (= {:select [[:*
                      [:+ 1 2]
                      [:/
                       [:+ 3 4]
                       5]]]}
           (parse/parse "SELECT 1 + 2 * 3 + 4 / 5")
           (parse/parse "SELECT (1 + 2) * (3 + 4 / 5)")
           (parse/parse "SELECT (1 + 2) * ((3 + 4) / 5)")))))

(deftest ^:parallel reserved-keywords-test
  (is (thrown?
       clojure.lang.ExceptionInfo
       (parse/parse "SELECT JOIN")))
  (is (thrown?
       clojure.lang.ExceptionInfo
       (parse/parse "SELECT JOIN ")))
  (is (= {:select [[:identifier "JOINS"]]}
         (parse/parse "SELECT JOINS")))
  (is (= {:select [[:identifier "JJOIN"]]}
         (parse/parse "SELECT JJOIN"))))

(deftest ^:parallel null-test
  (is (= {:select [nil]}
         (parse/parse "SELECT NULL"))))

(deftest ^:parallel select-as-test
  (is (= {:select [[:as
                    [:identifier "x"]
                    [:identifier "y"]]]}
         (parse/parse "SELECT x AS y"))))

(deftest ^:parallel case-test
  (is (= {:select [[:case
                    [:= [:identifier "a"] [:identifier "b"]]
                    [:identifier "a"]
                    [:identifier "source" "count"]]]}
         (parse/parse "SELECT CASE WHEN a = b THEN a ELSE source.count END")))
  (is (= {:select [[:/
                    [:cast [:identifier "Q2" "count"] [:identifier "float"]]
                    [:case
                     [:= [:identifier "source" "count"] 0] nil
                     [:identifier "source" "count"]]]]}
         (parse/parse "SELECT (CAST(Q2.count AS float) / CASE WHEN source.count = 0 THEN NULL ELSE source.count END)"))))

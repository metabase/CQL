(ns cql.core
  (:require
   [cql.options :as options]
   [cql.query :as query]))

(defn query
  ([data a-query]
   (query/query data a-query))
  ([data a-query options]
   (binding [options/*options* options]
     (query/query data a-query))))

(comment
  (cql.core/query (requiring-resolve 'cql.core-test/data) "SELECT * FROM birds WHERE type = 'parakeet'" nil)
  (cql.core/query (requiring-resolve 'cql.core-test/data) "SELECT * FROM birds WHERE type = 'parakeet'" {:debug true}))

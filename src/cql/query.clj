(ns cql.query
  (:require
   [clojure.spec.alpha :as s]
   [cql.from :as from]
   [cql.parse :as parse]
   [cql.where :as where]
   [methodical.core :as m]
   [puget.printer :as puget]))

(comment s/keep-me)

(m/defmulti query
  {:arglists            '([data a-query options])
   :defmethod-arities   #{3}
   :dispatch-value-spec (s/nonconforming (s/or :default    (partial = :default)
                                               :data-query (let [default-or-class-name (s/or :default    (partial = :default)
                                                                                             :class-name symbol?)]
                                                             (s/cat :data  default-or-class-name
                                                                    :query default-or-class-name))))}
  (fn [data a-query _options]
    [(type data) (type a-query)]))

(m/defmethod query :around :default
  [data a-query options]
  (if (:debug options)
    (do
      (puget/cprint (list `query data a-query))
      (next-method data a-query options))
    (next-method data a-query options)))

(m/defmethod query [clojure.lang.IDeref :default]
  "If we get something like an atom unwrap it and recursively call [[query]]."
  [data a-query options]
  (query @data a-query options))

(m/defmethod query [:default String]
  [data s options]
  (query data (parse/parse s) options))

(m/defmulti apply-clause
  {:arglists '([data clause v options]), :defmethod-arities #{4}}
  (fn [_data clause _v _options]
    (keyword clause)))

(m/defmethod apply-clause :around :default
  [data clause v options]
  (if (:debug options)
    (do
      (puget/cprint (list `apply-clause data clause v options))
      (let [result (next-method data clause v options)]
        (println '=>)
        (puget/cprint result)
        result))
    (next-method data clause v options)))

(m/defmethod apply-clause :from
  [data _from from-clause options]
  (from/apply-from data from-clause options))

(m/defmethod apply-clause :where
  [data _where where-clause options]
  (where/apply-where data where-clause options))

(m/defmethod apply-clause :select
  [data _select select _options]
  (when-not (= select [[:star]])
    (throw (ex-info "Only SELECT * is currently implemented!"
                    {:select select})))
  data)

(m/defmethod query [clojure.lang.IPersistentMap clojure.lang.IPersistentMap]
  [data a-query options]
  ;; TODO this should probably use `reduce` or something to apply all the keys in the appropriate order using a
  ;; multimethod for each key.
  (cond-> data
    (:from a-query)   (apply-clause :from   (:from a-query)   options)
    (:select a-query) (apply-clause :select (:select a-query) options)
    (:where a-query)  (apply-clause :where  (:where a-query)  options)))

(comment
  (cql.query/query (requiring-resolve 'cql.query-test/data) "SELECT * FROM birds WHERE type = 'parakeet'" nil)
  (cql.query/query (requiring-resolve 'cql.query-test/data) "SELECT * FROM birds WHERE type = 'parakeet'" {:debug true}))

(ns cql.query
  (:require
   [clojure.spec.alpha :as s]
   [cql.from :as from]
   [cql.options :as options]
   [cql.parse :as parse]
   [cql.where :as where]
   [methodical.core :as m]))

(comment s/keep-me)

(m/defmulti query
  {:arglists            '([data a-query] [data a-query options])
   :defmethod-arities   #{2}
   :dispatch-value-spec (s/nonconforming (s/or :default    (partial = :default)
                                               :data-query (let [default-or-class-name (s/or :default    (partial = :default)
                                                                                             :class-name symbol?)]
                                                             (s/cat :data  default-or-class-name
                                                                    :query default-or-class-name))))}
  (fn [data a-query]
    [(type data) (type a-query)]))

(m/defmethod query :around :default
  [data a-query]
  (options/debug-println (list `query data a-query))
  (next-method data a-query))

(m/defmethod query [clojure.lang.IDeref :default]
  "If we get something like an atom unwrap it and recursively call [[query]]."
  [data a-query]
  (query @data a-query))

(m/defmethod query [:default String]
  [data s]
  (query data (parse/parse s)))

(m/defmulti apply-clause
  {:arglists '([data clause v]), :defmethod-arities #{3}}
  (fn [_data clause _v]
    (keyword clause)))

(m/defmethod apply-clause :around :default
  [data clause v]
  (options/debug-println (list `apply-clause data clause v))
  (let [result (next-method data clause v)]
    (options/debug-println clause '=> result)
    result))

(m/defmethod apply-clause :from
  [data _from from-clause]
  (from/apply-from data from-clause))

(m/defmethod apply-clause :where
  [data _where where-clause]
  (where/apply-where data where-clause))

(m/defmethod apply-clause :select
  [data _select select]
  (when-not (= select [[:star]])
    (throw (ex-info "Only SELECT * is currently implemented!"
                    {:select select})))
  data)

(m/defmethod query [clojure.lang.IPersistentMap clojure.lang.IPersistentMap]
  [data a-query]
  ;; TODO this should probably use `reduce` or something to apply all the keys in the appropriate order using a
  ;; multimethod for each key.
  (cond-> data
    (:from a-query)   (apply-clause :from   (:from a-query))
    (:select a-query) (apply-clause :select (:select a-query))
    (:where a-query)  (apply-clause :where  (:where a-query))))

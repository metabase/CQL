(ns cql.query
  (:require
   [clojure.spec.alpha :as s]
   [cql.from :as from]
   [cql.options :as options]
   [cql.parse :as parse]
   [cql.select :as select]
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

(m/defmulti clause-xform
  "Return a transducer based on `clause` e.g. `:select` and `v` e.g. `[[:identifier :id]]`."
  {:arglists            '([clause v])
   :defmethod-arities   #{2}
   :dispatch-value-spec keyword?}
  (fn [clause _v]
    (keyword clause)))

(m/defmethod clause-xform :from
  [_from from-clause]
  (from/from-xform from-clause))

(m/defmethod clause-xform :where
  [_where where-clause]
  (where/where-xform where-clause))

(m/defmethod clause-xform :select
  [_select select-clause]
  (select/select-xform select-clause))

;;; TODO -- need a more clever way to specify this so we can add more clauses without having to hardcoding them
(def clauses
  [:from
   :select
   :where])

(defn query-xform [a-query]
  (reduce comp identity (for [clause clauses
                              :when (contains? a-query clause)]
                          (clause-xform clause (get a-query clause)))))

(m/defmethod query [clojure.lang.IPersistentMap clojure.lang.IPersistentMap]
  [data a-query]
  (transduce
   (query-xform a-query)
   conj
   [data]))

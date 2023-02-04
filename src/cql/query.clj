(ns cql.query
  (:require
   [clojure.spec.alpha :as s]
   [cql.from :as from]
   [cql.options :as options]
   [cql.parse :as parse]
   [cql.select :as select]
   [cql.util :as u]
   [cql.where :as where]
   [methodical.core :as m]))

(set! *warn-on-reflection* true)

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

(m/defmulti query-xform
  "Build the transducer to apply some data to get query results. `query-map` Honey SQL-esque map like

  ```clj
  {:select ..., :from ...}
  ```

  The methods for every key on the map will be invoked, and should return a transducer; they are combined with `comp`.
  The order they are applied is determined by this multimethod's preferences (i.e., `m/prefer-method!`)."
  {:arglists            '([query-map])
   :defmethod-arities   #{1}
   :dispatch-value-spec keyword?}
  :combo      (u/comp-method-combination)
  :dispatcher (u/map-keys-dispatcher))

(m/defmethod query-xform :default
  [_]
  identity)

(m/defmethod query-xform :from
  [{from-clause :from}]
  (from/from-xform from-clause))

(m/defmethod query-xform :where
  [{where-clause :where}]
  (where/where-xform where-clause))

(m/prefer-method! #'query-xform :from :where)

(m/defmethod query-xform :select
  [{select-clause :select}]
  (select/select-xform select-clause))

(m/prefer-method! #'query-xform :where :select)

(m/defmethod query [clojure.lang.IPersistentMap clojure.lang.IPersistentMap]
  [data a-query]
  (transduce
   (query-xform a-query)
   conj
   [data]))

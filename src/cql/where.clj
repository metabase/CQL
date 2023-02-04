(ns cql.where
  (:require
   [clojure.spec.alpha :as s]
   [cql.options :as options]
   [cql.value :as value]
   [methodical.core :as m]))

(comment s/keep-me)

(m/defmulti where-filter
  {:arglists            '([expr])
   :defmethod-arities   #{1}
   :dispatch-value-spec (s/nonconforming (s/or :keyword keyword? :class-name symbol?))}
  (fn [expr]
    (if (and (vector? expr)
             (keyword? (first expr)))
      (first expr)
      (type expr))))

(m/defmethod where-filter :around :default
  [expr]
  (options/debug-println :where expr)
  (next-method expr))

(defn- equals [row x-expr y-expr]
  (let [x      (value/value row x-expr)
        y      (value/value row y-expr)
        result (= x y)]
    (options/debug-println (list '= x y) '=> result)
    result))

(m/defmethod where-filter :=
  [[_= x-expr y-expr]]
  #(equals % x-expr y-expr))

(defn where-xform [expr]
  (filter (where-filter expr)))

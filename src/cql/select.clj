(ns cql.select
  (:require [methodical.core :as m]))

;;; TODO -- this doesn't deduplicate column names at all
;;;
;;; TODO -- this is pretty inefficient, having to do all the method dispatch and stuff for every expression for every
;;; row. It would be nicer to build some sort of optimized function that could be applied to each row before doing
;;; anything, and apply it to each row.

(defn expr-dispatch-value [expr]
  (if (and (vector? expr)
           (keyword? (first expr)))
    (first expr)
    (type expr)))

(m/defmulti project
  {:arglists '([expr row])}
  (m/dispatch-on-first-arg expr-dispatch-value))

(m/defmethod project :identifier
  [[_identifier k] row]
  (let [k (keyword k)]
    (when-not (contains? row k)
      (throw (ex-info (format "Column not found: %s" (pr-str k))
                      {:row row, :k k})))
    (select-keys row [k])))

(m/defmethod project :star
  [_star row]
  row)

(defn select-xform [exprs]
  (map (fn [row]
         (transduce (map #(project % row)) merge exprs))))

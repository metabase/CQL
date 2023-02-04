(ns cql.value
  (:require
   [clojure.spec.alpha :as s]
   [cql.options :as options]
   [methodical.core :as m]))

(comment s/keep-me)

(m/defmulti value
  {:arglists            '([row expr])
   :defmethod-arities   #{2}
   :dispatch-value-spec (s/nonconforming (s/or :keyword    keyword?
                                               :class-name symbol?))}
  (fn [_row expr]
    (if (and (vector? expr)
             (keyword? (first expr)))
      (first expr)
      (type expr))))

 (m/defmethod value :around :default
   [row expr]
   (options/debug-println (list `value row expr))
   (let [v (next-method row expr)]
     (options/debug-println '=> v)
     v))

(m/defmethod value :identifier
  [row [_identifier k]]
  (let [k (keyword k)]
    (when-not (contains? row k)
      (throw (ex-info (format "No column %s in row. Found: %s"
                              (pr-str k)
                              (pr-str (keys row)))
                      {:k k, :row row})))
    (get row k)))

(m/defmethod value :literal
  [_row [_literal s]]
  s)

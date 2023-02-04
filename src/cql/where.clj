(ns cql.where
  (:require
   [clojure.spec.alpha :as s]
   [cql.options :as options]
   [methodical.core :as m]))

(comment s/keep-me)

(m/defmulti get-value
  {:arglists            '([row x])
   :defmethod-arities   #{2}
   :dispatch-value-spec (s/nonconforming (s/or :keyword    keyword?
                                               :class-name symbol?))}
  (fn [_row x]
    (if (and (vector? x)
             (keyword? (first x)))
      (first x)
      (type x))))

 (m/defmethod get-value :around :default
   [row x]
   (options/debug-println (list `get-value row x))
   (let [v (next-method row x)]
     (options/debug-println '=> v)
     v))

(m/defmethod get-value :identifier
  [row [_identifier k]]
  (let [k (keyword k)]
    (when-not (contains? row k)
      (throw (ex-info (format "No column %s in row. Found: %s"
                              (pr-str k)
                              (pr-str (keys row)))
                      {:k k, :row row})))
    (get row k)))

(m/defmethod get-value :literal
  [_row [_literal s]]
  s)

(m/defmulti apply-where
  {:arglists            '([data where-clause])
   :defmethod-arities   #{2}
   :dispatch-value-spec (s/nonconforming (s/or :keyword keyword? :class-name symbol?))}
  (fn [_data where-clause]
    (if (and (vector? where-clause)
             (keyword? (first where-clause)))
      (first where-clause)
      (type where-clause))))

(m/defmethod apply-where :around :default
  [data where-clause]
  (options/debug-println 'WHERE where-clause)
  (let [result (next-method data where-clause)]
    (options/debug-println '=> result)
    result))

(m/defmethod apply-where :=
  [data [_:= x y]]
  (filter (fn [row]
            (let [x-value (get-value row x)
                  y-value (get-value row y)
                  result  (= x-value y-value)]
              (options/debug-println (list '= x-value y-value) '=> result)
              result))
          data))

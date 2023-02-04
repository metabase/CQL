(ns cql.where
  (:require
   [clojure.spec.alpha :as s]
   [methodical.core :as m]
   [puget.printer :as puget]))

(comment s/keep-me)

(m/defmulti get-value
  {:arglists '([row x options])
   :defmethod-arities #{3}
   :dispatch-value-spec (s/nonconforming (s/or :keyword    keyword?
                                               :class-name symbol?))}
  (fn [_row x _options]
    (if (and (vector? x)
             (keyword? (first x)))
      (first x)
      (type x))))

 (m/defmethod get-value :around :default
  [row x options]
  (if (:debug options)
    (do
      (println (puget/cprint-str (list `get-value row x options)))
      (let [v (next-method row x options)]
        (println '=> (puget/cprint-str v))
        v))
    (next-method row x options)))

(m/defmethod get-value :identifier
  [row [_identifier k] _options]
  (let [k (keyword k)]
    (when-not (contains? row k)
      (throw (ex-info (format "No column %s in row. Found: %s"
                              (pr-str k)
                              (pr-str (keys row)))
                      {:k k, :row row})))
    (get row k)))

(m/defmethod get-value :literal
  [_row [_literal s] _options]
  s)

(m/defmulti apply-where
  {:arglists            '([data where-clause options])
   :defmethod-arities   #{3}
   :dispatch-value-spec (s/nonconforming (s/or :keyword keyword? :class-name symbol?))}
  (fn [_data where-clause _options]
    (if (and (vector? where-clause)
             (keyword? (first where-clause)))
      (first where-clause)
      (type where-clause))))

(m/defmethod apply-where :around :default
  [data where-clause options]
  (if (:debug options)
    (do
      (println 'WHERE (puget/cprint-str where-clause))
      (let [result (next-method data where-clause options)]
        (println '=>)
        (puget/cprint result)
        result))
    (next-method data where-clause options)))

(m/defmethod apply-where :=
  [data [_:= x y] options]
  (filter (fn [row]
            (= (get-value row x options)
               (get-value row y options)))
          data))

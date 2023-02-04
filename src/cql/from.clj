(ns cql.from
  (:require [cql.options :as options]))

(defn- identifier? [x]
  (and (vector? x)
       (= (first x) :identifier)))

(defn from-xform
  ;; only one table currently supported. TODO: error
  [[from]]
  {:post [(some? %)]}
  (let [table (if (identifier? from)
                (keyword (second from))
                ;; TODO -- this error sucks, we need to find a way to preserve the original SQL snippet maybe
                (throw (ex-info (format "Invalid FROM clause: %s" (pr-str from))
                                {:from from})))]
    (options/debug-println :from table)
    (mapcat (fn [data]
              {:pre [(map? data)]}
              (when-not (contains? data table)
                (throw (ex-info (format "Invalid FROM: no such key %s; found: %s"
                                        (pr-str table)
                                        (pr-str (keys data)))
                                {:from from})))
              (get data table)))))

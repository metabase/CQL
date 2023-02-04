(ns cql.select)

(defn select-xform [columns]
  (when-not (= columns [[:star]])
    (throw (ex-info "Only SELECT * is currently implemented!"
                    {:columns columns})))
  identity)

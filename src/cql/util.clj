(ns cql.util
  (:require
   [cql.options :as options]
   [methodical.impl.combo.operator :as m.combo.operator]
   [methodical.impl.dispatcher.common :as m.dispatcher.common]
   [methodical.interface :as m.i]))

(deftype ^:private MapKeysDispatcher [hierarchy prefs default-value]
  m.i/Dispatcher
  (dispatch-value
    [_this m]
    (set (keys m)))

  (matching-primary-methods [_this method-table dispatch-value]
    {:pre [(set? dispatch-value)]}
    (let [ks        (sort-by identity
                             (m.dispatcher.common/domination-comparator @hierarchy prefs)
                             (concat dispatch-value [default-value]))
          k->method (m.i/primary-methods method-table)]
      (for [k ks]
        (or (some-> (get k->method k) (vary-meta assoc :dispatch-value k))
            (throw (ex-info (format "No implementation of %s for %s" `query-xform k)
                            {:k k}))))))

  (matching-aux-methods [_this method-table dispatch-value]
    {:pre [(set? dispatch-value)]}
    (let [ks (sort-by identity
                      (m.dispatcher.common/domination-comparator @hierarchy prefs)
                      (concat dispatch-value [default-value]))]
      (into {} (for [[qualifier k->methods] (m.i/aux-methods method-table)]
                 [qualifier (for [k      ks
                                  method (get k->methods k)]
                              (vary-meta method assoc :dispatch-value k))]))))

  (default-dispatch-value [_this]
    default-value)

  (prefers [_this]
    prefs)

  (with-prefers [_this new-prefs]
    (MapKeysDispatcher. hierarchy new-prefs default-value))

  (dominates? [_this dispatch-val-x dispatch-val-y]
    (m.dispatcher.common/dominates? @hierarchy prefs dispatch-val-x dispatch-val-y)))

(defn map-keys-dispatcher
  "Matches all methods for the keys in a map, e.g. if a multimethod is invoked like

    (m {:x 1, :y 2})

  Then both the `:x` and `:y` methods will be considered matching. The order they are applied is determined by the
  preferences map, e.g. if `:x` is preferred over `:y` then the `:x` method will be considered more specific."
  []
  (->MapKeysDispatcher #'clojure.core/global-hierarchy {} :default))

(m.combo.operator/defoperator ::comp [methodds invoke]
  (loop [xform identity, [method & more] methodds]
    (options/debug-println 'apply 'clause (:dispatch-value (meta method)))
    (let [xform (comp xform (invoke method))]
      (if (seq more)
        (recur xform more)
        xform))))

(defn comp-method-combination
  "Method combination that applies all matching methods and combines them with `comp`."
  []
  (m.combo.operator/operator-method-combination ::comp))

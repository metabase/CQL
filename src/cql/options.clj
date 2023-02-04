(ns cql.options
  (:require [puget.printer :as puget]))

(set! *warn-on-reflection* true)

(def ^:dynamic *options* nil)

(deftype DebugPrintArgs [args])

(defn- print-debug-print-args [printer ^DebugPrintArgs args]
  [:align (for [arg (.args args)]
            [:span (puget/format-doc printer arg) [:text " "]])])

(def print-handlers
  {DebugPrintArgs print-debug-print-args})

(defmacro debug-println [& args]
  `(when (:debug *options*)
     (puget/cprint (->DebugPrintArgs ~(vec args)) {:print-handlers print-handlers})))

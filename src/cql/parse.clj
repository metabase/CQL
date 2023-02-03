(ns cql.parse
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [instaparse.core :as instaparse]
   [methodical.core :as m]))

(set! *warn-on-reflection* true)

(def grammar
  (delay (slurp (io/resource "cql/grammar.bnf"))))

(def parser
  (delay (instaparse/parser @grammar)))

(m/defmulti transform
  {:arglists '([form])}
  first)

(m/defmethod transform :default
  [form]
  form)

(m/defmethod transform :function-call
  [[_ f & args]]
  (into [:fn f] args))

(m/defmethod transform :cast
  [[_ [_ x typename]]]
  [:cast x typename])

(m/defmethod transform :add      [[_ x y]] [:+ x y])
(m/defmethod transform :subtract [[_ x y]] [:- x y])
(m/defmethod transform :divide   [[_ x y]] [:/ x y])
(m/defmethod transform :multiply [[_ x y]] [:* x y])

;; (m/defmethod transform :unquoted-unqualified-indentifier
;;   [[_ s]]
;;   (str/lower-case s))

;; (m/defmethod transform :quoted-unqualified-identifier
;;   [[_ s]]
;;   (quoted s))

(m/defmethod transform :identifier
  [[_ & parts]]
  (into [:identifier] parts))

(m/defmethod transform :integer
  [[_ s]]
  (Long/parseLong s))

(m/defmethod transform :string-literal
  [[_ s]]
  [:literal s])

(m/defmethod transform :null
  [_]
  nil)

;; (remove-method transform :select-as)

(m/defmethod transform :select
  [[_ & args]]
  {:select (vec args)})

(m/defmethod transform :subselect-query
  [[_ & args]]
  (reduce merge {} args))

(m/defmethod transform :subselect
  [[_ query identifier]]
  [query identifier])

;; (m/defmethod transform :from-as
;;   [[_ table alias]]
;;   [table alias])

(m/defmethod transform :from
  [[_ & args]]
  {:from (vec args)})

;; (m/defmethod transform :join-source
;;   [[_ what alias]]
;;   (if alias
;;     [what alias]
;;     what))

(m/defmethod transform :join
  [[_ [join-type] what condition]]
  {join-type [[what condition]]})

(m/defmethod transform :joins
  [[_ & joins]]
  (reduce (partial merge-with concat) {} joins))

(m/defmethod transform :group-by
  [[_ & identifiers]]
  {:group-by (vec identifiers)})

(m/defmethod transform :order-by-subclause
  [[_ identifier [direction]]]
  (if direction
    [identifier direction]
    [identifier :asc]))

(m/defmethod transform :order-by
  [[_ & subclauses]]
  {:order-by (vec subclauses)})

(m/defmethod transform :equals
  [[_ x y]]
  [:= x y])

(m/defmethod transform :greater-than
  [[_ x y]]
  [:> x y])

(m/defmethod transform :less-than
  [[_ x y]]
  [:< x y])

(m/defmethod transform :greater-than-or-equal
  [[_ x y]]
  [:>= x y])

(m/defmethod transform :less-than-or-equal
  [[_ x y]]
  [:<= x y])

(m/defmethod transform :where
  [[_ tree]]
  {:where tree})

(m/defmethod transform :query
  [[_ & args]]
  (reduce merge {} args))

#_(m/defmethod transform :insert-into
  [[_ table & cols]]
  {:insert-into table, :columns cols})

;; (m/defmethod transform :row
;;   [[_ & args]]
;;   (vec args))

;; (m/defmethod transform :values
;;   [[_ & args]]
;;   {:values args})

(defn transform-all [x]
  (walk/postwalk
   (fn [form]
     (if (and (vector? form) (keyword? (first form)))
       (transform form)
       form))
   x))

(defn parse [s]
  (let [parsed (@parser s)]
    (when (instance? instaparse.gll.Failure parsed)
      (let [error-column (:column parsed)
            error-at     (str/join (take 20 (drop error-column (:text parsed))))]
        (throw (ex-info (format "Error parsing SQL at %s" (pr-str error-at))
                        (assoc parsed :error-at error-at)))))
    (transform-all parsed)))

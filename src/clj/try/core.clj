(ns try.core
  (:refer-clojure :exclude [val]
                  :rename {apply clj-apply
                           map   clj-map})
  (:require [clojure.pprint]))


(defn success?
  "Check if try is a successful Try."
  [try]
  (->> try
       :tag
       (= ::success)))

(defn failure?
  "Check if try is a failed Try."
  [try]
  (->> try
       :tag
       (= ::failure)))

(defn val
  "Retrieve the value of the Try."
  [try]
  (:value try))

(defn val-throw
  "Retrieve the value of the Try, but throw an exception if it's a
  failed Try."
  [try]
  (let [v (val try)]
    (if (success? try)
      v
      (throw (if (instance? Throwable v)
               v
               (ex-info "dereferenced failed try" try))))))

(defrecord Try [tag value]
  clojure.lang.IDeref
  (deref [this] (val-throw this)))

(defmethod clojure.pprint/simple-dispatch Try [try]
  ((get-method clojure.pprint/simple-dispatch clojure.lang.IRecord) try))

(defmethod clojure.core/print-method Try [^Try try ^java.io.Writer w]
  ((get-method clojure.core/print-method clojure.lang.IRecord) try w))

(defn succeed
  "Wrap value as a successful Try."
  [value]
  (->Try ::success value))

(defn fail
  "Wrap value as a failed Try."
  [value]
  (->Try ::failure value))

(defmacro tryc
  "Wrap body in a try/catch that returns a successful Try
  if no exception is raised, or a failed Try if an exception
  is raised."
  [& body]
  `(try
     (let [v# (do ~@body)]
       (succeed v#))
     (catch Exception e#
       (fail e#))))

(defmacro tryc-let
  "Wrap a let with bindings and body in a tryc."
  [bindings & body] 
  `(tryc
     (let ~bindings
       ~@body)))

(defn map
  "Apply function f to try's value if is a successful Try."
  [f try]
  (if (success? try)
    (succeed (f (val try)))
    try))

(defmacro map->
  "Map each form to the try."
  [try & forms]
  (let [forms* (for [form forms] 
                 `(map ~(if (seq? form)
                          `#(~(first form) % ~@(next form))
                          `#(~form %))))]
    `(->> ~try
          ~@forms*)))

(defn map-failure
  "Apply function f to try's value if is a failed Try."
  [f try]
  (if (success? try)
    try
    (fail (f (val try)))))

(defmacro map-failure->
  "Map-failure each form to the try."
  [try & forms]
  (let [forms* (for [form forms] 
                 `(map-failure ~(if (seq? form)
                                  `#(~(first form) % ~@(next form))
                                  `#(~form %))))]
    `(->> ~try
          ~@forms*)))

(defn bimap
  "Apply function f to try's value if it's a success, otherwise apply
  function g to try's value if it's a failure."
  [f g try]
  (map-failure g (map f try)))

(defn apply
  "Apply value in ftry if it's a success to value in try if it's also a
  success."
  [ftry try & rest-try]
  (let [all-try (cons try rest-try)]
    (if (success? ftry)
      (if (every? success? all-try)
        (clj-apply (val ftry) (clj-map val all-try))
        try)
      ftry)))

(defn bind
  "Apply f to try's value if is successful."
  [try f]
  (if (success? try)
    (f (val try))
    try))

(defmacro bind->
  "Bind each form to the try."
  [try & forms]
  (let [forms* (for [form forms] 
                 `(bind ~(if (seq? form)
                           `#(~(first form) % ~@(next form))
                           `#(~form %))))]
    `(-> ~try
         ~@forms*)))

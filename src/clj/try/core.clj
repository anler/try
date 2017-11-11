(ns try.core
  (:refer-clojure :exclude [val sequence]
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
  "Retrieve the value of try no matter if it's a success or a
  failure."
  [try]
  (:value try))

(defn val-throw
  "Retrieve the value of try, but throw an exception if it's a failed
  Try. This is the same as dereferencing try with deref or @."
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

(defmethod clojure.pprint/simple-dispatch Try [^Try try]
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
  "Wrap each form in a map application and thread try through it.
  Equivalent to:
  (-> try
      (map first-form)
      (map second-form)
      ...)"
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
  "Wrap each form in a map-failure application and thread try through it.
  Equivalent to:
  (-> try
      (map-failure first-form)
      (map-failure second-form)
      ...)"
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
  "Apply value in ftry if it's a success to the values in all the
  try if all of them are also a success. If ftry is a failure it is
  returned, otherwise the first failed try is returned."
  [ftry & try]
  (if (success? ftry)
    (if (every? success? try)
      (succeed (clj-apply (val ftry) (clj-map val try)))
      (first (filter failure? try)))
    ftry))

(defn bind
  "Apply f to try's value if is successful and return its result,
  otherwise return try."
  [try f]
  (if (success? try)
    (f (val try))
    try))

(defmacro bind->
  "Wrap each form in a bind application and thread try through it.
  Equivalent to:
  (-> try
      (bind first-form)
      (bind second-form)
      ...)"
  [try & forms]
  (let [forms* (for [form forms]
                 `(bind ~(if (seq? form)
                           `#(~(first form) % ~@(next form))
                           `#(~form %))))]
    `(-> ~try
         ~@forms*)))

(defn sequence
  "Transform a collection coll of try items in a try where the value is
  a collection of each try value. If given an empty collection,
  returns a successful try with the empty vector as value."
  [coll]
  (clj-apply apply (succeed (partial conj [])) coll))

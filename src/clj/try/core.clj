(ns try.core
  (:refer-clojure :exclude [val sequence]
                  :rename {apply clj-apply
                           map   clj-map
                           into  clj-into})
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

(defmacro val-or
  "Retrieve the value of try if it's a success or evaluate and return
  default if it's a failure."
  [try default]
  `(if (success? ~try)
     (val ~try)
     ~default))

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

(defmacro try>
  "Wrap body in a try/catch that returns a successful Try
  if no exception is raised, or a failed Try if an exception
  is raised."
  [& body]
  `(try
     (let [v# (do ~@body)]
       (succeed v#))
     (catch Exception e#
       (fail e#))))

(defmacro try-let
  "Wrap a let with bindings and body in a tryc."
  [bindings & body]
  `(try>
     (let ~bindings
       ~@body)))

(defn map
  "Apply function f to try's value if is a successful Try."
  [f try]
  (if (success? try)
    (succeed (f (val try)))
    try))

(defn map-failure
  "Apply function f to try's value if is a failed Try."
  [f try]
  (if (success? try)
    try
    (fail (f (val try)))))

(defn bimap
  "Apply function f to try's value if it's a success, otherwise apply
  function g to try's value if it's a failure."
  [f g try]
  (map-failure g (map f try)))

(defn apply
  "Apply value in ftry if it's a success to the values in all the
  try if all of them are also a success. If ftry is a failure it is
  returned, otherwise the first failed try is returned.

  Examples:
  (apply (succeed +) (succeed 1) (succeed 2) (succeed 3))
  => #try.core.Try{:tag :try.core/success, :value 6}

  (apply (succeed +) (succeed 1) (fail 2) (fail 3))
  => #try.core.Try{:tag :try.core/failure, :value 2}"
  [ftry & try]
  (if (success? ftry)
    (if (every? success? try)
      (succeed (clj-apply (val ftry) (clj-map val try)))
      (first (filter failure? try)))
    ftry))

(defn bind
  "Apply f to try's value if is successful and return its result,
  otherwise return try.
  Examples:
  (bind (succeed 1)
        #(succeed (+ 2 %)))
  => #try.core.Try{:tag :try.core/success, :value 3}

  (bind (fail 1)
        #(succeed (+ 2 %)))
  => #try.core.Try{:tag :try.core/failure, :value 1}

  (bind (succeed 1)
        #(fail (str \"err\" %))
        #(succeed (+ 1 %)))
  => #try.core.Try{:tag :try.core/failure, :value \"err1\"}"
  [try & fs]
  (let [x (volatile! try)]
    (loop [[f & fs] fs]
      (when (and f (success? @x))
        (vreset! x (f @@x))
        (recur fs)))
    @x))

(defn sequence
  "Transform a collection coll of try items in a try where the value is
  a collection of each try value. If given an empty collection,
  returns a successful try with the empty vector as value."
  [coll]
  (clj-apply apply (succeed (partial conj [])) coll))

(defn into
  "If all items in coll are successful Try, return a successful Try
  which value is the to collection filled with the value of each Try
  in coll. Otherwise return a failed Try which value is the to
  collection filled with the value of each Try in coll.
  Examples:
  (into [] (list (succeed 1) (succeed 2) (succeed 3)))
  => #try.core.Try{:tag :try.core/success, :value [1 2 3]}

  (into [] [(succeed 1) (fail 2) (fail 3)])
  => #try.core.Try{:tag :try.core/failure, :value [2 3]}
  "
  [to coll]
  (if (empty? coll)
    (succeed to)
    (let [[successes failures] (partition-by success? coll)]
      (if (empty? failures)
        (succeed (clj-into to (clj-map val successes)))
        (fail (clj-into to (clj-map val failures)))))))

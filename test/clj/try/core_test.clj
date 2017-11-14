(ns try.core-test
  (:require [try.core :as try]
            [clojure.test :as t]))

(t/deftest succeed-test
  (t/testing "succeed wraps result in a successful Try."
    (t/is (= (:tag (try/succeed 1))
             :try.core/success))))

(t/deftest fail-test
  (t/testing "fail wraps result in a failed Try."
    (t/is (= (:tag (try/fail 1))
             :try.core/failure))))

(t/deftest success?-test
  (t/testing "success? returns true for a successful Try."
    (t/is (try/success? (try/succeed 1))))
  (t/testing "success? returns false for a failed Try."
    (t/is (not (try/success? (try/fail 1))))))

(t/deftest failure?-test
  (t/testing "failure? returns true for a failed Try."
    (t/is (try/failure? (try/fail 1))))
  (t/testing "failure? returns false for a successful Try."
    (t/is (not (try/failure? (try/succeed 1))))))

(t/deftest val-test
  (t/testing "val returns the value of a failed Try."
    (t/is (= :some-val
             (try/val (try/fail :some-val)))))
  (t/testing "val returns the value of a successful Try."
    (t/is (= :some-val
             (try/val (try/succeed :some-val))))))

(t/deftest val-or-test
  (t/testing "val-or returns the value of a successful Try."
    (t/is (= :some-val
             (try/val-or (try/succeed :some-val) :default-val))))
  (t/testing "val-or doesn't evaluate the default value if given a successful Try."
    (let [effect (volatile! nil)
          result (try/val-or (try/succeed :some-val) (do (reset! effect :executed)))]
      (t/is (= :some-val
               result))
      (t/is (nil? @effect))))
  (t/testing "val returns the default value if given a failed Try."
    (t/is (= :default-val
             (try/val-or (try/fail :some-val) :default-val)))))

(t/deftest val-throw-test
  (t/testing "val-throw returns the value of a successful Try."
    (t/is (= :some-val
             (try/val-throw (try/succeed :some-val)))))
  (t/testing "val-throw raises an ex-info with a failed Try with no Throwable value."
    (let [e (try (try/val-throw (try/fail :some-val))
                 (catch Exception e e))]
      (t/is (instance? clojure.lang.ExceptionInfo e))
      (t/is (= :some-val
               (:value (ex-data e))))))
  (t/testing "val-throw raises the original exception a failed Try contains."
    (let [e (try (try/val-throw (try/fail (IllegalArgumentException. "some exception")))
                 (catch Exception e e))]
      (t/is (instance? IllegalArgumentException e))
      (t/is (= "some exception"
               (.getMessage e))))))

(t/deftest deref-test
  (t/testing "deref does the same as val-throw"
    (with-redefs [try/val-throw (constantly "called")]
      (t/is (= "called"
               @(try/succeed :some-val))))))

(t/deftest try>-test
  (t/testing "try> returns a successful Try if no exception is thrown."
    (t/is (try/success? (try/try> :some-val))))
  (t/testing "tryc returns a failed Try if an exception is thrown."
    (t/is (try/failure? (try/try> (throw (Exception. "err")))))))

(t/deftest tryc-let-test
  (t/testing "try-let is like tryc but accepts bindings"
    (t/is (= :some-val
             @(try/try-let [v :some-val]
                           v))))
  (t/testing "try-let catches any exception thrown in the let."
    (t/is (try/failure?
            (try/try-let [v (throw (Exception. "err"))]
                         v)))))

(t/deftest map-test
  (t/testing "map applies f to a successful Try value."
    (t/is (= 4
             (try/val (try/map inc (try/succeed 3))))))
  (t/testing "map applies f to a successful Try value."
    (t/is (= 3
             (try/val (try/map inc (try/fail 3))))))
  (t/testing "map doens't catch exceptions throwns by f."
    (t/is (thrown? IllegalArgumentException
                   (try/map #(throw (IllegalArgumentException. (str %)))
                            (try/succeed 3))))))

(t/deftest map-failure-test
  (t/testing "map-failure applies f to a failed Try value."
    (t/is (= 3
             (try/val (try/map-failure inc (try/succeed 3))))))
  (t/testing "map-failure applies f to a successful Try value."
    (t/is (= 4
             (try/val (try/map-failure inc (try/fail 3))))))
  (t/testing "map-failure doesn't catch exceptions throwns by f."
    (t/is (thrown? IllegalArgumentException
                   (try/map-failure #(throw (IllegalArgumentException. (str %)))
                                    (try/fail 3))))))

(t/deftest bimap-test
  (t/testing "bimap applies f to a successful Try value."
    (t/is (= 4
             (try/val (try/bimap inc
                                 dec
                                 (try/succeed 3))))))
  (t/testing "bimap applies g to a failed Try value."
    (t/is (= 2
             (try/val (try/bimap inc
                                 dec
                                 (try/fail 3))))))
  (t/testing "bimap doesn't catch exceptions throwns by f."
    (t/is (thrown? IllegalArgumentException
                   (try/bimap #(throw (IllegalArgumentException. (str %)))
                              identity
                              (try/succeed 3)))))
  (t/testing "bimap doesn't catch exceptions throwns by g."
    (t/is (thrown? IllegalArgumentException
                   (try/bimap identity
                              #(throw (IllegalArgumentException. (str %)))
                              (try/fail 3))))))

(t/deftest apply-test
  (t/testing "apply applies f inside a successful Try, to a successful Try value."
    (t/is (= 4
             (try/val (try/apply (try/succeed inc)
                                 (try/succeed 3))))))
  (t/testing "apply applies f inside a successful Try, to all the successful Try values."
    (t/is (= 9
             (try/val (try/apply (try/succeed +)
                                 (try/succeed 3)
                                 (try/succeed 3)
                                 (try/succeed 3))))))
  (t/testing "apply returns the failed Try with the f if applied to a successful Try value."
    (t/is (= inc
             (try/val (try/apply (try/fail inc)
                                 (try/succeed 3))))))

  (t/testing "apply returns the first failed Try to which the successful Try with if is applied."
    (t/is (= 3
             (try/val (try/apply (try/succeed inc)
                                 (try/succeed 1)
                                 (try/succeed 2)
                                 (try/fail 3)))))))

(t/deftest bind-test
  (t/testing "bind applies each f while they return a successful Try."
    (t/is (= 5
             (try/val (try/bind (try/succeed 3)
                                (comp try/succeed inc)
                                (comp try/succeed inc))))))
  (t/testing "bind doesn't apply an f to a failed Try returned by one f before."
    (t/is (= 3
             (try/val (try/bind (try/fail 3)
                                (comp try/fail inc)
                                (comp try/succeed inc)))))))

(t/deftest sequence-test
  (t/testing "sequence transform a collection of successes into a success of collection."
    (t/is (= [1 2 3]
             (try/val (try/sequence [(try/succeed 1)
                                     (try/succeed 2)
                                     (try/succeed 3)])))))
  (t/testing "sequence returns the first failure in the collection"
    (t/is (= 2
             (try/val (try/sequence [(try/succeed 1)
                                     (try/fail 2)
                                     (try/fail 3)])))))
  (t/testing "sequence returns the empty vector if the collection is empty"
    (t/is (= []
             (try/val (try/sequence []))))))

(t/deftest into-test
  (t/testing "into transform a collection of successes into a success of collection."
    (t/is (= {:one 1, :two 2}
             (try/val (try/into {} [(try/succeed [:one 1])
                                    (try/succeed [:two 2])])))))
  (t/testing "into transform a collection with at least one failure into a failure of collection of failure values."
    (t/is (= {:one 1, :two 2}
             (try/val (try/into {} [(try/succeed 1)
                                    (try/fail [:one 1])
                                    (try/fail [:two 2])])))))
  (t/testing "into returns the given collection if the collection of trys is empty"
    (t/is (= []
             (try/val (try/into [] []))))
    (t/is (= {}
             (try/val (try/into {} []))))))

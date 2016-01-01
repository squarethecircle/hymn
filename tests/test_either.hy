;;; -*- coding: utf-8 -*-
;;; Copyright (c) 2014-2016, Philip Xu <pyx@xrefactor.com>
;;; License: BSD New, see LICENSE for details.

(import
  [hymn.operations [sequence]]
  [hymn.types [either :as either-module]]
  [hymn.types.either
    [either-m Left Right ->either to-either left? right? either failsafe]])

(require hymn.types.either)
(require hymn.operations)

(def data 42)

(defn test-reader-macro-failsafe []
  "failsafe reader macro | should wrap a function with decorator failsafe"
  (def failsafe-int #|int)
  (assert (instance? either-m (failsafe-int 1)))
  (assert (= (failsafe-int 1) (either-m.unit 1))))

(defn test-module-level-unit []
  "either module should have a working module level unit function"
  (assert (instance? either-m (either-module.unit nil))))

(defn test-module-level-zero []
  "either module should have a module level zero"
  (assert (instance? either-m either-module.zero)))

(defn test-zero-is-left []
  "zero of either monad should be a left"
  (assert (instance? Left either-module.zero))
  (assert (instance? Left either-m.zero)))

(defn test-compare []
  "compare between either"
  (assert (= (Left data) (Left data)))
  (assert (= (Right data) (Right data)))
  (assert (!= (Left data) (Right data))))

(defn test-ordering []
  "ordering logic of either monad"
  (assert (is false (> (Left data) (Left data))))
  (assert (> (Left data) (Left (dec data))))
  (assert (< (Left data) (Left (inc data))))
  (assert (is false (> (Right data) (Right data))))
  (assert (> (Right data) (Right (dec data))))
  (assert (< (Right data) (Right (inc data))))
  ;; left is less then right
  (assert (< (Left data) (Right data)))
  ;; even the value inside is bigger, still
  (assert (< (Left (inc data)) (Right data))))

(defn test-boolean []
  "Left is falsy and Right is truthy"
  (assert (is false (bool (Left nil))))
  (assert (is true (bool (Right nil)))))

(defn test-from-value []
  "from-value will return Left for anything false, Right otherwise"
  (assert (instance? Left (either-m.from-value nil)))
  (assert (instance? Left (either-m.from-value 0)))
  (assert (instance? Left (either-m.from-value "")))
  (assert (instance? Left (either-m.from-value [])))
  (assert (instance? Left (either-m.from-value {})))
  (assert (either-m.from-value 1))
  (assert (either-m.from-value (object)))
  (assert (either-m.from-value [42])))

(defn test-to-either []
  "to-either and ->either work as from-value"
  (assert (= (to-either nil) (either-m.from-value nil)))
  (assert (= (to-either 1) (either-m.from-value 1)))
  (assert (= (->either nil) (either-m.from-value nil)))
  (assert (= (->either 1) (either-m.from-value 1))))

(defn test-is-left []
  "left? testing for Left"
  (assert (left? (Left nil)))
  (assert (not (left? (Right nil)))))

(defn test-is-right []
  "right? testing for Right"
  (assert (right? (Right nil)))
  (assert (not (right? (Left nil)))))

(defn test-failsafe-decorator []
  "failsafe decorator make function return Left when exception is raised"
  (with-decorator failsafe
    (defn safe-div [a b] (/ a b)))
  (assert (left? (safe-div 1 0)))
  (assert (= (Right 2) (safe-div 4 2)))
  (def safe-int (failsafe int))
  (assert (= (Right 42) (safe-int "42")))
  (assert (left? (safe-int "this is no a number"))))

(defn test-do-monad-either []
  "either computation with do-monad"
  (assert (= (Right 3) (do-monad [a (Right 1) b (Right 2)] (+ a b))))
  (assert (left? (do-monad [a (Left 1) b (Right 0)] (/ a b))))
  (assert (left? (do-monad
                   [a (Right 1)
                    b (Right 0)
                    :when (not (zero? b))]
                    (/ a b)))))

(defn test-either-monadplus []
  "either string is monadplus"
  (assert (= (Right [1 2 3]) (sequence [(Right 1) (Right 2) (Right 3)])))
  (assert (left? (sequence [(Right 1) (Left 2) (Right 3)])))
  (assert (= (Right 1) (+ (Right 1) (Right 2))))
  (assert (= (Right 2) (+ (Left 1) (Right 2))))
  (assert (= (Right 1) (+ (Right 1) (Left 2))))
  (assert (left? (+ (Left 1) (Left 2))))
  (assert (= (Left 2) (+ (Left 1) (Left 2)))))

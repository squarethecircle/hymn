;;; -*- coding: utf-8 -*-
;;; Copyright (c) 2014-2016, Philip Xu <pyx@xrefactor.com>
;;; License: BSD New, see LICENSE for details.

(import
  [hymn.operations [sequence]]
  [hymn.types [maybe :as maybe-module]]
  [hymn.types.maybe [maybe-m Just Nothing nothing? maybe]])

(require hymn.types.maybe)
(require hymn.operations)

(defn test-reader-macro-maybe []
  "maybe reader macro ? should wrap a function with decorator maybe"
  (def maybe-int #?int)
  (assert (instance? maybe-m (maybe-int 1)))
  (assert (= (maybe-int 1) (maybe-m.unit 1))))

(defn test-module-level-unit []
  "maybe module should have a working module level unit function"
  (assert (instance? maybe-m (maybe-module.unit nil))))

(defn test-module-level-zero []
  "maybe module should have a module level zero"
  (assert (instance? maybe-m maybe-module.zero)))

(defn test-module-level-from-maybe []
  "maybe module should have a module level from-maybe"
  (assert (= 1 (maybe-module.from-maybe (Just 1) 2)))
  (assert (= 2 (maybe-module.from-maybe Nothing 2)))
  (assert (= 1 (maybe-module.<-maybe (Just 1) 2))
  (assert (= 2 (maybe-module.<-maybe Nothing 2)))))

(defn test-module-level-to-maybe []
  "maybe module should have a module level to-maybe"
  (assert (= (Just 1) (maybe-module.to-maybe 1)))
  (assert (= (Just 1) (maybe-module.->maybe 1)))
  (assert (is Nothing (maybe-module.to-maybe nil)))
  (assert (is Nothing (maybe-module.->maybe nil))))

(defn test-zero-is-nothing []
  "zero of maybe monad should be Nothing"
  (assert (is maybe-module.zero Nothing))
  (assert (is maybe-m.zero Nothing)))

(defn test-type []
  "Nothing and Just are of maybe monad"
  (assert (instance? maybe-m Nothing))
  (assert (instance? maybe-m (Just nil)))
  (assert (not (instance? Just Nothing))))

(defn test-compare []
  "compare between maybe"
  (assert (= Nothing Nothing))
  (assert (= (Just 42) (Just 42)))
  (assert (!= (Just nil) Nothing))
  (assert (!= Nothing (Just nil)))
  (assert (is Nothing Nothing)))

(defn test-ordering []
  "ordering logic of maybe monad"
  (assert (is false (> Nothing Nothing)))
  (assert (is false (< Nothing Nothing)))
  (assert (is false (> Nothing (Just nil))))
  (assert (< Nothing (Just nil)))
  (assert (> (Just nil) Nothing))
  (assert (> (Just 1) (Just 0)))
  (assert (>= (Just 1) (Just 1)))
  (assert (= (Just 1) (Just 1))))

(defn test-boolean []
  "Nothing is falsy and Just is truthy"
  (assert (is false (bool Nothing)))
  (assert (is true (bool (Just nil)))))

(defn test-from-value []
  "from-value will return Nothing for anything false, Just otherwise"
  (assert (is Nothing (maybe-m.from-value nil)))
  (assert (is Nothing (maybe-m.from-value 0)))
  (assert (is Nothing (maybe-m.from-value "")))
  (assert (is Nothing (maybe-m.from-value [])))
  (assert (is Nothing (maybe-m.from-value {})))
  (assert (instance? Just (maybe-m.from-value 1)))
  (assert (instance? Just (maybe-m.from-value (object))))
  (assert (instance? Just (maybe-m.from-value [42]))))

(defn test-from-maybe []
  "from-maybe should get the value from Just and return default otherwise"
  (assert (= 1 (.from-maybe (Just 1) nil)))
  (assert (= nil (.from-maybe (Just nil) 1)))
  (assert (= 1 (.from-maybe Nothing 1)))
  (assert (= nil (.from-maybe Nothing nil))))

(defn test-is-nothing []
  "nothing? testing for Nothing"
  (assert (nothing? Nothing))
  (assert (not (nothing? (Just nil)))))

(defn test-maybe-decorator []
  "maybe decorator make function return Nothing when exception is raised"
  (with-decorator maybe
    (defn safe-div [a b] (/ a b)))
  (assert (is Nothing (safe-div 1 0)))
  (assert (= (Just 2) (safe-div 4 2)))
  (def safe-int (maybe int))
  (assert (= (Just 42) (safe-int "42")))
  (assert (is Nothing (safe-int "this is no a number"))))

(defn test-do-monad-maybe []
  "maybe computation with do-monad"
  (assert (= (Just 3) (do-monad [a (Just 1) b (Just 2)] (+ a b)))))

(defn test-do-monad-maybe-when []
  "maybe is monadplus, :when can be used in do-monad"
  (assert (is Nothing
            (do-monad
              [a (Just 1)
               b (Just 0)
              :when (not (zero? b))]
              (/ a b)))))

(defn test-maybe-monadplus []
  "maybe is monadplus"
  (assert (= (Just [1 2 3]) (sequence [(Just 1) (Just 2) (Just 3)])))
  (assert (is Nothing (sequence [(Just 1) Nothing (Just 3)])))
  (assert (= (Just 1) (+ (Just 1) (Just 2))))
  (assert (= (Just 1) (+ Nothing (Just 1))))
  (assert (= (Just 1) (+ (Just 1) Nothing)))
  (assert (is Nothing (+ Nothing Nothing))))

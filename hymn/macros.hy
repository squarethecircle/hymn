;;; -*- coding: utf-8 -*-
;;; Copyright (c) 2014-2018, Philip Xu <pyx@xrefactor.com>
;;; License: BSD New, see LICENSE for details.
"hymn.macros - monad operations implemented as macros"

(import
  [hymn.utils [thread-first thread-last thread-bindings]])
(import [hy [HySymbol HyList HyString]])

;;; lift tag macro, e.g. #^ + => (lift +)
(deftag ^ [f]
  (with-gensyms [lift]
    `(do (import [hymn.operations [lift :as ~lift]]) (~lift ~f))))

;;; monad return tag macro, replaced by 'm-return, used in do-monad,
;;; e.g.
;;; (do-monad [a (Just 1) b #= (inc a)] #= [a b])
;;; is equivalent to
;;; (do-monad [a (Just 1) b (m-return (inc c))] (m-return [a b])
(deftag = [expr] `(m-return ~expr))

(defmacro do-monad [binding-forms expr]
  "macro for sequencing monadic computations, a.k.a do notation in haskell"
  (when (odd? (len binding-forms))
    (macro-error None "do-monad binding forms must come in pairs"))
  (setv iterator (iter binding-forms))
  (setv bindings (-> (zip iterator iterator) list reversed list))
  (unless (len bindings)
    (macro-error None "do-monad must have at least one binding form"))
  (defn bind-action [mexpr binding-expr]
    (setv [binding expr] binding-expr)
    (if
      (= binding :when) `(if ~expr ~mexpr (. (m-return None) zero))
      (= binding :let) `(do (setv ~@expr) ~mexpr)
      (with-gensyms [monad]
        `(do
           (setv ~monad ~expr)
           (>> ~monad (fn [~binding &optional [m-return (. ~monad unit)]]
                        ~mexpr))))))
  (reduce bind-action bindings expr))

(defmacro applicative-do-monad [binding-forms expr]
  "macro for sequencing monadic computations, a.k.a do notation in haskell"
  (when (odd? (len binding-forms))
    (macro-error None "do-monad binding forms must come in pairs"))
  (setv iterator (iter binding-forms))
  (setv bindings (-> (zip iterator iterator) list))
  (setv binding-names (HyList (map (comp HySymbol first) bindings)))
  (setv fn-expr `(fn ~binding-names ~expr))
  (unless (len bindings)
    (macro-error None "do-monad must have at least one binding form"))
  (defn bind-action [mexpr binding-expr]
    (setv [binding expr] binding-expr)
    (if
      (with-gensyms [ronad bit]
        `(do
           (setv ~ronad ~expr)
           (setv ~bit ~mexpr)
           (if (not (instance? Monad ~bit)) (setv ~bit ((. ~ronad unit) ~bit)))
           ((. ~bit ap) ~ronad ~(HyString (mangle binding)))))))
  `(do (import [hymn.types.monad [Monad]])
    ~(reduce bind-action bindings fn-expr))
)

(defmacro do-monad-return [binding-forms expr]
  "macro for sequencing monadic computations, with automatic return"
  `(do (require hymn.macros)
     (hymn.macros.do-monad ~binding-forms (m-return ~expr))))

(defmacro do-monad-with [monad binding-forms expr]
  "macro for sequencing monadic composition, with said monad as default"
  `(do (require hymn.macros)
     (hymn.macros.with-monad ~monad
       (hymn.macros.do-monad-return ~binding-forms ~expr))))

(defmacro monad-> [init-value &rest actions]
  "threading macro for monad"
  (setv bindings (list (thread-bindings thread-first init-value actions)))
  `(do (require hymn.macros)
    (hymn.macros.do-monad
      [~@(butlast bindings)]
      ~(last bindings))))

(defmacro monad->> [init-value &rest actions]
  "threading tail macro for monad"
  (setv bindings (list (thread-bindings thread-last init-value actions)))
  `(do (require hymn.macros)
     (hymn.macros.do-monad
       [~@(butlast bindings)]
       ~(last bindings))))

(defmacro m-for [n-seq &rest mexpr]
  "macro for sequencing monadic actions"
  (setv [n seq] n-seq)
  (with-gensyms [m-map]
    `(do (import [hymn.operations [m-map :as ~m-map]])
       (~m-map (fn [~n] ~@mexpr) ~seq))))

(defmacro m-when [test mexpr]
  "conditional execution of monadic expressions"
  `(if ~test ~mexpr (m-return None)))

(defmacro monad-comp [expr bindings &optional condition]
  "different syntax for do notation"
  (setv guard (if (none? condition) `() `(:when ~condition)))
  `(do (require hymn.macros)
     (hymn.macros.do-monad-return ~(+ bindings guard) ~expr)))

(defmacro with-monad [monad &rest exprs]
  "provide default function m-return as the unit of the monad"
  `(do (setv m-return (. ~monad unit)) ~@exprs))

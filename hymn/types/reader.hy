;;; -*- coding: utf-8 -*-
;;; Copyright (c) 2014-2018, Philip Xu <pyx@xrefactor.com>
;;; License: BSD New, see LICENSE for details.
"hymn.types.reader - the reader monad"

(import
  [operator [itemgetter]]
  [hymn.types.monad [Monad]])

(import [toolz [curry]])

(defclass Reader [Monad]
  "the reader monad

  computations which read values from a shared environment"
  (defn --init-- [self value &optional [deps #{}]] 
    (setv self.value value)
    (setv self.deps deps)
  )

  (defn --repr-- [self]
    (.format "{}({})" (. (type self) --name--) self.value.--name--))

  (defn ap [self f &optional [arg None]]
    "the ap operation of :class:`Reader`"
    (if arg 
      ((type self) (fn [e] ((curry (.run self e)) #** {arg (.run f e)}))
        :deps (| self.deps f.deps) )
      ((type self) (fn [e] ((curry (.run self e)) (.run f e))) 
        :deps (| self.deps f.deps) )))

  (defn bind [self f]
    "the bind operation of :class:`Reader`"
    ((type self) (fn [e] (.run (f (.run self e)) e))))

  (with-decorator classmethod
    (defn unit [cls value]
      "the unit of reader monad"
      (cls (constantly value))))

  (defn local [self f]
    "return a reader that execute computation in modified environment"
    ((type self) (fn [e] (.run self (f e)))))

  (defn run [self e]
    "run the reader and extract the final vaule"
    (self.value e)))

;;; alias
(setv reader-m Reader)
(setv run Reader.run)
(setv unit Reader.unit)

(defn asks [f &optional [deps #{}]]
  "create a simple reader action from :code:`f`"
  (Reader f :deps deps))

(setv reader asks)

(setv ask (reader identity))

(defn local [f]
  "executes a computation in a modified environment, :code:`f :: e -> e`"
  (fn [m] (m.local f)))

(defn lookup [key]
  "create a lookup reader of :code:`key` in the environment"
  (reader (itemgetter key) :deps #{key}))
(setv <- lookup)

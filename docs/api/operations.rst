Monad Operations
================

.. automodule:: hymn.operations

:code:`hymn.operations` provide operations for monad computations


.. autofunction:: k_compose
.. function:: <=<

  alias of :func:`k_compose`

.. code-block:: clojure

  => (import [hymn.operations [k-compose <=<]])
  => (import [hymn.types.maybe [Just Nothing]])
  => (defn m-double [x] (if (numeric? x) (Just (* x 2)) Nothing))
  => (defn m-inc [x] (if (numeric? x) (Just (inc x)) Nothing))
  => (setv +1*2 (k-compose m-double m-inc))
  => (+1*2 1)
  Just(4)
  => (setv *2+1 (<=< m-inc m-double))
  => (*2+1 2)
  Just(5)
  => (*2+1 "two")
  Nothing

.. autofunction:: k_pipe
.. function:: >=>

  alias of :func:`k_pipe`

.. code-block:: clojure

  => (import [hymn.operations [k-pipe >=>]])
  => (import [hymn.types.maybe [Just Nothing maybe]])
  => (setv m-int (maybe int))
  => (defn m-array [n] (if (> n 0) (Just (* [0] n)) Nothing))
  => (setv make-array (k-pipe m-int m-array))
  => (make-array 0)
  Nothing
  => (make-array 3)
  Just([0, 0, 0])
  => (setv make-array (>=> m-int m-array))
  => (make-array 2)
  Just([0, 0])

.. autofunction:: lift

.. code-block:: clojure

  => (import [hymn.operations [lift]])
  => (import [hymn.types.maybe [Just]])
  => (setv m+ (lift +))
  => (m+ (Just 1) (Just 2))
  Just(3)

.. autofunction:: m_map
.. function:: m-map

  alias of :func:`m_map`

.. code-block:: clojure

  => (import [hymn.operations [m-map]])
  => (import [hymn.types.maybe [maybe-m]])
  => (m-map maybe-m.unit (range 5))
  Just([0, 1, 2, 3, 4])
  => (m-map (maybe-m.monadic inc) (range 5))
  Just([1, 2, 3, 4, 5])
  => (import [hymn.types.writer [tell]])
  => (.execute (m-map tell (range 1 101)))
  5050

.. autofunction:: replicate

.. code-block:: clojure

  => (import [hymn.operations [replicate]])
  => (import [hymn.types.list [list-m]])
  => (list (replicate 2 (list-m [0 1])))
  [[0, 0], [0, 1], [1, 0], [1, 1]]

.. autofunction:: sequence

.. code-block:: clojure

  => (import [hymn.operations [sequence]])
  => (import [hymn.types.writer [tell]])
  => (.execute (sequence (map tell (range 1 101))))
  5050

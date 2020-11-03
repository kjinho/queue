;;;; package.lisp

(defpackage #:queue
  (:use #:cl)
  (:export
   :queue
   :q-length
   :enqueue!
   :dequeue!
   :queue->list
   :math-queue
   :sum
   :q-reduce
   :q-max
   :q-min
   :stdev
   ))

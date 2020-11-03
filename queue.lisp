;;;; queue.lisp

(in-package #:queue)

(defclass queue ()
  ((head :initform -1)
   (tail :initform -1)
   (size :initarg :size :initform 50)
   (type :initarg :type :initform 'float)
   values))

(defmethod initialize-instance :after ((queue queue) &key)
  (with-slots (values size type) queue
    (setf values (make-array size :element-type type))))

(defgeneric q-length (queue)
  (:documentation "Returns the length of the queue. O(1)."))

(defgeneric queue->list (queue)
  (:documentation "Returns a new list containing the values in queue. O(n)."))

(defgeneric enqueue! (queue item)
  (:documentation "Add item to the back of the queue. O(1)."))

(defgeneric dequeue! (queue)
  (:documentation "Remove and return item from the front of the queue. O(1)."))

(defgeneric reset-queue-counters! (queue)
  (:documentation "Helper function that resets head and tail to sensible numbers"))

(defmethod reset-queue-counters! ((queue queue))
  (with-slots (head tail size) queue
    (when (>= head size)
      (decf head size)
      (decf tail size)
      (reset-queue-counters! queue))))

(defmethod enqueue! ((queue queue) item)
  (reset-queue-counters! queue)
  (with-slots (tail head values size type) queue
    (cond ((= tail -1) ; new queue
           (progn
             (setf head 0)
             (setf tail 0)
             (setf (aref values tail) (coerce item type))
             queue))
          ((>= (- tail head)
               (- size 1)) ; queue already full
           (error "Queue not big enough to enqueue again"))
          (t
           (progn
             (incf tail)
             (setf (aref values (mod tail size)) (coerce item type))
             queue)))))

(defmethod dequeue! ((queue queue))
  (reset-queue-counters! queue)
  (with-slots (tail head size values) queue
    (cond ((= tail -1) ; new queue
           (error "Can't dequeue an empty queue"))
          ((= head tail)
           (prog1
               (aref values
                     (mod head size))
             (setf head -1)
             (setf tail -1)))
          (t
           (prog1
               (aref values
                     (mod head size))
             (incf head))))))

(defmethod q-length ((queue queue))
  (with-slots (size tail head) queue
    (if (< tail 0)
        0
        (+ 1 (mod (+ size tail (- head)) size)))))

(defmethod queue->list ((queue queue))
  (with-slots (head tail size values) queue
    (if (< tail 0)
        nil
        (loop for i from head to tail
              collect (aref values (mod i size))))))

(defgeneric q-reduce (fn queue &key initial-value)
  (:documentation "reduce over the queue"))

(defun q-reduce-helper (fn vector tail size initial-value start-index)
  (if (> start-index tail)
      initial-value
      (q-reduce-helper fn
                       vector
                       tail
                       size
                       (funcall fn
                                initial-value
                                (elt vector (mod start-index size)))
                       (1+ start-index))))

(defmethod q-reduce (fn (queue queue) &key (initial-value :none))
  (with-slots (head tail size values) queue
    (if (eql initial-value :none)
        (q-reduce-helper fn values tail size
                         (elt values (mod head size))
                         (1+ head))
        (q-reduce-helper fn values tail size initial-value head))))

    
(defclass math-queue (queue)
  ((sum :initform 0
        :accessor sum)))

(defmethod initialize-instance :after ((queue math-queue) &key)
  (with-slots (sum type) queue
    (setf sum (coerce sum type))))

(defmethod enqueue! :after ((queue math-queue) item)
  (with-slots (sum) queue
    (incf sum item)))

(defmethod dequeue! :around ((queue math-queue))
  (with-slots (sum) queue
    (let ((popped-value (call-next-method)))
      (decf sum popped-value)
      popped-value)))

(defgeneric q-max (math-queue)
  (:documentation "Returns the maximum value in the queue."))
(defgeneric q-min (math-queue)
  (:documentation "Returns the minimum value in the queue."))
(defgeneric stdev (math-queue)
  (:documentation "Returns the standard deviation of the queue."))

(defmethod q-max ((q math-queue))
  (q-reduce (lambda (x y) (if (> x y) x y)) q))

(defmethod q-min ((q math-queue))
  (q-reduce (lambda (x y) (if (< x y) x y)) q))

(defmethod stdev ((q math-queue))
  (let* ((len (q-length q))
         (avg (/ (sum q) len)))
    (sqrt (/ (q-reduce
              (lambda (acc x) (+ acc (expt (- x avg) 2)))
              q
              :initial-value 0)
             (- len 1)))))

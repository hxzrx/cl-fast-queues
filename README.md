## About

cl-fast-queues implements arrays based, optimized unbounded LIFO and FIFO queues for both unsafe and safe accessing. It uses arrays to implement both FIFO queue and LIFO queue, and extends the queues with new longer arrays before overflowing.

Both FIFO and LIFO queues have unsafe and safe version, so the latter implements thread safe apis for basic operations of queues such as enqueue and dequeue, and he underground queues are made by cl-speedy-queue (something modified with vanilla cl-speedy-queue and it comes in speedy-queue.lisp together with this lib) and cl-speedy-lifo according.

## Date Types

### unsafe-fast-fifo

The struct of unsafe-fifo, unsafe among threads.

#### slots

##### push-queue
An array create by cl-speedy-queue:make-queue, which is the container that new items push into.

##### pop-queue
an array create by `cl-speedy-queue:make-queue`, which is the container that old items pop out.

##### queue-list
A list of queues create by `cl-speedy-queue:make-queue`, this list will contain at least one array, when the length of queue-list is one, push-queue eq to pop-queue.

##### enlarge-size
Before  push-queue overflows, another array should create whose length is `(* push-queue-len push-queue-len)`.
One expects new array will be longer, so enlarge-size should be at least 1.
The default value is 1.5.

### unsafe-fast-lifo

The struct of unsafe-lifo, unsafe among threads.

#### slots
##### cur-queue
An array create by cl-speedy-lifo:make-queue, which is the container that new items push into and pops out.

##### queue-list
A list of queues create by `cl-speedy-queue:make-queue`, this list will contain at least one array, when the length of queue-list is one, push-queue eq to pop-queue.

##### enlarge-size
Before  cur-queue overflows, another array should create whose length is `(* push-queue-len push-queue-len)`.
One expects new array will be longer, so enlarge-size should be at least 1.
The default value is 1.5.

### safe-fast-fifo

The struct of safe-fifo  that can be safely accessed among threads.

#### slots

##### push-queue
An array create by cl-speedy-queue:make-queue, which is the container that new items push into.

##### pop-queue
an array create by `cl-speedy-queue:make-queue`, which is the container that old items pop out.

##### queue-list
A list of queues create by `cl-speedy-queue:make-queue`, this list will contain at least one array, when the length of queue-list is one, push-queue eq to pop-queue.

##### enlarge-size
Before  push-queue overflows, another array should create whose length is `(* push-queue-len push-queue-len)`.
One expects new array will be longer, so enlarge-size should be at least 1.
The default value is 1.5.

##### lock
A lock created with Bordeaux Threads which is used to protect accessing the slots.

##### cvar
A condition variable created with Bordeaux Threads which is used in enqueue and dequeue.

### safe-fast-lifo

The struct of safe-lifo that can be safely accessed among threads.

#### slots
##### cur-queue
An array create by cl-speedy-lifo:make-queue, which is the container that new items push into and pops out.

##### queue-list
A list of queues create by `cl-speedy-queue:make-queue`, this list will contain at least one array, when the length of queue-list is one, push-queue eq to pop-queue.

##### enlarge-size
Before  cur-queue overflows, another array should create whose length is `(* push-queue-len push-queue-len)`.
One expects new array will be longer, so enlarge-size should be at least 1.
The default value is 1.5.

##### lock
A lock created with Bordeaux Threads which is used to protect accessing the slots.

##### cvar
A condition variable created with Bordeaux Threads which is used in enqueue and dequeue.


## APIs
### function `make-unsafe-fifo (&key (init-length 1000) (enlarge-size 1.5) (enlarge-threshold 1.0)`
Creates an instance of unsafe-fast-fifo.

### function `make-unsafe-lifo (&key (init-length 1000) (enlarge-size 1.5) (enlarge-threshold 1.0)`
Creates an instance of unsafe-fast-lifo.

### function `make-safe-fifo (&key (init-length 1000) (enlarge-size 1.5) (enlarge-threshold 1.0)`
Creates an instance of safe-fast-fifo.

### function `make-safe-lifo (&key (init-length 1000) (enlarge-size 1.5) (enlarge-threshold 1.0)`
Creates an instance of safe-fast-lifo.

### method `enqueue (object queue)`
Push `object` into `queue`, return the object itself.
The `queue` should be an instance of any one of the queue structs above.

### method `dequeue (queue &key (keep-in-queue-p t) waitp)`
Pop an object from `queue`, return the object itself.

If `keep-in-queue-p` is NIL, the value in the place that's just popped will be set to NIL, or else the value will not be changed.

`waitp`takes effect only when `queue` is an instance of `safe-fifo` or `safe-lifo`.
When waitp is NIL, and the `queue` is empty, calling this method will return the special symbol `*underflow-flag*` instantly without waiting or signaling a condition.
When waitp is T, and the `queue` is empty, call this method will wait for the condition variable's notification and thus the call blocks until a notification is received.

### method `queue-peek (queue)`
Returns the next object that would be dequeued without dequeueing it.

### method `queue-empty-p (queue)`
Returns NIL if there is at least one object in the queue.

### method `queue-count (queue)`
Return the count of the objects can be dequeued in the queue.

### `*overflow-flag*`
An special symbol return by enqueue if the queue meets its upper limit.
If this symbol return in your code, please issue a bug.

### `*underflow-flag*`
An special symbol return by dequeue if the queue is empty.
If this symbol return in your code and the queue is an instance of safe-fifo or safe-lifo, please issue a bug.

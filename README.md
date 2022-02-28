## About
cl-fast-queues implements arrays based, optimized unbounded LIFO and FIFO queues for both unsafe and safe accessing. It uses arrays to implement both FIFO queue and LIFO queue, and extends the queues with new longer arrays before overflowing.

Both FIFO and LIFO queues have unsafe and safe version, so the latter implements thread safe apis for basic operations of queues such as enqueue and dequeue, and he underground queues are made by cl-speedy-queue (something modified with vanilla cl-speedy-queue and it comes in speedy-queue.lisp together with this library) and cl-speedy-lifo according.

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

### unsafe-fast-lifo
The struct of unsafe-lifo, unsafe among threads.
#### slots
##### cur-queue
An array create by cl-speedy-lifo:make-queue, which is the container that new items push into and pops out.

##### queue-list
A list of queues create by `cl-speedy-queue:make-queue`, this list will contain at least one array, when the length of queue-list is one, push-queue eq to pop-queue.

### safe-fast-fifo
The struct of safe-fifo  that can be safely accessed among threads.

#### slots
##### push-queue
An array create by cl-speedy-queue:make-queue, which is the container that new items push into.

##### pop-queue
an array create by `cl-speedy-queue:make-queue`, which is the container that old items pop out.

##### queue-list
A list of queues create by `cl-speedy-queue:make-queue`, this list will contain at least one array, when the length of queue-list is one, push-queue eq to pop-queue.

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

##### lock
A lock created with Bordeaux Threads which is used to protect accessing the slots.

##### cvar
A condition variable created with Bordeaux Threads which is used in enqueue and dequeue.

## APIs
### function `make-unsafe-fifo (&optional (init-length 1000)`
Creates an instance of unsafe-fast-fifo.

### function `make-unsafe-lifo (&optional (init-length 1000)`
Creates an instance of unsafe-fast-lifo.

### function `make-safe-fifo (&optional (init-length 1000)`
Creates an instance of safe-fast-fifo.

### function `make-safe-lifo (&optional (init-length 1000)`
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

### method `queue-find (item (queue unsafe-fast-fifo) &key (key #'identity) (test #'eql))`
If `item' has been found in `queue', return the item itself, or else return nil.
So if `item' is nil, the returned value will be nil whatever.

### method `queue-to-list (queue)`
Return a list of items those have been enqueued.
To make the returned list have the same popping order,
the order of the returned list is the same as queue order for the FIFO queues,
,and the order of the returned list is the reverse of the enqueue order for LIFO queues.

### method list-to-queue (list queue-type)
Make a queue, then enque the items in the list from left to right.
`queue-type' is a keyword which should be one of `'(:unsafe-fifo :unsafe-lifo :safe-fifo :safe-lifo)`.

### `*overflow-flag*`
An special symbol return by enqueue if the queue meets its upper limit.
If this symbol return in your code, please issue a bug.

### `*underflow-flag*`
An special symbol return by dequeue if the queue is empty.
If this symbol return in your code and the queue is an instance of safe-fifo or safe-lifo, please issue a bug.

### `*enlarge-size*`
When the queue is full, another array will be created whose length is `*enlarge-size*` times longer than the current longest array.
The default value is 1.5.

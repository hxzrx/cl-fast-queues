## About
cl-fast-queues implements arrays based, optimized unbounded LIFO and FIFO queues for both unsafe and safe accessing. It uses arrays to implement both FIFO queue and LIFO queue, and extends the queues with new longer arrays before overflowing.

Both FIFO and LIFO queues have unsafe and safe version, so the latter implements thread safe apis for basic operations of queues such as enqueue and dequeue, and he underground queues are made by cl-speedy-queue (something modified with vanilla cl-speedy-queue and it comes in speedy-queue.lisp together with this library) and cl-speedy-lifo according.

This library has been intensively tested and has showed that it has a competitive performance.

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

##### waitp
If waitp is set to T, the dequeue operation will wait if the queue is currently empty until a new item enqueued and notified.
If waitp is set to NIL,the dequeue operation will not wait and return *underflow-flag* instantly if the queue is currently empty.
The value of waitp is very important for the performance. DO NOT set it to T unless you do want to wait for a significant value in threads.
The default value of waitp is T.

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

##### waitp
If waitp is set to T, the dequeue operation will wait if the queue is currently empty until a new item enqueued and notified.
If waitp is set to NIL,the dequeue operation will not wait and return *underflow-flag* instantly if the queue is currently empty.
The value of waitp is very important for the performance. DO NOT set it to T unless you do want to wait for a significant value in threads.
The default value of waitp is T.

## APIs
### function `make-unsafe-fifo (&optional (init-length 1000)`
Creates an instance of unsafe-fast-fifo.

### function `make-unsafe-lifo (&optional (init-length 1000)`
Creates an instance of unsafe-fast-lifo.

### function `make-safe-fifo (&optional (init-length 1000) (waitp t)`
Creates an instance of safe-fast-fifo.

### function `make-safe-lifo (&optional (init-length 1000) (waitp t)`
Creates an instance of safe-fast-lifo.

### method `enqueue (object queue)`
Push `object` into `queue`, return the object itself.
The `queue` should be an instance of any one of the queue structs above.

### method `dequeue (queue &key (keep-in-queue-p t))`
Pop an object from `queue`, return the object itself.

If `keep-in-queue-p` is NIL, the value in the place that's just popped will be set to NIL, or else the value will not be changed.

`waitp` slot takes effect only when `queue` is an instance of `safe-fifo` or `safe-lifo`.
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

### method list-to-queue (list queue-type &optional (waitp t))
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

## Performance

In this section, 10^8 times of enqueue and 10^8 times of dequeue were made and have they were  timed with common lisp's `time` macro, the results were showed below.
All of the tests were ran in SBCL,  in a Linux x86-64 vm hosted on win10, the vm has 4 cores and 16G memory.
All time units are in seconds.

### FIFO, single thread, init-length 1000, waitp nil

In this table, several FIFO queues were compared which make 10^n times of enqueue+dequeue.
The init-length of unsafe-fifo and safe-fifo were set to default (1000).
Note that cl-speedy-queue is a bounded queue so that enough space was allocated first.
All of these result were ran in single thread.


| ops 10^n | unsafe-fifo | safe-fifo (wait nil) | safe-fifo(wait t) | cl-speedy-queue | simple-queue | simple-cqueue | list-queue |
| :------: | :---------: | :------------------: | :---------------: | :-------------: | :----------: | :-----------: | :--------: |
|    3     |    0.000    |        0.000         |       0.000       |      0.000      |    0.000     |     0.004     |   0.000    |
|    4     |    0.000    |        0.000         |       0.004       |      0.000      |    0.000     |     0.004     |   0.000    |
|    5     |    0.008    |        0.016         |       0.064       |      0.000      |    0.020     |     0.032     |   0.000    |
|    6     |    0.052    |        0.184         |       0.648       |      0.012      |    0.188     |     0.344     |   0.020    |
|    7     |    0.477    |        1.848         |       6.680       |      0.148      |    1,892     |     3.624     |   0.180    |
|    8     |    4.714    |        18.724        |      63.744       |      1.542      |    19.348    |    35.676     |   2.080    |

`simple-queue` and `simple-cqueue` are queue types for the library of queues <https://github.com/oconnore/queues>, where simple-queue is an unsafe FIFO queue, and simple-cqueue is a safe FIFO queue which are implemented by adjustable array. It's clear that the fifo queues of cl-fast-queues are faster.

### LIFO, single thread, init-length 1000

In this table, four LIFO queues were compared which make 10^n times of enqueue+dequeue.
The init-length of unsafe-fifo and safe-fifo were set to default (1000).
Note that cl-speedy-lifo is a bounded queue so that enough space was allocated first.
All of these result were ran in single thread.

| ops 10^n | unsafe-lifo | safe-lifo(waitp nil) | safe-lifo(waitp t) | cl-speedy-lifo | list  |
| :------: | :---------: | :------------------: | :----------------: | :------------: | :---: |
|    3     |    0.000    |        0.000         |       0.000        |     0.000      | 0.000 |
|    4     |    0.000    |        0.004         |       0.016        |     0.000      | 0.000 |
|    5     |    0.004    |        0.020         |       0.076        |     0.000      | 0.000 |
|    6     |    0.048    |        0.184         |       0.636        |     0.012      | 0.012 |
|    7     |    0.440    |        1.828         |       6.588        |     0.148      | 0.128 |
|    8     |    4.284    |        18.928        |       66.764       |     1.468      | 1.676 |


### FIFO, single thread, init-length changed

In this table, a fixed 10^8 times of enqueue+dequeue were made, but the init-length were changed from 10^3 to 10^8.
Note that cl-speedy-lifo and list-queue are not applicable here.

| init-length 10^n | unsafe-fifo | safe-fifo(waitp nil) | safe-fifo(waitp t) |
| :--------------: | :---------: | :------------------: | :----------------: |
|        3         |    4.320    |        18.636        |       65.076       |
|        4         |    4.576    |        18.840        |       68.432       |
|        5         |    4.552    |        19.084        |       65.572       |
|        6         |    4.500    |        19.228        |       65.640       |
|        7         |    4.588    |        19.596        |       66.076       |
|        8         |    4.564    |        19.148        |       65.168       |


### LIFO, single-thread, init-length changed

In this table, a fixed 10^8 times of enqueue+dequeue were made, but the init-length were changed from 10^3 to 10^8.
Note that cl-speedy-lifo and list-queue are not applicable here.

| init-length 10^n | unsafe-lifo | safe-lifo(wait nil) | safe-lifo(waitp t) |
| :--------------: | :---------: | :-----------------: | :----------------: |
|        3         |    4.428    |       19.440        |       60.720       |
|        4         |    4.708    |       19.640        |       62.756       |
|        5         |    4.568    |       19.348        |       64.032       |
|        6         |    4.716    |       19.464        |       62.348       |
|        7         |    4.732    |       19.436        |       64.636       |
|        8         |    4.524    |       19.056        |       63.356       |


### Multi-threads

In this table, a fixed 10^8 times of enqueue+dequeue were made.
Note that for each case, a number of threads were spawned to execute enqueue, and the same number of threads were spawned to execute dequeue. For example, in the 2nd row, there were 2 threads which executed enqueue as well as 2 threads which executed dequeue.
`time` macro was no longer suitable in this situation, instead of that, a loop was used and inspected that if all the en/dequeue operations were done in the loop in the main thread, and thus the time interval could be found out.


| threads num | safe-fifo(waitp nil) | safe-fifo(waitp t) | safe-lifo(waitp nil) | safe-lifo(waitp t) | simple-cqueue |
| :---------: | :------------------: | :----------------: | :------------------: | :----------------: | :-----------: |
|      1      |        51.432        |      119.198       |        52.450        |      126.755       |    88.673     |
|      2      |        35.211        |      148.590       |        37.557        |      150.945       |    76.129     |
|      3      |        40.442        |      120.081       |        41.046        |      133.014       |    55.811     |
|      4      |        46.402        |      128.051       |        47.366        |      132.243       |    73.094     |

## Conclusion

The four queues, unsafe-fifo, unsafe-lifo, safe-fifo and safe-lifo are developed to deal with unbound queues which are implemented with simple-arrays.

The unsafe versions are slightly slower than their underground bounded counterparts. The `waitp` slot is an very important factor for the performance, and the safe versions are 3`~`4 times slower than their unsafe versions in single thread environments, and 2`~`3 times slower in multiple threads environments. The reason is apparent, the semantics of queues (either fifo or lifo) are single threaded, which can not accelerate in threads, and the locks and condition variables in the safe versions defeated their performances.

The safe version are suitable for the environments where data may be modified by other threads. The `waitp` slot is a very important for the performance,  DO NOT set it to T unless you do want to wait for a significant value in threads, although the default value of waitp is T.

 If single thread accessing can be guaranteed and the queue might be unbound, use the unsafe versions first.

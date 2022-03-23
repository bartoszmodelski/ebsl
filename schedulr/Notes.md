
* Comparing indices that may wrap around is a little subtle. For example, careless check whether queue is full (`head + size <= tail`) does not work if tail has wrapped around. The following is safer (`size <= tail - head`).  

Note `<=` since owner increments `tail` optimistically.

* Scheduler should return once all domains idle and queues are empty. 
* There should be a way to terminate the scheduler if needed.
* Scheduler should not die when a thread outside the pool tries to schedule something. We should have an overflow queue, where those tasks can be pushed. 
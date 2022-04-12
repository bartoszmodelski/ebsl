Work-stealing is very good at prioritising throughput. Under high stress, processors are pretty much allowed to just focus on their own chunk of work without having to coordinate with others. This bias, generally, makes sense, as it maximizes locality of the workload and overall performance. Further, there are enticing optimization opportunities to make the local add and remove operations fast since they are known not to collide with each other (conversely, global steal has to linearize with all local methods). 

This design, however, may not translate into favourable latency characteristics. Each CPU is locally greedy. To reap described benefits, it prefers to run its own tasks as long as there are any. Assume 10 cores with NIC. 9 out of 10 cores process packets as they come, while the 10th has a 200ms backlog. In such a scenario, 10th CPU won't receive any help, even though it would be clearly beneficial from tail latency point of view. 

What if a work-stealing CPU obligatorily stole work every n tasks? Such a strategy (and other naive heuristics) cause more harm than good as:
* The main benefit of work-stealing - locality - is traded off. 
* The underlying problem is one of communication to make the right call with respect to global workload. Biasing local choice towards stealing is going to hurt the performance&latency when stealing is inappropriate, e.g. equal backlog on all CPUs. 

A more principled approach enables said communication in an efficient way, for example using a queue to distribute work. MPMC queues are commonly considered an antipattern for work distribution as all inserts and removals are serialized, thus overall performance is comparable with an SPSC. The queue, nonetheless, can be sharded. 


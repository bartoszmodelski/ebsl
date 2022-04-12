# Work-stealing vs micropools

Work-stealing is heavily biased towards throughput. Under high stress, processors do not need to coordinate anything with others and can mostly focus on their own chunk of work. Further, even though the runqueue has to be thread-safe, there is a number of enticing optimiziations to make the local enqueue-dequeue operations ultra performant thanks to the fact that they do not need to synchronize with each other. (Conversely, steal has to be designed with more care as it needs to linearize under all conditions). This bias is not coincidental; focus on owned tasks improves locality and minimum required synchronization bring the performance overhead close to single-core processing.

This design, however, may not translate into favourable latency characteristics. Each CPU is locally greedy. To reap described benefits, it prefers to run its own tasks as long as there are any. Assume 10 cores with NIC. 9 out of 10 cores process packets as they come, while the 10th has a 200ms backlog. In such a scenario, 10th CPU won't receive any help, even though it would be clearly beneficial from tail latency point of view. 

What if a work-stealing CPU obligatorily stole work every n tasks? Such a strategy (and other naive heuristics) cause more harm than good as:
* The main benefit of work-stealing - locality - is traded off. 
* The underlying problem is one of communication to make the right call with respect to global workload. Biasing local choice towards stealing is going to hurt the performance&latency when stealing is inappropriate, e.g. equal backlog on all CPUs. 

A more principled approach should enable said communication in an efficient way. One way is a queue, which maximizes global fairness by design. MPMC queues are commonly considered an antipattern for work distribution as all inserts and removals are serialized, what heavily impedes scalability. However, that can be simpley remediated with sharding. While sharded queue is technically not a queue (as FIFO is not guaranteed), it still provides far more fairness than required. (Proof?)

Assume a bimodal workload of 5m packets. Vast majority of them are cheap to process but there's a 0.01% chance that a packet turns out to require orders of magnitude more CPU time than average. In such a case, a division of the resources into 2 pools joined by queue improves tail latencies. Even though, there is an extra cost of maintaining the queue and potentially wasted CPU cycles due to fixed CPU allocation.

4 executions: micropools, pure work-stealing, micropools, pure work-stealing.
```
./benchmark_micropools_bimod.exe -mode pools
starting micropool a (sz:2)
starting micropool b (sz:1)
latency:[     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,    40,    86,   192,   366,436415,4551334, 11068,   417,    81,     1,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0]
Quartiles: .5: 21, .99: 21, .999: 22, 9995: 22

./benchmark_micropools_bimod.exe -mode single
starting micropool a (sz:3)
latency:[     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,    51,   158,   254,225595,4218762,523474, 25067,  6587,    52,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0]
Quartiles: .5: 20, .99: 21, .999: 23, 9995: 23

./benchmark_micropools_bimod.exe -mode pools
starting micropool a (sz:2)
starting micropool b (sz:1)
latency:[     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,    26,    74,   596,  7986,4722059,268789,     1,   353,   107,     9,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0]
Quartiles: .5: 20, .99: 21, .999: 21, 9995: 21

./benchmark_micropools_bimod.exe -mode single
starting micropool a (sz:3)
done
latency:[     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,    20,   130,   496, 14769,4367348,587184, 25853,  4130,    70,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0]
Quartiles: .5: 20, .99: 21, .999: 22, 9995: 23
```


# Criticism of work-stealing - work-shedding TODO 

Different approach: try to fix work-stealing. Work-stealing is necessary in a non-preemptive scheduler, but it should not be the primary mean of distributing load. Only of saving latency for stuck cpus. 

How about work-shedding? And not a naive (that is, still heavily randomized) one like mcclure, but one where the load is shedded via a scalable queue. 
  

# Resizing - horizontal or vertical?

Resizable runqueues are implemented. There is an interesting novelty to try though: instead of scaling up runqueue 2x, we could create another queue and randomize between them. It will have marginal effect locally but will make work-stealing far more efficient, as N thief can act simultaneously.

Arguably, contention is an unlikely problem with pure work-stealing, since thiefs will rarely contend on the same victim. Work-shedding OTOH is likely to have all free CPUs try to steal from a single spawner and that's a good thing, as long as the thiefs can be served. 


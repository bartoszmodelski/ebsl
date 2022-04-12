Work-stealing is very good at prioritising throughput. Under high stress, processors are pretty much allowed to just focus on their own chunk of work without having to coordinate with others. This bias, generally, makes sense, as it maximizes locality of the workload and overall performance. Further, there are enticing optimization opportunities to make the local add and remove operations fast since they are known not to collide with each other (conversely, global steal has to linearize with all local methods). 

This design, however, may not translate into favourable latency characteristics. Each CPU is locally greedy. To reap described benefits, it prefers to run its own tasks as long as there are any. Assume 10 cores with NIC. 9 out of 10 cores process packets as they come, while the 10th has a 200ms backlog. In such a scenario, 10th CPU won't receive any help, even though it would be clearly beneficial from tail latency point of view. 

What if a work-stealing CPU obligatorily stole work every n tasks? Such a strategy (and other naive heuristics) cause more harm than good as:
* The main benefit of work-stealing - locality - is traded off. 
* The underlying problem is one of communication to make the right call with respect to global workload. Biasing local choice towards stealing is going to hurt the performance&latency when stealing is inappropriate, e.g. equal backlog on all CPUs. 

A more principled approach enables said communication in an efficient way, for example using a queue to distribute work. MPMC queues are commonly considered an antipattern for work distribution as all inserts and removals are serialized, what heavily impedes scalability. A simple solution to that problem is sharding. Sharded queue is technically not a queue anymore, as FIFO is not guaranteed, but it still provides very good fairness on average. 

Assume a bimodal workload of 5m packets. Vast majority of them are cheap to process but there's a 0.01% chance that a packet turns out to require orders of magnitude more CPU time than average. In such a case, a division of the resources into 2 pools joined by queue improves tail latencies. Even though, there is an extra cost of maintaining the queue and potentially wasted CPU cycles due to fixed CPU allocation.

Naive:
```
./benchmark_micropools_bimod.exe -mode pools
starting micropool a (sz:2)
starting micropool b (sz:1)
done
latency:[     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,    40,    86,   192,   366,436415,4551334, 11068,   417,    81,     1,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0]
.5: 21, .99: 21, .999: 22, 9995: 22

./benchmark_micropools_bimod.exe -mode single
starting micropool a (sz:3)
latency:[     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,    51,   158,   254,225595,4218762,523474, 25067,  6587,    52,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0]
.5: 20, .99: 21, .999: 23, 9995: 23

./benchmark_micropools_bimod.exe -mode pools
starting micropool a (sz:2)
starting micropool b (sz:1)
latency:[     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,    26,    74,   596,  7986,4722059,268789,     1,   353,   107,     9,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0]
.5: 20, .99: 21, .999: 21, 9995: 21

./benchmark_micropools_bimod.exe -mode single
starting micropool a (sz:3)
done
latency:[     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,    20,   130,   496, 14769,4367348,587184, 25853,  4130,    70,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0]
.5: 20, .99: 21, .999: 22, 9995: 23
```



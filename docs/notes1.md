* Benchmarked queues, removed some slow blocking in the dequeue of MPMC, created sharded MPMC that scales far better (enque random and blocking, deque iters over all queues). 

Result: https://github.com/bartoszmodelski/ebsl/blob/main/evaluation/queue/queues2.png - half domains send, half receive. LF - lock-free, LFM - sharded. 



* Queues between stages don't really improve latency. If the tasks are very lightweight and queue very optimized the performance is similar - gains from pooling are similar to costs of extra queueing. If the tasks are heavy, I think that work-stealing wins due to better resource allocation. 

Tested with 5 pools undercommited & overcommited.

Maybe having them autoscale would help bring out benefits? That is, currently, there is a constant number of threads per pool. What if there was just n threads that work on all pools, each thread reevaluating which pool to take from every 100 tasks? That would also help testing the idea of stage-based arch at a limit. Literally turn off work-stealing, per processor queues and have every task "kind" land in a different queue. 



* Created a bunch of heuristics (`instance.ml`) for improving tail latency of LIFO and they work, e.g.:

```
./benchmark_process_packet.exe -scheduler LIFO -num-of-domains 1 -num-of-spawners 1
time:796
latency:[     ...,     0,    35,    86,   145,   302,  2377,  2004,  3830,  7299, 14846, 24437, 18411, 12860,  8006,  3716,  1263,   302,    70,    10,     2,     0]

./benchmark_process_packet.exe -scheduler hybrid_reverse_every_n -num-of-domains 1 -num-of-spawners 1
time:819
latency:[     ...,     0,    22,    50,   112,   249,  1685,  1541,  2941,  5664, 11486, 18641, 19254, 18767, 15530,  4045,    14,     0,     0,     0,     0,     0]
```

- Benchmarked queues, removed some slow blocking in the dequeue of MPMC, created sharded MPMC that scales far better (enque random and blocking, deque iters over all queues). 

Result: https://github.com/bartoszmodelski/ebsl/blob/main/evaluation/queue/queues2.png - half domains send, half receive. LF - lock-free, LFM - sharded. 



- Queues between stages don't really improve latency. If the tasks are very lightweight and queue very optimized the performance is similar - gains from pooling are similar to costs of extra queueing. If the tasks are heavy, I think that work-stealing wins due to better resource allocation. 

Tested with 5 pools under&overcommited.

Maybe having them autoscale would help bring out benefits? That is, currently, there is a constant number of threads per pool. What if there was just n threads that work on all pools, each thread reevaluating which pool to take from every 100 tasks? That would also help testing the idea of stage-based arch at a limit. Literally turn off work-stealing, per processor queues and have every task "kind" land in a different queue. 
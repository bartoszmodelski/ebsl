Scheduler for multicore OCaml (work in progress).

Directories:
* `datastructure` - core datastructures used by the scheduler. Queues, stacks, obstruction and lock-free implementations, with/out stealing. 
* `schedulr` - simple task scheduler with promises.
* `micropools` - extension of `schedulr` to a multischeduler graph framework.
* `evaluation` - jupyter notebooks with benchmarks.  
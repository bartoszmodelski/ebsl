Scheduler for multicore OCaml (work in progress).

Directories:
* `datastructure` - core datastructures used by the scheduler. Queues, stacks, obstruction and lock-free implementations, with/out stealing. 
* `schedulr` - simple task scheduler with promises.
* `micropools` - extension of `schedulr` to a multischeduler graph framework (SEDA-like).
* `evaluation` - jupyter notebooks with benchmarks.  
* `test` - mix of tests and benchmarks.
* `dscheck` - vendored dscheck (to be removed).

Todo:
* Move stack and other queues into `datastructure`
* Trim dependencies 
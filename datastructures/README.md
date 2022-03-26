The algorithms here are inspired by: 

https://dl.acm.org/doi/pdf/10.1145/3437801.3441583

It has been modified in a number of ways to: 
* Prevent overlaps. 
* Prevent blocking.
* Improve enque in a single-producer case. 
* Do LIFO.
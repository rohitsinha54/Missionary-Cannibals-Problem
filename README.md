Missionary-Cannibals-Problem
============================

Classical missionary cannibal problem in LISP. 

The solution uses a greedy search strategy to solve the given problem. Greedy algorithm works through a problem on the basis of heuristic (h) and tries to obtain global optimum by making optimal choices locally.
The solution uses an admissible heuristic, which is (total number of missionaries + total number of cannibals -1 on the starting side of the river). States are stored in a data structure state-node
```
state-node{
	state - an array containing number of missionary, cannibals and boat position
	h - heuristic
	state_id – a unique number given to this state for identification 
	parent_state_id – state id of the parent state
}
```

How to compile and run
-----------------------
~$ clisp

[1]> (load "<path from root to this folder>/mc.lisp")

[2]> (solve-missionary-cannibal 13 13 6)

```
Output comes here
```

[3]> (load "<path from root to this folder>/mc.lisp")

[4]> (solve-missionary-cannibal 22 22 6)

```
Output comes here
```

Contact Information
--------------------
Name: Rohit Sinha

Email id: sinha049@umn.edu
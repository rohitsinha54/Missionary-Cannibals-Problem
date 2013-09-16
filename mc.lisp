;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Title    :   Missionary-Carnnibal Solver (using heurisitc) 
;; Problem  :   Write Lisp code that solves the missionaries and cannibals 
;;          :   problem assuming that you have one boat, and 13 cannibals and 
;;          :   13 missionaries. Assume that the boat can carry at most six 
;;          :   people  and there is a single boat available. 
;;          :   You cannot also have the boat moving with no one onboard. 
;;          :   The cannibals should never outnumber the missionaries (either on
;;          :   the boat or at both sides of the river). You can write your own 
;;          :   code or modify the code from the Russell’s web site to solve 
;;          :   the problem. You need to printout the sequence of moves 
;;          :   leading to a correct solution. Try the same problem with 22 
;;          :   cannibals and 22 missionaries. You should not hard-code your 
;;          :   solution.
;; Date     :   Nov 28, 2012
;; Author   :   Rohit Sinha
;; Assignment:  Artificial Intelligence CSCI 5511 - Lisp Assignment
;; Due Date :   Dec 11, 2012
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Variables
;; to store the different states
(setf prioritized-state-queue (make-array 0 :fill-pointer t :adjustable t))
;; traversed states are stored here
(setf traversed_states (make-array 0 :fill-pointer t :adjustable t))
;; the moves are stored here
(setf move_sequence (make-array 0 :fill-pointer t :adjustable t))
;; explicitly defined goal. We know that we want 0 missionary and cannibals at
;; the starting side and the boat on the goal side
(setf goal-state '#(0 0 0))
;; the boat. it will store the no of missionary and cannibals on the boat and
;; the current position of the boat
(setf boat (make-array 3))
;; counter variable 
(setf counter 0)
;; the traversed flag. set to 1 when the state is traversed else 0 or clear
(setf traversed_flag 0)

;; the structure of the of a state
(defstruct state-node
	state		; an array contatining the number of missionary, cannibals and
                ; boat position
	h		    ; heuristic
	state_id	    ; id of the node
	parent_state_id); id of the parent node



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   solve-missionary-cannibal    
;; Purpose          :   the main function which calls others function to solve.
;; Parameters       :   total number of missionaries, total number of cannibals 
;;                  :   and the boat size
;; Returns          :   none
;; Calls to         :   initializer, m-c-solver, move-sequence,
;;                  :   print-move-sequence
;; Called from      :   command line from this program (the earlier line)
;; Method           :   1. Initialize the starting state
;;                  :   2. call the solver funtion to solve
;;                  :   3. call the move sequence function to find moves
;;                  :   4. find total moves made
;;                  :   5. call print move sequence function to print moves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun solve-missionary-cannibal(total_missionary total_cannibals boat_size)

    (format t "~%~%~%~%~%")
    (format t "============ Welcome to Missionary-Cannibal Solver ============")
    (format t "===~%")
	;; Initialize the missionary cannibal problem
    (initializer total_missionary total_cannibals boat_size)
    ;; call the recursive solver function to solve the peoblem
	(m-c-solver total_missionary total_cannibals boat_size)	
    ;; call the move-sequence function to find the exact moves
	(move-sequence)
    ;; calculate total number of moves done
	(setf total_moves (- (length move_sequence) 1))		
	;; pop the first initializer move
    (setf last_move (vector-pop move_sequence))
    (format t "~%Initial Side           Boat             Goal Side")
	;; print the move sequence
    (print-move-sequence)
    ;; print total number of moves made to solve
    (format t "~%Number of moves made = ~S" total_moves)
    ;; print end marker
    (format t "~%~%=========================================================")
    (format t "=========")
)	



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   initializer 
;; Purpose          :   to initialize for the solver with starting state of
;;                  :   missionary and cannibals and boat position 
;; Parameters       :   tot_m:  total number of missionary,
;;                  :   tot_c:  total number of cannibals,
;;                  :   boat_s: size of the boat
;; Returns          :   none
;; Calls to         :   none
;; Called from      :   solve-missionary-cannibal
;; Method           :   1. Make an array of size 3
;;                  :   2. array[0] = total_missionary
;;                  :   3. array[1] = total_cannibal
;;                  :   4. array[2] = position of boat
;;                  :   5. create a node with this state and push to prioritized
;;                  :      state queue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun initializer (tot_m tot_c boat_s)
    ;; make the array of length 3 for storing missionary, cannibal and boat pos
    (setf start_state (make-array 3))
    ;; store the values 
    (setf (aref start_state 0) tot_m)
    (setf (aref start_state 1) tot_c)
    (setf (aref start_state 2) 1)
    ;; create and push this state to the prioritized state tree
    (vector-push-extend (make-state-node 
                            :state start_state 
                            :h (- (+ tot_m tot_c) 1) 
                            :state_id 0 
                            :parent_state_id 0 
                        ) prioritized-state-queue
    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   m-c-solver
;; Purpose          :   recurssive calls itself untill reach the goal state and 
;;                  :   keep on generating new state while moving boat
;; Parameters       :   tot_m:  total number of missionary,
;;                  :   tot_c:  total number of cannibals,
;;                  :   boat_s: size of the boat
;; Returns          :   performs recurssion
;; Calls to         :   move-boat, sort-queue,
;;                  :   m-c-solver
;; Called from      :   solve-missionary-cannibal, m-c-solver(itself) 
;; Method           :   1. pops a state from the state queue
;;                  :   2. pushes it to traversed states
;;                  :   3. return 0 if this is the goal state and ends
;;                  :   4. if this not the goal state then
;;                  :       4a. for loops till boat size
;;                  :       4b. sets the boat with valid values
;;                  :       4c. calls the move boat function to generate new
;;                  :           states by moving missionary and cannibals.
;;                  :   5. sort the state queue on prioroty
;;                  :   6. recursive call
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun m-c-solver(tot_m tot_c boat_s)
    ;; pop the next state from the prioritized state queue
	(setf this_state (vector-pop prioritized-state-queue))
    ;; since this state is now been traversed push to traversed_states		
	(vector-push-extend this_state traversed_states)			
	(if (equalp (state-node-state this_state) goal-state)
        (progn
            ;; return 0 if this is the goal state
            0
        )
		(progn
            ;; loop till boat size
            ;; calls move boat function with constraints of boat
		    (loop for i from 0 to boat_s do
			    (loop for j from 0 to boat_s do
                    ;; if the sum of missionary and cannibal to moved are not
                    ;; more than boat size and more than 0 and number of
                    ;; cannibals on the boat is less than equal to missionaries 
                    ;; or either missionary or cannibals are not on boat at all
			        (if (and(and(< (+ i j) (1+ boat_s)) (> (+ i j) 0)) 
                                                (or (<= j i) (= 0 i) (= 0 j)))
				        (progn
                            ;; if all this contraint of boat are meet
                            ;; the number of missionaries on the boat
                            (setf (aref boat 0) i) 
                            ;; number of cannibals on boat
                            (setf (aref boat 1) j) 	
                            ;; the current position of boat
				            (setf (aref boat 2) 1) 	
                            ;; call the boat move function on this state with
                            ;; the current value of the boat array to generate
                            ;; new state             
                            (move-boat this_state boat tot_m)
                        )
                    )
                )
            )
            ;; sort the state queue on basis of the priority
		    (sort-queue prioritized-state-queue)
            ;; do recurssion until goal state is reached	
		    (m-c-solver tot_m tot_c boat_s)
        ) ;; end of progn
    ) ;; end of if
)	



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   move-boat
;; Purpose          :   generates new valid states by moving missionary and
;;                  :   cannibals from one side of the river to other
;; Parameters       :   this_state: the current state from which new states can
;;                  :               be generated
;;                  :   boat: array containing boat values
;;                  :   tot_m: total number of missionaries
;; Returns          :   none
;; Calls to         :   is-traveersed, is-valid   
;; Called from      :   m-c-solver
;; Method           :   1.  make a new array to store new values of missionary, 
;;                  :       cannibals and boat
;;                  :   2.  set the heurisitc of this state to 0 for now
;;                  :   3.  loop for the length of the state from which move has
;;                  :       to be performed
;;                  :   4.  create new state values
;;                  :   5.  perform checks for new and valid state
;;                  :       5a.  if new and valid state push it to state tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun move-boat((state-node-state this_state) boat tot_m)
    ;; make a new array to store new values of missionary, cannibals and boat
	(setf new_state (make-array (length (state-node-state this_state))))
    ;; set the heurisitc of this state to 0 for now
	(setf heuristic 0)
    ;; loop for the length of the state from which move has to be performed
	(loop for i from 0 to (1- (length (state-node-state this_state))) do
		(if (= (aref (state-node-state this_state) 2) 1)
            (progn
                ;; do this if boat is on the starting side of the river
                ;; subtract moving missionaries and cannibals from the
                ;; missionary and cannibals on the starting side of the river
		        (setf (aref new_state i) 
                    (- (aref (state-node-state this_state) i)(aref boat i))
                )
            )
            (progn
                ;; do this if boat is on the goal side of the river
                ;; add moving missionaries and cannibals from the
                ;; missionary and cannibals on the starting side of the river
		        (setf (aref new_state i) 
                    (+ (aref (state-node-state this_state) i)(aref boat i))
                )
            )
        )
    )
    ;; calculate the heurisitic value with the new value of missionary and
    ;; cannibals
	(setf heuristic (- (+ (aref new_state 0) (aref new_state 1)) 1))
    ;; check if already present in the state tree
	(is-traversed new_state)
    ;; check for valid state
	(if (and (= 1 (is-valid new_state tot_m)) (= traversed_flag 0))
        (progn
            ;;; if this is a valid new state push to the state tree
			(vector-push-extend (make-state-node 
                                    :state new_state 
                                    :h heuristic 
			                        :state_id (incf counter)  
                                    :parent_state_id (state-node-state_id 
                                                                    this_state)
                                ) prioritized-state-queue
            )	
        )
    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   is-valid  
;; Purpose          :   to check is the new state generated is vaid i.e. satisfy
;;                  :   the problem constraints
;; Parameters       :   to_check: state to be checked,
;;                  :   tot_m: total number of missionaries
;; Returns          :   0 - invalid; 1 - valid
;; Calls to         :   none
;; Called from      :   move-boat 
;; Method           :   1. return 0 if any of the contraint is unsatisfied
;;                  :   2. else returns 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun is-valid (to_check tot_m)
    ;; return 0 if number of missionary or cannibals are 0 or more than total
    ;; number of total missionaries
    (if (or (< (aref to_check 0) 0) (< (aref to_check 1) 0) 
                    (> (aref to_check 0) tot_m) (> (aref to_check 1) tot_m))
    	0
	    (progn
            ;; return 0 if the total number of cannibals are more than the
            ;; number of missionaries in the new state
	        (if(and (> (aref to_check 1) (aref to_check 0)) 
                            (/= 0 (aref to_check 1)) (/= 0 (aref to_check 0)))
		        0
                (progn
                    ;; return 0 if the total number of missionaries - new
                    ;; cannibals is greater than total missionaries - 
                    ;; new missionaries and their subtration result of both of
                    ;; them is not 0
                    (if (and (> (- tot_m (aref to_check 1)) 
                            (- tot_m (aref to_check 0)))
                            (/= 0 (- tot_m (aref to_check 0)))
                            (/= 0 (- tot_m (aref to_check 1)))
                        )
		                0			
		                1 ;; this is a valid state. return 1
                    )
                )
            )
        )
    )
)			



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   is-traversed  
;; Purpose          :   to check is the state is already visited
;; Parameters       :   to_check: the state to be checked
;; Returns          :   none
;; Calls to         :   none
;; Called from      :   move-boat  
;; Method           :   1.  clear the traversed flag
;;                  :   2.  check this state to match any of the state in the
;;                  :       traversed states
;;                  :   3.  if matched then set the traversed flag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun is-traversed (to_check)
    ;; clear the flag 
	(setf traversed_flag 0)
    ;; set the flag if this state is present in the traversed states
	(loop for i from 0 to (1- (length traversed_states)) do
		(if (equalp to_check (state-node-state (aref traversed_states i)))
	    	(setf traversed_flag 1)
        )
    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   sort-queue
;; Purpose          :   perform sorting on the array passed
;; Parameters       :   array to be sorted
;; Returns          :   sorted array
;; Calls to         :   none
;; Called from      :   m-c-solver  
;; Method           :   1. sort the array on basis on the h value of the node
;                   :   2. return the sorted array
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sort-queue (arraytosort)
    ;; a temp array 		
 	(setq settemp arraytosort)
        ;; loop from the end to the begining of the array
    	(loop for i from (1- (length arraytosort)) downto 0 do
            ;; loop from 0 to current element position
       		(loop for j from 0 to i
            		when (> (state-node-h (aref settemp i)) 
                                    (state-node-h (aref settemp j))) do 
                        ;; swap the elements
                        (rotatef (aref settemp i) (aref settemp j))
            )
        )
        ;; return the sorted array
        settemp
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   move-sequence 
;; Purpose          :   to find the optimal move sequence by bactracking if
;;                  :   needed 
;; Parameters       :   none
;; Returns          :   none
;; Calls to         :   none
;; Called from      :   solve-missionary-cannibal
;; Method           :   1. push the last traversed state to move_sequence
;;                  :   2. determine the parent of this state
;;                  :   3. if this is root node then return
;;                  :   4. push the parent state to move sequence take the id
;;                  :      and return
;;                  :       4a. else decrement the index value and do again 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun move-sequence()
    ;; total number of traversed states
	(setq ts_index (1- (length traversed_states)))
    ;; push the last state in traversed state to
	(vector-push-extend (state-node-state (aref traversed_states ts_index)) 
                                                                move_sequence)
	;; parent id of the current state
    (setq this_state_parent (state-node-parent_state_id (aref traversed_states
                                                                    ts_index)))
	(loop
        ;; if parent id is 0 i.e. if this is root node then return
		(if (= 0 ts_index)
		    (return)
        )
        ;; if this is not the root node
		(loop
			(if (= this_state_parent (state-node-state_id (aref traversed_states
                                                                    ts_index)))
			    (progn 
                    ;; if this state is the parent state then push
                    (vector-push-extend (state-node-state 
                                            (aref traversed_states ts_index)
                                        ) move_sequence
                    )
                    ;; parent id of that state
                    (setq this_state_parent (state-node-parent_state_id 
                                            (aref traversed_states ts_index))
                    ) 
                    (return)
                )
                (decf ts_index) ;; decrement index if this is not parent node
            )
        )   ;; end of inner loop
    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Name    :   print-move-sequence
;; Purpose          :   prints the formatted move sequence till goal is reached
;; Parameters       :   none
;; Returns          :   none
;; Calls to         :   recurive function
;; Called from      :   solve-missionary-cannibal, print-move-sequence(itslef) 
;; Method           :   1. if move sequence is 0 print goal reached
;;                  :       1a. else pop form move sequence
;;                  :       1b. print the move in formatted manner
;;                  :   2. set this move as last move
;;                  :   3. do recursive call       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun print-move-sequence()
    ;; if no moves left then print goal reached
	(if (= (length move_sequence) 0)
		(format t "~%~%GOAL REACHED: Transported all missionaries and cannibals")
		(progn
			(let((this_move (vector-pop move_sequence)))
                ;; if the boat has to be moved towards goal side
        	    (if (= (aref this_move 2) 0)
			    (format t "~%(~S ~S)            -- (~S ~S) --->         (~S ~S)" 
			        ( aref last_move 0) (aref last_move 1) (- (aref last_move 0) 
			        (aref this_move 0)) (- (aref  last_move 1)
			        (aref this_move 1)) 
			        (- (aref start_state 0) (aref this_move 0))
			        (- (aref start_state 1) (aref this_move 1))
			    )
			    (format t "~%(~S ~S)           <--- (~S ~S) --          (~S ~S)"
			        (+ (aref last_move 0) (- (aref this_move 0) 
			                                                (aref last_move 0))) 
                    (+ (aref last_move 1) (- (aref  this_move 1) 
                                                            (aref last_move 1)))
                    (- (aref this_move 0) (aref last_move 0)) 
                    (- (aref  this_move 1) (aref last_move 1)) 
			        (+ (- (aref start_state 0) (aref this_move 0)) 
			                        (- (aref this_move 0) (aref last_move 0)))  
			        (+ (- (aref start_state 1) (aref this_move 1)) 
			                        (- (aref  this_move 1) (aref last_move 1)))
			    ))
                ;; set this move as last move
		        (setf last_move this_move)
		    )
            ;; do recurssion
	        (print-move-sequence)
	    )
    )
)

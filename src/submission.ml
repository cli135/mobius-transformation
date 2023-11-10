(*

FPSE Final Project
601.429 Functional Programming in Software Engineering
Fall 2023
Professor Smith
Group Advisor: Brandon Stride
Group: Hongyi Liu, Christopher Li
Date: 11-09-2023 (November 9, 2023)

*)

(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]

(* 
	You are required to use the Core libraries, don't remove the following line. If the editor is not recognizing Core (red squiggle under it for example), run a "dune build" from the shell -- the first time you build it will create some .merlin files which tells the editor where the libraries are.
*)
open Core

(* Here is a simple function which gets passed unit, (), as argument and raises an exception.  It is the initial implementation below. *)

let unimplemented () =
	failwith "unimplemented"

(* 
	This homework is structured into modules. You are to complete module Part1 and all sections within it by the first due date.

	These modules act as containers for your functions.

	You will not need to modify any lines with 'module', 'end', or 'include'. You WILL need to modify the functions within the modules.
*)

module Part1 = struct
	(*
		Part I Section 1: simple numeric recursions.
		
		All functions must be total for the specified domain;	overflow is excluded from this restriction but should be avoided.
		
	*)
	module Section1 = struct

		(*
			Given a non-negative integer `n`, compute `0+1+2+ ... +n` using recursion
			(don't use the closed-form solution, do the actual addition).
		*)
		(* tail recursive implementation, using accumulator formal parameter *)
		let rec summate_helper (n: int) (acc: int): int =
			match n with
			| 0 -> acc
			| x -> summate_helper (n - 1) (acc + x)

		let summate (n: int): int = summate_helper n 0

		(*
			Given non-negative integers `n` and `m`, compute their least common multiple.
		*)
		(* tail recursive implementation *)
		let rec gcd (n: int) (m: int): int = 
			if n < m then gcd m n
			else
				(* Euclidean algorithm *)
				match n, m with
				| a, 0 -> a
				| a, b -> gcd b (a % b)

		let lcm (n: int) (m: int): int = n * m / (gcd n m)	

		(*
			Given a non-negative integer `n`, compute the n-th fibonacci number.	Give an implementation that does not take exponential time; the naive version from lecture is exponential	since it has two recursive calls for each call.
		*)
		(* tail recursive implementation, using sliding window of length 2
			 for 1-D dynamic programming *)
		let rec fibonacci_fast (n: int) (first: int) (second: int): int = 
			match n with
			| 0 -> first
			| 1 -> second
			| x -> fibonacci_fast (n - 1) (second) (first + second)
		
		let fibonacci (n: int): int = fibonacci_fast n 0 1

	end (* module Section1 *)

	include Section1

	module Section2 = struct
		(*
			Part I Section 2: building and modifying lists. The List. module functions may NOT be used (yet).
		*)
			
		(*
			Given a non-negative integer `n`, produce a list [n; n-1; ...; 2; 1].
		*)
		(* TODO if there are issues with memory, do a tail recursive
			 version and then later do a O(n) time reversal (will it be in-place though,
			 and what about memory constraints?) *)
		(* Answer: since no mutation occurs in OCaml at this stage in our learning,
			 It will not be in-place, since the list is immutable and the links cannot change.
			 We will end up creating a new list based off of the previous, kind of like
			 copying a bus that is parking in a parking space. *)
		(* This is currently a fold right O(N) time and O(N) memory solution *)
		let rec iota1_helper (n: int) : int list =
			match n with
			| 0 -> []
			| x -> x :: iota1_helper (n - 1)
		let iota1 (n: int): int list = iota1_helper n
			
		(*
			Given a non-negative integer `n`, produce a list [1; 2; ...; n-1; n],	without taking O(n^2) time.
		*)
		(* tail recursive implementation, using accumulator formal parameter *)
		(* This is an O(N) time and O(1) memory solution *)
		let rec iota2_helper (n: int) (acc: int list): int list =
			match n with
			| 0 -> acc
			| x -> iota2_helper (n - 1) (x::acc)
		let iota2 (n: int): int list = iota2_helper n []
			
		(*
			Given a positive integer `n`, produce the list of integers in the range (0, n] which it is divisible by, in ascending order.
		*)
		(* Going to try simple O(N) linear check, if this takes too much time not sure
			 how to get it sublinear, maybe like trying to break it down into its factors? *)
		(* Tail recursive as of now, to get the factors in ascending order *)
		let rec factors_helper (n: int) (potential_factor: int) (acc: int list): int list =
			match potential_factor with
			| 0 -> acc (* The tail recursive base case returns the accumulated *)
			| d -> if n % d = 0 then factors_helper n (potential_factor - 1) (d :: acc) 
						 else factors_helper n (potential_factor - 1) (acc)
		let factors (n: int): int list = factors_helper n n []

		(* 
			 Reverse a list. Your solution must be in O(n) time. Note: the solution in lecture is O(n^2).
		*)
		(* Tail recursive O(N) time and O(1) space reversal of a list,
			 Not in-place since OCaml doesn't allow mutation of links or objects
			 lists are immutable in memory
			 So we just create a new list in the recursion pattern matching on the fly *)
		let rec reverse_helper (ls : 'a list) (acc: 'a list) : 'a list =
			match ls with
			| [] -> acc
			| h :: t -> reverse_helper t (h :: acc)
		let reverse (ls : 'a list) : 'a list = reverse_helper ls []

		(* 
			Rotate a list to the right. That is, move index i to index i + k, and wrap the list around
			where necessary.
			e.g. rotate_list [1;2;3;4;5] 3 evaluates to [3;4;5;1;2]
			`k` is non-negative with no further constraints.
		*)
		(* tail recursive length of list *)
		let rec length_of_list (ls : 'a list) (length_so_far: int): int =
			match ls with
			| [] -> length_so_far
			| h :: t -> length_of_list t (length_so_far + 1)

		(* This rotate problems seems more imperative in nature
			 so that it seems like much harder to do in efficient time.
			 So I may end up doing this in quadratic time. *)
		(* Not tail recursive so no memory saved *)
		(* Helper for getting the first part of the list which will be sent
			 later to the back of the resulting list *)
		let rec get_first_m_elements (ls : 'a list) (m : int) : 'a list = 
			match ls, m with
			(* Lesson learned: always have the 2 degrees of freedom for the
				 4 exhaustive cases when pattern matching on 2-tuples, to not accidentally
				 combine two cases where they should have separate behaviors.
				 Just explicitly name the behavior for all 4 cases *)
			(* Confirmed: ok so the autograder now passes, the above scenario was indeed the case *)
			| [], 0 -> [] (* We ended exactly on time so this should be a good case I think *)
			| [], _ -> failwith "This case technically shouldn't occur and may correspond to an index out of bounds error" (* This case technically shouldn't occur *)
			(* | [], _ -> [] This case technically shouldn't occur *)
			| h :: t, 0 -> []
			(* Lesson learned: remember, x is a integer here, and h is the actual element
				 that you want to cons to the list here in a fold right fashion *)
			| h :: t, x -> h :: get_first_m_elements t (m - 1)

		(* This is folding right so not tail recursive in order to preserve
			 the original order of the list. Maybe this can be optimized and changed later. *)
		let rec get_last_elements_by_skipping_first_m_elements_and_move_to_front (ls : 'a list) (m : int) (acc: 'a list): 'a list =
			if m > 0 then
				match ls with
				(* I think this below case should never be able to be reached *)
				| [] -> failwith "I think this below case should never be reached, test later"
				| h :: t -> get_last_elements_by_skipping_first_m_elements_and_move_to_front t (m - 1) acc
			else
				match ls with
				| [] -> acc
				| h :: t -> h :: get_last_elements_by_skipping_first_m_elements_and_move_to_front t (m - 1) acc

		let rotate_list_helper (ls : 'a list) (k : int) (n : int) : 'a list =
			(* Get the first n - k elements and put that at the back *)
			let first_n_minus_k_elements = get_first_m_elements ls (n - k) in
			let acc = first_n_minus_k_elements in
			(* Then cons the last k elements to the front of the list as a whole,
				 preserving original order *)
			(* Below function takes an accumulator to hit the ground running and add on to what
				 we already have directly instead of later doing an append operation *)
			let rotated_elements =
				get_last_elements_by_skipping_first_m_elements_and_move_to_front ls (n - k) acc in
			rotated_elements
		

		(* Hmm is there another way to do this rotate function without doing it kind
			 of like getting the first few and the last few and then concatenating them
			 in the opposite order while preserving their internal orders? *)
		let rotate_list (ls : 'a list) (k : int) : 'a list =
			let n = length_of_list ls 0 in
			rotate_list_helper ls (k % n) n

		
	end (* module Section2 *)

	include Section2

	module Section3 = struct
		(*
			Part I Section 3: strings, lists, and sorting.  The List module functions cannot be used.	String comparisons operations such as String.(<=) can be used, but no other String module functions.
		*)

		(*
			Given a list of strings, check to see if it is ordered, i.e. whether earlier elements are less than or equal to later elements.
		*)
		(* It seems that empty string is lexicographically less than all other strings
			 except the empty string itself, which it is equal to *)
		let rec is_ordered_helper (ls: string list) (most_recent_seen: string): bool =
			match ls with
			(* made it to end of list successfully without finding anything breaking
				 the ordering invariant of each successive string being larger lexicographically *)
			| [] -> true
			(* checking to see if there is a counterexample which breaks the sorting invariant *)
			| h :: t ->
				(* if invariant held, then good, we continue checking through the list *)
				if String.(<=) most_recent_seen h then is_ordered_helper t h
				(* otherwise, counterexample found and we return false *)
				else false

		let is_ordered (ls: string list): bool = is_ordered_helper ls ""

		(*
			Define a function to remove the lexicographically maximum string in a list of strings.
			Return
			Error("empty list") if the input list is empty (and has no max)
			Ok(s,s_list) for s the maximum string and s_list the list with s removed, 
				if the list is not empty.

			If there are more than one max string, remove the max string that occurs last in the list.
			Relative order of the list must be maintained except for the single removed element.
		*)
		
		(* Precondition: l is not an empty list, which is checked for in the main function
			 which is called remove_max *)
		(* Finds the index of the element that is the max seen so far in the list
			 (and the element itself too, all packaged up in a tuple) *)
		let rec find_index_of_last_max_string_in_list (l: string list)
			(max_seen_so_far: string) (index_of_max_seen_so_far: int) (cur_index: int): (int * string) =
			match l with
			(* reached end of the list, assuming that the list is non-empty, which is precondition
				 of this function *)
			| [] -> (index_of_max_seen_so_far, max_seen_so_far)
			| h :: t ->
				(* String.(>=) so that we find and remove the max string that occurs last in the list *)
				(* i.e. eagerly updating max_seen_so_far (and its associated index)
					 as we go from left to right in the list *)
				if String.(>=) h max_seen_so_far then find_index_of_last_max_string_in_list t h cur_index (cur_index + 1)
				(* No max found, so we retain our current max_seen_so_far and associated index and keep searching *)
				else find_index_of_last_max_string_in_list t max_seen_so_far index_of_max_seen_so_far (cur_index + 1)
			
		(* Preserve original order by emulating the fold right pattern.
			 Now it will not be tail recursive but I think that is fine for now. *)
		let rec construct_new_list_without_max_string (l: string list) (index_of_max_string: int): string list =
			(* TODO be careful when going through a list solely based on indices
				 you have to make sure that the indices will always be valid going through the list
				 and that you didn't allow any of the indices to slip through edge cases when you
				 generated the index, i.e. you have to bounds check or verify or be sure somehow
				 you won't go out of bounds *)
			match l, index_of_max_string with
			(* TODO try to check for an out of bounds case and report on that exception *)
			| [], idx -> if idx <= 0 then [] else failwith "Index out of bounds,
				this case should not occur due to previous validation, but if it does occur, then there is an error"
			| h :: t, 0 -> construct_new_list_without_max_string t (index_of_max_string - 1)
			| h :: t, idx -> h :: construct_new_list_without_max_string t (index_of_max_string - 1)

		let remove_max (l: string list): (string * string list, string) result =
			match l with
			| [] -> Error("empty list")
			| l ->
				(* negative numbers need to be wrapping parentheses before putting them as arguments/actual parameters *)
				let (index_of_max_string, max_string) = find_index_of_last_max_string_in_list (l) ("") (-1) (0) in
				let list_without_max_string = construct_new_list_without_max_string l index_of_max_string in
				Ok(max_string, list_without_max_string)


		(*
			Write a sort routine by repeated invocations of remove_max to pull out the largest
			elements one-by-one.  You should never need to invoke `remove_max` on an empty
			list, and you can thus `assert false` (an invariant failure) if the `Error`
			case is ever returned from `remove_max`.  
			This problem shows how we can manually encode the exceptional condition in `
			remove_max` with `Ok`/`Error` but convert it to an actual side effect here
			(the `assert false` will raise an exception if hit).

			Max sort on an empty list should return an empty list, not throw an exception.
			The sorted list should be sorted from smallest to largest string lexicographically
		*)
		let rec max_sort_helper (l: string list) (acc: string list): string list = 
			match l with
			| [] -> acc
			| l ->
				(match remove_max l with
				| Error(_) -> assert false
				| Ok(removed_max_string, list_minus_max_string) ->
					max_sort_helper list_minus_max_string (removed_max_string :: acc))

		let max_sort (l: string list): string list = max_sort_helper l []

		(* 
			Split a list `ls` into two pieces, the first of which is the first `n` elements of `ls`,
			and the second is all remaining elements.
			e.g. split_list [1;2;3;4;5] 3 evaluates to ([1;2;3], [4;5])	 
			Note that this function returns a tuple. Here is an example of a tuple.
			```
			let f x = (x, 10)
			```
			Assume `n` is non-negative.
		*)
		let rec get_first_m_elements_of_list_section3 (ls : 'a list) (m : int) : 'a list = 
			match ls, m with
			(* Lesson learned: always have the 2 degrees of freedom for the
				 4 exhaustive cases when pattern matching on 2-tuples, to not accidentally
				 combine two cases where they should have separate behaviors.
				 Just explicitly name the behavior for all 4 cases *)
			(* Confirmed: ok so the autograder now passes, the above scenario was indeed the case *)
			| [], 0 -> [] (* We ended exactly on time so this should be a good case I think *)
			| [], _ -> failwith "This case technically shouldn't occur and may correspond to an index out of bounds error" (* This case technically shouldn't occur *)
			(* | [], _ -> [] This case technically shouldn't occur *)
			| h :: t, 0 -> []
			(* Lesson learned: remember, x is a integer here, and h is the actual element
				 that you want to cons to the list here in a fold right fashion *)
			| h :: t, x -> h :: get_first_m_elements_of_list_section3 t (m - 1)


		(* Getting the trailing elements after skipping the first m elements *)
		(* right folding to preserve original order of the elements in a convenient (albeit
			 O(N) memory usage) way *)
		(* m is the requested index in this function, the index at which we start copying the list *)
		let rec get_last_few_elements_of_list_by_skipping_the_first_m_elements_section3 (ls : 'a list) (m : int) : 'a list = 
			match ls, m with
			| [], x -> if x <= 0 then [] else failwith "This means index out of bounds and if it occurs, this is an error"
			(* Lesson learned: remember, x is a integer here, and h is the actual element
					that you want to cons to the list here in a fold right fashion *)
			| h :: t, x ->
				(* not skipping elements case anymore *)
				if x <= 0 then h :: get_last_few_elements_of_list_by_skipping_the_first_m_elements_section3 t (m - 1)
				(* still skipping elements until we've skipped m elements total at the beginning of the list *)
				else get_last_few_elements_of_list_by_skipping_the_first_m_elements_section3 t (m - 1)

		let split_list (ls : 'a list) (n : int) : 'a list * 'a list =
			(get_first_m_elements_of_list_section3 ls n,
			get_last_few_elements_of_list_by_skipping_the_first_m_elements_section3 ls n)

		(* 
			 Sort an int list using merge sort. Your solution must be O(n log n).
		*)
		(* tail recursive length of list *)
		let rec length_of_list_section3 (ls : 'a list) (length_so_far: int): int =
			match ls with
			| [] -> length_so_far
			| h :: t -> length_of_list_section3 t (length_so_far + 1)
		(* I'm going to hope that we can use the split_list function below in the merge_sort function *)
		(* This merge sort function is the only function originally here in the assigment
			 that I added rec to instead of making a helper function, just for extra clarity
			 of recursively calling the merge sort function itself here which makes more sense
			 than offloading that work unnecessarily to a helper function *)
		
		(* this function is fold right in order to preserve existing order in the lists
			 from what I understand at least so far *)
		let rec merge_two_sorted_lists (ls1: int list) (ls2: int list): int list =
			(* you do need all four cases to cover the exhaustive pattern matching
				 because not all lists can be destructed into h :: t so you need to
				 provide an empty list option for both lists, resulting in 2 * 2 = 4 possibilities
				 to cover in the cases below *)
			match ls1, ls2 with
			| [], [] -> []
			| [], h2 :: t2 -> h2 :: merge_two_sorted_lists [] t2
			| h1 :: t1, [] -> h1 :: merge_two_sorted_lists t1 []
			| h1 :: t1, h2 :: t2 ->
				(* ****important lesson learned****: only advance one pointer at a time when merging,
					 it doesn't make any sense to advance both because you are just
					 picking one at each step (the lesser one) to include in your merged
					 array, tadashi tokieda style, moving only one foot at a time and never both
					 moving simultaneously *)
				(* h1 was lesser, so we only advance to t1 and we keep ls2 in the same spot 
					 for the next check *)
				if h1 <= h2 then h1 :: merge_two_sorted_lists t1 ls2
				(* h2 was lesser, so we only advance to t2 and we keep ls1 in the same spot
					 for the next check *)
			else h2 :: merge_two_sorted_lists ls1 t2

		let rec merge_sort (ls : int list) =
			let length = length_of_list_section3 ls 0 in
			(* integer division in OCaml by default, nice *)
			(* figure out the inclusive and/or exclusive start and/or end indices
				 of the merge sort halves  *)
			if length = 1 then ls
			else
				let (left_half, right_half) = split_list ls (length / 2) in
				let sorted_left_half = merge_sort left_half in
				let sorted_right_half = merge_sort right_half in
				let merged_and_sorted_list_of_both_halves =
					merge_two_sorted_lists sorted_left_half sorted_right_half in
				merged_and_sorted_list_of_both_halves

	end (* module Section3 *)

	include Section3

end (* module Part1 *)

include Part1

(* *************
    END PART I
   ************* *)		

(* ***************
		BEGIN PART II
	 *************** *)

module Part2 = struct

	module Section1 = struct
		(*
			Part II Section 1: for selected functions in Part I, provide a reimplementation of your previous code by refactoring the definition to use combinators provided by the List module.
			
			Care should be taken to use a concise, elegant combination of these provided functions to best express the task.
			
			These new implementations should not be explicitly recursive.	Note that the autograder is not aware if you cheated and used recursion; we will manually inspect your code and give you negative points if 
			you used recursion.
		*)

		(*
			Given a non-negative integer `n`, produce a list [n; n-1; ...; 2; 1].
		*)
		(* Using combinators from List module below *)
		let iota1' (n: int): int list = List.init n ~f:(fun i -> n - i)

		(*
			Given a non-negative integer `n`, produce a list [1; 2; ...; n-1; n],	without taking O(n^2) time.
		*)
		(* Using combinators from List module below *)
		let iota2' (n: int): int list = List.init n ~f:(fun i -> i + 1)

		(*
			Given a positive integer `n`, produce the list of integers in the range (0, n] which it is divisible by, in ascending order.
		*)
		(* Using combinators from List module below *)
		let factors' (n: int): int list =
			List.init n ~f:(fun i -> i + 1) |>
			List.filter ~f:(fun d -> n % d = 0)

		(* 
			Split a list `ls` into two pieces, the first of which is the first `n` elements of `ls`,
			and the second is all remaining elements.
			e.g. split_list [1;2;3;4;5] 3 evaluates to ([1;2;3], [4;5])	 
			Note that this function returns a tuple. Here is an example of a tuple.
			```
			let f x = (x, 10)
			```
			Assume `n` is non-negative.
		*)
		(* Using combinators from List module below *)
		let split_list' (ls : 'a list) (n : int) : 'a list * 'a list =
			List.split_n ls n

	end (* module Section1 *)

	include Section1

	module Section2 = struct
		(*
			Part II Section 2: primes. In this section we focus on numeric recursive functions.
		*)

		(* 
			 Check if positive integer `n` is prime. This should be able to quickly handle numbers up to 2^32.
		*)
		(* let rec check_primality_starting_from_sqrt (n: int) (sqrt_n : int) : bool =
			match sqrt_n with
			| 1 -> true (* Got to end, it is prime *)
			| x -> if n % x = 0 then false
						 else check_primality_starting_from_sqrt (n) (x - 1)  *)

		(* TODO check if I am allowed to use Float module here *)
		let is_prime (n : int) : bool =
			(int_of_float (Float.sqrt (float_of_int n))) |>
			(* start the list from 2, 3, 4, ... *)
			List.init ~f:(fun i -> i + 2) |>
			List.filter ~f:(fun d -> n % d = 0) |>
			(* for 2 *)
			List.filter ~f:(fun d -> d <= n - 1) |>
			List.is_empty
			(* for testing *)
			(* if 1 = 1 then true else *)
			(* check_primality_starting_from_sqrt n (int_of_float (Float.sqrt (float_of_int n))) *)
		(* let is_prime (n : int) : bool = unimplemented () *)
		(* 
			Given a positive integer `n`, return the prime factor of `n` that has the greatest multiplicity.
			e.g. if n = 120, then its prime factors are 2, 2, 2, 3, 5, so the factor with the greatest
				multiplicity is 2.
			If two prime factors have the same multiplicity, return the largest factor.
		*)
		(* maximum geq helper *)
		(* let maximum_integer_geq n1 n2 = if n1 >= n2 then n1 else n2 *)
		(* recursive helper function to find greatest multiplicity factor *)
		let rec prime_factor_with_greatest_multiplicity_helper (n : int)
			(remaining : int) (cur_factor : int) (cur_count : int)
			(max_count : int) (max_count_factor : int) : int =
			(* for testing *)
			(* if 1 = 1 then 0 else *)
			match remaining with
			(* We divided away all the factors and are just left with 1 *)
			(* meaning all factors were traversed *)
			| 1 -> max_count_factor
			(* Case where there are still remaining factors *)
			| x -> if remaining % cur_factor = 0
						 then
								(* this factor is a part of the current count now, so
									 we will write cur_count + 1 and pass this value onto
									 the next iteration soon *)
								(* including this instance of the current factor,
									 is this the most frequent factor we have seen so far? *)
								(* geq >= since we are going from factors 1 and counting upward
									 so greater factors will override earlier factors
									 given equal counts *)
								if cur_count + 1 >= max_count
								then
									(* if so, divide away the current factor, increment the current factor's count,
										 and update the max count factor to be the current factor *)
									prime_factor_with_greatest_multiplicity_helper (n)
										(remaining / cur_factor) (cur_factor) (cur_count + 1)
										(cur_count + 1) (cur_factor)
										(* TODO maybe check the above lines later *)
									else
									(* otherwise, divide away the current factor, increment the current factor's count,
										 and keep the max count factor the same way it is for now *)
									prime_factor_with_greatest_multiplicity_helper (n)
										(remaining / cur_factor) (cur_factor) (cur_count + 1)
										(max_count) (max_count_factor)
										(* TODO maybe check the above lines later *)
									else
							(* Move on to the next factor and reset the current factor's count to 0
								 since we are moving onto the next largest factor *)
							prime_factor_with_greatest_multiplicity_helper (n)
								(remaining) (cur_factor + 1) (0)
								(max_count) (max_count_factor)

		let prime_factor_with_greatest_multiplicity (n : int) : int =
			prime_factor_with_greatest_multiplicity_helper (n)
				(* Lesson learned: the factor should start at 2,
					 since dividing by 1 will always get you the original n again
					 and you will have infinite recursion *)
				(n) (2) (0)
				(-1) (-1)
				(* TODO not sure if max_count on the left on the above line
					 should be 0 or -1, leaving as -1 for now *)

				(* TODO verify the above functions and make sure they work *)

	end (* module Section2 *)

	include Section2

	module Section3 = struct
		(*
		Part II Section 3: Checking validity of a Towers game solution

		Towers is a simple puzzle game, for rules and to play the game see
		https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/towers.html

		Here is a description taken from that site:

		Your task is to build a tower on every square, in such a way that:
		* Each row contains every possible height of tower once
		* Each column contains every possible height of tower once
		* Each numeric clue describes the number of towers that can be seen if you look into the square from that direction, assuming that shorter towers are hidden behind taller ones. For example, in a 5×5 grid, a clue marked ‘5’ indicates that the five tower heights must appear in increasing order (otherwise you would not be able to see all five towers), whereas a clue marked ‘1’ indicates that the tallest tower (the one marked 5) must come first.
			
		Since this assignment is an exercise to become familiar with functional programming, we are going to give appropriate auxiliary functions which will decompose the problem into smaller pieces.  You should be able to compose these short functions to accomplish the ultimate goal of verifying whether a completely-filled grid is a solution based on the clues around the edges.  We will only be verifying the "easy" games which don't leave out any of the clues around the grid edges.
			
		Use the List combinators, pipelining, etc. when possible and whenever it improves the readability of the solution.  All but the last function are directly code-able with the List combinators and no `rec`.
		As in Part I feel free to write your own helper functions if needed.

		If you are unsure on what the requirements of the functions are, look at the test cases we provide.

		*)

		(* We can represent the tower board itself as a list of lists of ints.  
			See `tower_board_example` in `tests.ml` for an example.

		The first task is to determine if a list of lists of integers is "square", i.e. each list is the same length and there are the same number of lists as there are elements in any of the lists. If it is, return Ok(the dimension of the array).  If not, return Error "not square". *)
		let square_size (grid: int list list) : (int, string) result =
			(* TODO ask why pipelining seems to like streamline your data flow
		into just passing one parameter or return value end-to-end consecutively
		so you can't really have parallelizing branches or multiple params,
		you just have to take the thing you are given in the bottleneck
		it seems like each pipe is a huge bottleneck of data, you only get one thing,
		and if it's just a bool then it's a real handicap. This is why it's better
			to have variables from earlier in scope to refer to
		more graph based programming rather than linked list based programming *)
			grid |>
			(List.map ~f:(List.length)) |>
			(List.all_equal ~equal:(=)) |>
			(fun all_equal_element -> 
				match all_equal_element with
				| None -> Error "not square"
				| Some(length) -> 
				(if List.length grid = length
				then Ok(length)
				else Error "not square")
			)
			
		(* Given a list of integers of length n, determine if the list has exactly one occurrence
			of each number in 1 .. n in it. Return false if not *)	
		let elements_span_range (l : int list) : bool = 
			l |>
			(* TODO am I allowed to use Int.compare here? *)
			List.sort ~compare:(Int.compare) |>
			List.for_alli ~f:(fun idx -> fun elt -> elt = idx + 1)

		
		(* Check to see if a towers grid is well-formed, namely
			1) it is square as per above,
			2) it is at least 1x1 in size (no 0x0 degenerates are allowed)
			2) each row and column spans the range as per above *)
		let well_formed_grid (grid : int list list) : bool = 
			(* TODO see the part that I am having trouble with is that
				 you have to pattern match on the result which arrives,
				 which means that you have to interrupt the pipelining
				 which doesn't make it very clean. Is there a cleaner
				 way to pattern match on results like Ok/Error or
				 options like Some/None when they are evaluted in the middle
				 of a pipeline, and when the behavior of the pipeline needs
				 to be different (i.e. return true or false, ending early/
				 short-circuiting) depending on the value of the function returned?
				 You could custom-define a helper function which would make it cleaner
				 but I am not sure if that would be okay *)
			match square_size grid with
			| Ok(0) -> false 
			| Error(s) -> false
			| Ok(n) ->
				(
					let rows_good = List.for_all grid ~f:(elements_span_range) in
					match List.transpose grid with
					| None -> false
					| Some(transposed) ->
						(
							let cols_good = List.for_all (transposed) ~f:(elements_span_range) in
							rows_good && cols_good		
						)
				)
		
		(* The next six auxiliary functions should only be called on well-formed grids, or rows from well-formed
			grids, and so you don't need to deal with ill-formed cases there such as 0x0 or non-spanning grid rows.  *)	
		
		(* The lowest level of the validity check for towers requires finding the number of
		local maxima going down a given row of the grid (i.e. down a list of integers).  Define
		a function local_max_count to find that value.  *)
		
		(* I think this function as currently written will be O(N^2)
			 when in reality if you do a sliding window of length 3 it will be O(N)
			 but I am not sure how to get that sliding window using List combinators *)


		(* 9-3-2023 2:28pm:
			 	Below is incorrect, should return 1:
	
		local_max_count <-- [3; 1; 2]
		local_max_count --> 2
		
		Lesson learned: Ohh issue found: my local max count function is wrong, it should be a skyline function with memory
			ok local max count function is harder than expected
		*)
		let local_max_count (row : int list) : int =
			 (* TODO is it okay to use List.nth_exn here? *)
			(* List.counti row
				~f:(fun idx -> fun elt ->
					if List.length row = 1 then true
					else if idx = 0 && elt > List.nth_exn row (idx + 1) then true
					else if (idx = (List.length row) - 1) && (elt > (List.nth_exn row (idx - 1))) then true
					else if (elt > List.nth_exn row (idx + 1)) && (elt > (List.nth_exn row (idx - 1))) then true
					else false) *)
			(* TODO are we allowed to use List.nth_exn as long as we are only
				 calling it on valid indices and a valid grid? *)
			(* Using List.nth_exn is O(N^2) slow, indexing into the list from the
				 beginning each time with idx, is there a way to get a sliding
				 window of length 2 with a fold_left or fold_right? *)

			(* I guess what you're looking for is the length of the
				 longest strictly increasing prefix that occurs in the row
				 wait actually it could go down like
				 1 2 4 3 5 -> should return 4, not 3
				 So even the longest strictly increasing prefix is not enough
				 you need a memory runner to remember what the highest seen is so far
				 to carry across count iterations - but how do you maintain that state
				 other than just threading through recursion. Well fold left maybe.
				 *)
			(* 
			1 2 4 3 5
			5 3 4 2 1	 
			*)
			(* With the new way of finding the max in a sublist,
				 there is no need to prepend the -1 since we are not doing
				 neighbor adjacent consecutive checking through the list anymore *)
			(* let prepended_row = ((-1) :: row) in  *)
			(* In fact, what we need now is the reversed row to take
				 advantage of the direction that OCaml lists work in, sharing suffixes,
				 not sharing prefixes. Only remaining challenge is how to refer to that suffix t below. *)

			(* This may indeed be O(N^2) due to indexing with idx into the list again
				 but at least it may work without mutation *)
			let reversed_row = List.rev row in 
			List.counti reversed_row ~f:(
				fun idx -> fun elt ->
					(* if idx < (List.length reversed_row - 1) *)
					(* Lesson learned: the parentheses around (idx + 1) are very necessary
						 in order for the parsing to work, since functions are left associative *)
						 (* then List.nth_exn reversed_prepended_row idx < List.nth_exn reversed_prepended_row (idx + 1)  *)
						 (* Important**** : this assumes that the row is [1 ... n]
								of ***unique*** numbers, so that the maximum occurs only once *)
							(* then ( *)
							(
								(* So we are indexing to get a sublist starting at that position. Ok
									 this looks like an O(N^2) operation in total for the whole
									 local_max_count method ok it's fine though *)
								match List.max_elt (List.sub reversed_row ~pos:(idx) ~len:(List.length reversed_row - idx)) ~compare:(Int.compare) with
								| None -> failwith "This case shouldn't occur because row will be nonempty"
								| Some(max_element) ->
									max_element = elt
						 	)
						(* else false *)
			)
			
		
		(* Now we need to apply the above function to each row/column around the grid.  There
		are many reasonable ways to solve that task, but here we ask you to first write a
		function verify_left_clues to check the "left side grid" clues only are correct.  For
		this function the `edge_clues` list is only the left-side clues. *)
		
		(* Again there is a better way than using idx and indexing into list
				with List.nth_exn, which takes O(N^2) time for all idx = 0 to idx = n - 1
				You can do it by zipping lists in a cartesian product *)
		
		(* This was a helpful function to see return values in the trace, just as a intermediate breakpoint kind of *)
		(* let get_cartesian_product grid edge_clues =
			List.cartesian_product grid edge_clues *)
		let get_zip grid edge_clues =
			(* TODO Look into exn vs non exn version and pattern match on Ok/Error result *)
			(* or propagate Ok/Error back up. Depends on how much testing you want to do
				 but i think the conditions are already satisfied*)
			List.zip_exn grid edge_clues

		let verify_left_clues (grid : int list list) (edge_clues : int list) : bool =
			(* Precondition: assuming length of grid and length of edge_clues are equal
				 if not, you can use List.zip but that has some Or_unequal_lengths
					thing to worry about in the evaluated type *)
			(* Lesson learned: I learned that cartesian product is ***not***
				 the same as zip!!! zip is only the diagonal
				 of the matrix, N elements. Cartesian product
				 is all N^2 elements of the matrix. *)
			(* TODO is List.zip_exn okay to use here? *)
			(* Ok zip is working correctly at least *)
			(* Lesson learned: Ohh issue found: my local max count function is wrong, it should be a skyline function with memory
ok local max count function is harder than expected

	Below is incorrect, should return 1:

		local_max_count <-- [3; 1; 2]
		local_max_count --> 2
 *)
			get_zip grid edge_clues |>
			List.for_all ~f:(fun (row, clue) -> (local_max_count row) = clue)
		
		(* In order to check the clues on all four edges, we will rotate the grid counter-clockwise and call the above function at each rotation. 
		There many ways we can rotate our grid.  Here we suggest using a combination of transpose (like for a matrix: flip rows and columns), and reflect.  Note you can assume the grid is well-formed.  *)	
		let transpose (grid : int list list) : int list list =
			match List.transpose grid with
			| None -> failwith "This case should not occur since square grid"
			| Some(transposed) -> transposed
			
		let reflect_vertical_axis (grid : int list list) : int list list =
			grid |>
			(* reverse each row to reflect across the median vertical line of longitude *)
			List.map ~f:(List.rev)
		
		(* Now it should not be difficult to define a function to rotate the grid counterclockwise *)
		let rotate_ccw (grid : int list list) : int list list = 
			(* in linear algebra, reflection matrices (and matrices in general)
				 are not commutative. Depending on the order, we can rotate either
				 90 degrees ccw or 90 degrees cw *)
			(* The correct order is below *)
			(* first reflect, and then transpose *)
			grid |>
			reflect_vertical_axis |>
			transpose
		
		(* Finally, write a function verify_towers_solution which given a grid and the clues all around the grid, verifies that the solution is correct with respect to the clues: the grid is well-formed as per above, and the clues are all satisfied by the solution.  
		The clues are a list of lists, the first element of the list is the edge_clues for the original board orientation, and the subsequent lists correspond to clues for successive rotations of the grid. 
		If either the grid is ill-formed or the clues are not all satisfied, return false.  *)
		let rec verify_towers_solution_helper  (grid : int list list) (four_edge_clues : int list list) : bool =
		 	(* TODO Why does the below line not work??? *)
			(* if ((false) = (well_formed_grid grid)) *)
			if (well_formed_grid grid)
			then 
				match four_edge_clues with
				| [] -> true (* reached the end *)
				| cur_edge_clues :: rest_of_clues ->
					(
						if verify_left_clues grid cur_edge_clues
						(* continuing to rotate and verify the other edges *)
						then verify_towers_solution_helper (rotate_ccw grid) rest_of_clues
						(* This means that we found a discrepancy *)
						else false
					)
			else false


		let verify_towers_solution  (grid : int list list) (four_edge_clues : int list list) : bool =
			verify_towers_solution_helper grid four_edge_clues

		
	end (* module Section3 *)

	include Section3

end (* module Part2 *)

include Part2
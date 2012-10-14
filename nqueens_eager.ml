(* eager nqueens program written in lazy style *)


(***** list functions *****)
(* reimplemented here in anticipation of adding laziness *)

let rec map f = function 
  | [] -> []
  | x::xs -> f x::map f xs

let rec filter p = function
  | [] -> []
  | x::xs -> 
      if p x 
      then x::filter p xs
      else filter p xs

let rec foldl f acc = function
  | [] -> acc
  | x::xs -> foldl f (f x acc) xs

let rec foldr f base = function
  | [] -> base
  | x::xs -> f x (foldr f base xs)

let rec forall f = function
  | [] -> true
  | x::xs -> f x && forall f xs

let rec (@) lst1 lst2 = 
  match lst1 with
  | [] -> lst2
  | x::xs -> x::(xs @ lst2)

let rec tails = function 
  | [] -> [[]]
  | lst -> lst::(tails (List.tl lst))

let rec rng n m =
  if n=m
  then n::[]
  else n::(rng (n+1) m)


(***** queens conflict predicates *****)

(* true indicates given two queens have no conflict *)
let isSafe (x1,y1) (x2,y2) = 
  x1 != x2 && y1 != y2
    &&
  abs (x1-x2) != abs (y1-y2)

(* true means first queen in given list is not conflicted with any other *)
let isSafe_lst = function 
  | [] -> true
  | x::xs -> forall (isSafe x) xs

(* true means no conflicts between any pair of queens in given list *)
let isValid lst = forall isSafe_lst (tails lst)
  
  
(***** nqueens *****)

let nqueens n = 
  let process_row r qss_so_far = 
    foldr 
      (fun qs new_qss -> 
	(map (fun c -> (r,c)::qs) (rng 1 n)) @ new_qss)
      [] qss_so_far
  in let all_possible_solns = foldl process_row [[]] (rng 1 n)
  in List.hd (filter isValid all_possible_solns)

(* print results *)	
let _ = map (fun (x,y) -> Printf.printf "(%d,%d)\n" x y) (nqueens 8)

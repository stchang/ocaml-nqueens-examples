(* properly lazy nqueens program -- lazy cons + extra delays *)


(* lazy list *)
type 'a l_node = Nil | Cons of 'a * 'a l_list
and 'a l_list = 'a l_node lazy_t


(***** list functions *****)

let rec map f lst = 
  match Lazy.force lst with
  | Nil -> Nil
  | Cons(x,xs) -> Cons(f x,lazy(map f xs))
  
let rec filter p lst = 
  match Lazy.force lst with
  | Nil -> Nil
  | Cons(x,xs) -> 
      if p x 
      then Cons(x,lazy(filter p xs))
      else filter p xs

let rec foldl f acc lst = 
  match Lazy.force lst with
  | Nil -> acc
  | Cons(x,xs) -> foldl f (f x acc) xs

let rec foldr f base lst = 
  match Lazy.force lst with
  | Nil -> (lazy base)
  | Cons(x,xs) -> f x (lazy (Lazy.force (foldr f base xs)))

let rec forall f lst = 
  match Lazy.force lst with
  | Nil -> true
  | Cons(x,xs) -> f x && forall f xs

let rec (@) lst1 lst2 = 
  match Lazy.force lst1 with
  | Nil -> lst2
  | Cons(x,xs) -> lazy(Cons(x,xs @ lst2))

let rec tails lst =
  match Lazy.force lst with 
  | Nil -> Cons(Nil,lazy Nil)
  | Cons(x,xs) -> Cons(Cons(x,xs),lazy(tails xs))

let rec rng n m = 
  if n=m
  then Cons(n,lazy Nil)
  else Cons(n,lazy(rng (n+1) m))


(***** queens conflict predicates *****)

let (!=) x y = not (x=y)

(* true indicates given two queens have no conflict *)
let isSafe (x1,y1) (x2,y2) = 
  x1 != x2 && y1 != y2
    &&
  abs (x1-x2) != abs (y1-y2)

(* true means first queen in given list is not conflicted with any other *)
let isSafe_lst lst =
  match lst with
  | Nil -> true
  | Cons(x,xs) -> forall (isSafe x) xs

(* true means no conflicts between queens in given list *)
let isValid lst = forall isSafe_lst (lazy(tails(lazy lst)))

	
(***** nqueens *****)

let nqueens n = 
  let process_row r qss_so_far = 
    foldr 
      (fun qs new_qss -> 
	(lazy(map (fun c -> Cons((r,c),lazy qs)) (lazy(rng 1 n)))) @ new_qss)
      Nil qss_so_far
  in
  let all_possible_solns = 
    foldl process_row (lazy(Cons(Nil,lazy Nil))) (lazy(rng 1 n))
  in 
  match filter isValid all_possible_solns with
    Nil -> Nil
  | Cons(x,xs) -> x

(* print results *)
let rec force_stream lst = 
  match Lazy.force lst with
    Nil -> Nil
  | Cons(x,xs) -> force_stream xs

let _ = 
  force_stream
    (lazy(map (fun (x,y) -> Printf.printf "(%d,%d)\n" x y) (lazy(nqueens 8))))

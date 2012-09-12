(* timing data is from nqueens_lazy_proper.ml *)
(* properly lazy nqueens program -- lazy cons + extra delays
real    0m3.379s
user    0m3.324s
sys     0m0.040s
*)

(* similar to nqueens_lazy_proper.ml, but after adding the neccessary delay
   in foldr, uses different typing to make program type check
   -- in nqueens_lazy_proper3.ml, try to avoid forcing until strict points
   -- see ../types examples/nqueens-lazy-proper-typed3 for more notes *)

type 'a l_node = Nil | Cons of 'a * 'a l_list
and 'a l_list = 'a l_node lazy_t

let add1 x = x + 1
let sub1 x = x - 1

let rec map f lst = 
  match Lazy.force lst with
    Nil -> Nil
  | Cons(x,xs) -> Cons(f x,lazy(map f xs))
  
let rec build_list_help n f m = 
  if n=m 
  then Nil
  else
    Cons(f m,lazy(build_list_help n f (add1 m)))
let build_list n f = build_list_help n f 0

let rec filter p lst = 
  match Lazy.force lst with
    Nil -> Nil
  | Cons(x,xs) -> 
      if p x 
      then Cons(x,lazy(filter p xs))
      else filter p xs

let rec foldl f acc lst = 
  match Lazy.force lst with
    Nil -> acc
  | Cons(x,xs) -> foldl f (f x acc) xs

let rec foldr f base lst = 
  match Lazy.force lst with
    Nil -> (lazy base)
(* this lazy-force definitely needed *)
  | Cons(x,xs) -> f x (lazy (Lazy.force (foldr f base xs)))

let rec andmap f lst = 
  match Lazy.force lst with
    Nil -> true
  | Cons(x,xs) -> (f x) && (andmap f xs)

let rec append lst1 lst2 = 
  match Lazy.force lst1 with
    Nil -> lst2
(* this lazy-force not needed but keep for consistency *)
  | Cons(x,xs) -> lazy(Cons(x,lazy(Lazy.force(append xs lst2))))

let (!=) x y = not (x=y)

let abs x y = 
  if x < y then y - x else x - y

let isSafe (x1,y1) (x2,y2) = 
  ((x1 != x2) && (y1 != y2))
    &&
  ((abs x1 x2) != (abs y1 y2))

let isOk lst =
  match lst with
    Nil -> true
  | Cons(x,xs) -> andmap (fun q -> isSafe x q) xs

let rec tails lst =
  match Lazy.force lst with 
    Nil -> Cons(Nil,lazy Nil)
  | Cons(x,xs) -> Cons(Cons(x,xs),lazy(tails xs))

let isValid lst = andmap isOk (lazy(tails(lazy lst)))

	
let nqueens n = 
  let qu i qss = 
    foldr 
      (fun qs acc -> 
	append 
	  (lazy(map (fun k -> Cons((i,k),lazy qs)) 
		  (lazy(build_list n add1))))
	  acc)
      Nil (lazy (Lazy.force qss))
  in
  let all_possible_solns = 
    foldl qu (lazy(Cons(Nil,lazy Nil))) (lazy(build_list n add1))
  in 
(* this lazy-force isnt needed to get lazy performance but I keep it for
   consistency *)
  match filter isValid (lazy (Lazy.force all_possible_solns)) with
    Nil -> Nil
  | Cons(x,xs) -> x

let rec force_stream lst = 
  match Lazy.force lst with
    Nil -> Nil
  | Cons(x,xs) -> force_stream xs

	
let _ = 
  force_stream
    (lazy(map (fun (x,y) -> Printf.printf "(%d,%d)\n" x y) (lazy(nqueens 8))))


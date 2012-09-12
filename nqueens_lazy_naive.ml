(* attempt at making nqueens program (written in lazy style) lazy --
   ie, just switch to lazy cons
real    3m27.474s
user    3m26.309s
sys     0m0.416s
*)

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

let rec foldr (f:'a -> 'b -> 'b) (base:'b) (lst:'a l_list) : 'b = 
  match Lazy.force lst with
    Nil -> base
  | Cons(x,xs) -> f x (foldr f base xs)

let rec andmap f lst = 
  match Lazy.force lst with
    Nil -> true
  | Cons(x,xs) -> (f x) && (andmap f xs)

let rec append lst1 lst2 = 
  match Lazy.force lst1 with
    Nil -> lst2
  | Cons(x,xs) -> Cons(x,lazy(append xs lst2))

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

	
(*
let all_possible_solns = 
  foldr
    (fun qs acc ->
      append
	(lazy(map (fun k -> Cons((1,k),lazy qs)) (lazy(build_list 8 add1))))
	acc)
    Nil (Cons(Nil,lazy Nil))
*)


let nqueens n = 
  let qu i qss = 
    foldr 
      (fun qs acc -> 
	append 
	  (lazy(map (fun k -> Cons((i,k),lazy qs)) (lazy(build_list n add1))))
	  acc)
      Nil (lazy qss)
  in
  let all_possible_solns = 
    foldl qu (Cons(Nil,lazy Nil)) (lazy(build_list n add1))
  in 
  match filter isValid (lazy all_possible_solns) with
    Nil -> Nil
  | Cons(x,xs) -> x

let rec force_stream lst = 
  match Lazy.force lst with
    Nil -> Nil
  | Cons(x,xs) -> force_stream xs
	
let _ = 
  force_stream
    (lazy(map (fun (x,y) -> Printf.printf "(%d,%d)\n" x y) (lazy(nqueens 8))))

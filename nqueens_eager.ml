(* eager nqueens program written in lazy style *)

let rec map f = function 
  | [] -> []
  | x::xs -> f x::map f xs

let rec 1to_help n f m = 
  if n=m 
  then []
  else
    f m::1to_help n f (add1 m)
let 1to n f = 1to_help n f 0

let rec filter p = function
  | [] -> []
  | x::xs -> 
      if p x 
      then x::filter p xs
      else filter p xs

let rec foldl f acc = function
  | [] -> acc
  | x::xs -> foldl f (f x acc) xs

(*
let rec foldr f base lst = 
  match lst with
    [] -> base
  | x::xs -> f x (foldr f base xs)
*)
let rec foldr f base = function
  | [] -> base
  | x::xs -> f x (foldr f base xs)

let rec andmap f = function
  | [] -> true
  | x::xs -> f x && andmap f xs

let rec append lst1 lst2 = 
  match lst1 with
  [] -> lst2
  | x::xs -> x::append xs lst2

let rec (@@) lst1 lst2 = 
  match lst1 with
  | [] -> lst2
  | x::xs -> x::xs @@ lst2

let (!=) x y = not (x=y)

let abs x y = if x < y then y - x else x - y

let isSafe (x1,y1) (x2,y2) = 
  x1 != x2 && y1 != y2
    &&
  abs x1 x2 != abs y1 y2
  
let rec tails = 
  function [] -> [[]]
    | lst -> lst::(tails (List.tl lst))

let rec rng n m =
  if n=m
  then n::[]
  else n::(rng (n+1) m)
  
let nqueens n = 
  let qu i qss = 
    foldr 
      (fun qs acc -> 
(*	append *)
	(map (fun k -> (i,k)::qs) (rng 1 n)) @@ acc)
      [] qss
  and isOk = 
    function [] -> true
      | x::xs -> andmap (fun q -> isSafe x q) xs
  in 
  let all_possible_solns = foldl qu [[]] (rng 1 n)
  and isValid lst = andmap isOk (tails lst)
  in List.hd (filter isValid all_possible_solns)
	
let _ = map (fun (x,y) -> Printf.printf "(%d,%d)\n" x y) (nqueens 8)

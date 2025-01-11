let rec last l = 
  match l with 
  | [] -> None 
  | [x] -> Some x
  | _ :: t -> last t 
;;
(* 'a list -> 'a option *)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t
;;

let rec last_two = function
  | [] | [_] -> None 
  | [x; y] -> Some (x, y)
  | _ :: t -> last_two t 
;;

let rec last_three = function
  | [] | [_] | [_; _] -> None
  | [x; y; z] -> Some (x, y, z)
  | _ :: t -> last_three t 
;;

let rec lastx n = function
  | [] -> None 
  | h :: t -> if n = 0 then Some h else lastx (n - 1) t;;
;;

let len list = 
  let aux acc = function
    | [] -> acc
    | _ :: t -> aux (acc + 1) t 
;;

let rev list =
  let rec aux acc = function
    | [] -> acc 
    | h :: t -> aux (h :: acc) t 
in
  aux [] list
;;

let rec rev = function
  | [] -> []
  | h :: t -> rev t @ [h] (* Note: This uses @, which is not optimal for efficiency *)

(* or for efficiency *)
let rec rev_helper acc = function
  | [] -> acc
  | h :: t -> rev_helper (h :: acc) t

let rev l = rev_helper [] l

(* Then use this in place of List.rev in the partition return *)

(* split in half *)
let split list =
  let rec helper left right count = function
    | [] -> (left, right)
    | h :: t -> 
        if count mod 2 = 0 
        then helper (h :: left) right (count + 1) t
        else helper left (h :: right) (count + 1) t
  in
  helper [] [] 0 list
;;

let rec compress = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | x -> x
;;


;;
let rec quicksort lst =
  | [] -> []
  | pivot :: rest ->
        let rec partition left right = function
              | [] -> (left, right)
              | x :: xs -> 
                  if x <= pivot then 
                      partition (x :: left) right xs
                  else
                      partition left (x :: right) xs
        in 
        let left, right = partition [] [] rest in 
        quicksort left
        @ [pivot]
        @ quicksort right
;;
(* sorts a list from smallest to largest *)




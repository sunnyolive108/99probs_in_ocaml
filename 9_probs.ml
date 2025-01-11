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
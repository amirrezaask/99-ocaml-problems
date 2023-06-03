let rec last = function
    | [] -> None
    | h :: [] -> Some h
    | _ :: tl -> last tl
;;


let rec last_two = function
    | [] -> None 
    | a :: [] -> None
    | a :: b :: [] -> Some (a, b)
    | _ :: tl -> last_two tl
;;


let rec nth l n =
    match l with
    | [] -> None
    | a :: tl -> match n with
        | 0 -> Some a
        | _ -> nth tl (n - 1)
;;


let rec length =  function
    | [] -> 0
    | _ :: tl -> 1 + (length tl)
;;


let rec reverse = function
    | [] -> []
    | h :: tl -> (reverse tl) @ [h]
;;



let rec palindrome l =
    (reverse l) = l
;;

type 'a node =
      | One of 'a 
      | Many of 'a node list

let rec flatten l = 
    let rec flat = function
        | One o -> [o]
        | Many os -> List.concat (List.map flat os)
    in
    match l with
    | [] -> []
    | n :: tl -> (flat n) @ (flatten tl)
;;



    



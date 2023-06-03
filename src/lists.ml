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



    



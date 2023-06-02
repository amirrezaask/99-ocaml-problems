let rec tail_of_a_list = function
    | [] -> None
    | h :: [] -> Some h
    | _ :: tl -> tail_of_a_list tl
;;


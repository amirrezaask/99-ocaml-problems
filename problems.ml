let rec last = function
  | [] -> None
  | h :: [] -> Some h
  | _ :: tl -> last tl
;;

let rec last_two = function
  | [] -> None
  | _ :: [] -> None
  | [ a; b ] -> Some (a, b)
  | _ :: tl -> last_two tl
;;

let rec nth l n =
  match l with
  | [] -> None
  | a :: tl ->
    (match n with
     | 0 -> Some a
     | _ -> nth tl (n - 1))
;;

let rec length = function
  | [] -> 0
  | _ :: tl -> 1 + length tl
;;

let rec reverse = function
  | [] -> []
  | h :: tl -> reverse tl @ [ h ]
;;

let is_palindrome l = reverse l = l

type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten l =
  let rec flat = function
    | One o -> [ o ]
    | Many os -> List.concat (List.map flat os)
  in
  match l with
  | [] -> []
  | n :: tl -> flat n @ flatten tl
;;

let compress l =
  let rec aux l out =
    match l with
        | [] -> out
        | x :: y :: tl when x = y -> aux tl (x :: out)
        | x :: tl -> aux tl (x :: out)
  in
  aux l []
;;

let () =
  let _ = last [ "a"; "b"; "c"; "d" ] in
  let _ = last_two [ "a"; "b"; "c"; "d" ] in
  let _ = nth [ "a"; "b"; "c"; "d"; "e" ] 2 in
  let _ = length [ "a"; "b"; "c" ] in
  let _ = reverse [ "a"; "b"; "c" ] in
  let _ = is_palindrome [ "x"; "a"; "m"; "a"; "x" ] in
  let _ =
    flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  in
  let _ =
    compress
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  in
  ()
;;

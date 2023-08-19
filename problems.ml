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
    | x :: y :: tl when x = y ->
      (match out with
       | a :: _ when a = x -> aux tl out
       | _ -> aux tl (x :: out))
    | x :: tl -> aux tl (x :: out)
  in
  aux l [] |> reverse
;;

let pack l =
  let rec aux l buffer out =
    match l with
    | [] -> if length buffer = 0 then out else buffer :: out
    | x :: t ->
      (match buffer with
       | [] -> aux t [ x ] out
       | y :: _ -> if x = y then aux t (x :: buffer) out else aux t [ x ] (buffer :: out))
  in
  aux l [] [] |> reverse
;;

(* No list usage *)
let pack2 l =
  let rec aux l out =
    match l with
    | [] -> out
    | x :: t ->
      (match out with
       | [] -> aux t ([ x ] :: out)
       | buffer :: tl ->
         (match buffer with
          | [] -> [ x ] :: out
          | y :: buffertl when x = y -> aux t ((x :: y :: buffertl) :: tl)
          | _ -> aux t ([ x ] :: out)))
  in
  aux l [] |> reverse
;;

let print_list l =
  let open Stdio in
  let rec aux l =
    match l with
    | x :: xs ->
      printf "%s," x;
      aux xs
    | [] -> ()
  in
  printf "[";
  aux l;
  printf "]"
;;

let print_tuple (a, b) =
  Stdio.printf "%d, %s\n" a b

let encode l =
  let rec aux list last counter out =
    match list with
      | [] -> (match last with
        | Some c -> (counter, c) :: out
        | None -> out)
      | h :: tl ->
        match last with
        | None -> aux tl (Some h) 1 out
        | Some c when c = h -> aux tl (Some h) (counter+1) out
        | Some c -> aux tl (Some h) 1 ((counter, c) :: out)

  in

  aux l None 0 [] |> reverse

let () =
  let _ = last [ "a"; "b"; "c"; "d" ] in
  let _ = last_two [ "a"; "b"; "c"; "d" ] in
  let _ = nth [ "a"; "b"; "c"; "d"; "e" ] 2 in
  let _ = length [ "a"; "b"; "c" ] in
  let _ = reverse [ "a"; "b"; "c" ] in
  let _ = is_palindrome [ "x"; "a"; "m"; "a"; "x" ] in
  let _ = flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ] in
  let _ = compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ] in
  let _ = pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ] in
  let _ = pack2 [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ] in
  let _ = encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] |> List.map print_tuple in
  ()
;;

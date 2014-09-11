let enum ~sep l f =
    let first = ref true in
    List.fold_left
      (fun a b ->
          a
          ^ (if !first then (first := false; "") else sep)
          ^ (f b))
      "" l

let empty = function
    | [] -> true
    | _ -> false

let add l v =
    l := [v] @ !l

let iter l f = List.iter f l
let fold_left d l f = List.fold_left f d l
let find l f = List.find f l

let next l =
    let hd = List.hd !l in
    l := (List.tl !l);
    hd

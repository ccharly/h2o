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

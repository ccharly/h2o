let enum ?(predicate = (fun _ -> true)) ~sep ?(fallback_sep = "") l f =
    let i = ref 0 in
    let max = List.length l in
    List.fold_left
      (fun a b ->
        let sep =
          if not (!i = max) then
            if predicate b
            then sep
            else fallback_sep
          else ""
         in
         incr i;
          a
          ^ (f b)
          ^ sep)
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

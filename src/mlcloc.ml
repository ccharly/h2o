let count_eol s =
    let i = ref 0 in
    String.iter (fun c -> if c = '\n' then incr i) s;
    !i

let current_line = ref 1

let (!+) n =
    current_line := !current_line + n

let get_current_line () =
    !current_line

let reset () =
    current_line := 1

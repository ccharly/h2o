module Map = Map.Make(struct type t = string let compare = compare end)

module Make(M : sig
    type value

    val has_name : value -> bool
    val get_name : value -> string
    val default : (value -> string) -> value -> string
end) = struct

  type builder = (M.value -> string) -> M.value -> string

  type kind = [
      | `String of string
      | `Regexp of string
  ]

  let r rr = `Regexp rr
  let s ss = `String ss

  let string_c, regexp_c =
      ref (Map.empty : builder Map.t),
      ref ([] : (Str.regexp * builder) list)

  let register (kind, f) = match kind with
      | `String s -> string_c := Map.add s f !string_c
      | `Regexp r -> regexp_c := (Str.regexp r, f)::!regexp_c

  let rec build v =
      if M.has_name v then
          let n = M.get_name v in
          try
            let f = Map.find n !string_c in
            f build v
          with Not_found ->
            try
                let (_,f) =
                  List.find
                    (fun (r, _) -> Str.string_match r n 0)
                    !regexp_c
                in
                f build v
            with Not_found -> M.default build v
      else M.default build v
end

open Printf

let depth = ref 0

let default_node_builder node_builder kind =
    let d = !depth in
    let rec prefix = function
        | n when n <= 0 -> ""
        | n -> " "^(prefix (n-1))
    in
    let prefix = prefix (d * 2) in
    prefix ^ (match kind with
    | `Node (name, attrs, children) ->
            sprintf "%s ~a:[%s] [\n%s%s];"
                name
                (H2o_list.enum ~sep:"; " attrs H2o_attr_printer.build)
                (H2o_list.enum ~sep:"\n" children
                    (fun node ->
                        incr depth; let node = node_builder node in decr depth; node))
                (if H2o_list.empty children then prefix else "\n" ^ prefix)
    | `Data d ->
            sprintf "pcdata %S;" d
    | `Comment c ->
            sprintf "(* %s *)" c
    | `Eof -> ""
    )

module C = H2o_cache_builder.Make(struct
    type value = H2o_ast.t

    let has_name = function
        | `Node _ -> true
        | _ -> false

    let get_name = function
        | `Node (n, _, _) -> n
        | _ -> failwith "get_name"

    let default = default_node_builder
end)

let () =
    let open C in
    List.iter C.register [
        (* Add specializations here *)
    ]

let build = C.build
let print_attr v =
    printf "%s\n" (build v)

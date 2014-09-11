open Printf

let registered = ref []

let register obj =
    H2o_list.add registered obj

let cache_find ~default n =
    try H2o_list.find !registered (fun obj -> obj#matches n)
    with Not_found -> default

class type node_t = object
    (* Returns true if the object must be registered to the cache *)
    method register: bool

    (* Must be true if an element does not expect any children (such as `img`) * *)
    method nullary: bool

    (* Returns true if the given name matches, otherwise returns false *)
    method matches: string -> bool

    (* Returns a string version of the element *)
    method name: string -> string

    (* Returns the equivalent eliom code for an attribute *)
    method on_attr: (string * string) -> H2o_attr_printer.t

    (* Returns the equivalent eliom code for all attributes *)
    method on_attrs: (string * string) list -> string

    (* Returns data *)
    method on_data: string -> string

    (* Returns commentaries *)
    method on_comment: string -> string

    (* Returns empty  *)
    method on_eof: string

    (* Prefix before children *)
    method on_begin: string

    (* Suffix after children *)
    method on_end: string
end

class node : node_t = object(this)
    method register = true

    method nullary = false

    method matches _ = true

    method name n = n

    method on_attr ((n, v) as attr) =
        `a attr

    method on_attrs attrs =
        let al = ref [] in
        let ll = ref [] in
        H2o_list.iter attrs
            (fun a ->
                let a = this#on_attr a in
                if H2o_attr_printer.is_label a
                then H2o_list.add ll a
                else H2o_list.add al a);
        let a_label =
            if H2o_list.empty !al then ""
            else
                H2o_syntax.make_label ~is_list:true
                   "a" (H2o_list.enum ~sep:"; " !al H2o_attr_printer.build)
        in
        sprintf "%s %s"
            a_label
            (H2o_list.enum ~sep:" "  !ll H2o_attr_printer.build)

    method on_data d =
        sprintf "pcdata %S;" d

    method on_comment c =
        sprintf "(* %s *)" c

    method on_eof =
        ""

    method on_begin =
        ""

    method on_end =
        ""

    initializer
        if this#register then
            register (this :> node_t)
end

class node_default = object
    inherit node

    method register = false
end

class node_regexp r = object
    inherit node

    (* The regexp to match *)
    val reg = Str.regexp r

    method matches name =
        Str.string_match reg name 0
end

class node_string n = object
    inherit node

    (* The string to match *)
    val n = n

    method matches name =
        (n = name)
end

let () =
    (* Register all *)
    (* script *)
    ignore (object
        inherit node_string "script"

        method on_data d = d
        method on_begin = "pcdata \""
        method on_end = "\""
    end);
    (* img *)
    ignore (object
        inherit node_string "img"

        method nullary = true
    end);
    (* End of register *)
    ()

let default_obj = new node_default

let build kind =
    let rec aux parent kind =
        let prefix = H2o_syntax.make_pad () in
        prefix ^ (match kind with
        | `Node (name, attrs, children) ->
                let obj = cache_find ~default:default_obj name in
                if obj#nullary then
                    sprintf "%s %s ();"
                        (obj#name name)
                        (obj#on_attrs attrs)
                else
                    sprintf "%s %s [%s\n%s%s%s];"
                        (obj#name name)
                        (obj#on_attrs attrs)
                        (obj#on_begin)
                        (H2o_list.enum ~sep:"\n" children
                            (fun c ->
                                H2o_syntax.incr_depth ();
                                let node = aux obj c in
                                H2o_syntax.decr_depth ();
                                node))
                        (if H2o_list.empty children then prefix else "\n" ^ prefix)
                        (obj#on_end)
        | `Data d ->
                parent#on_data d
        | `Comment c ->
                parent#on_comment c
        | `Eof ->
                parent#on_eof
        )
    in aux default_obj kind

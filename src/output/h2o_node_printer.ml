open Printf

module S = Set.Make(struct
    type t = H2o_attr_printer.t
    let compare = compare
end)

let registered = ref []

let register obj =
    H2o_list.add registered obj

let cache_find ~default n =
    try H2o_list.find !registered (fun obj -> obj#matches n)
    with Not_found -> default

type arg = [
    | `Arg
    | `List
    | `Unit
    | `Label of string
]

type kind = [
    | `Unary
    | `List
    | `Args of [
        | arg
        | `Expect of (string * arg)
        | `Option of (string * arg)
    ] list
]

class type node_t = object
    (* A string version of the object, debug purposes *)
    method to_string: string

    (* Returns true if the object must be registered to the cache *)
    method register: bool

    (* Returns the kind of the node *)
    method kind: kind

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

    (* Called on each child *)
    method on_child: default:(H2o_ast.t -> string) -> H2o_ast.t -> string

    method defaults: (string * string) list
end

class node : node_t = object(this)
    method to_string = "default:node"

    method register = true

    method kind = `List

    method matches _ = true

    method name n = n

    method on_attr ((n, v) as attr) =
        `a attr

    method on_attrs attrs =
        let al = ref S.empty in
        let ll = ref S.empty in
        H2o_list.iter (this#defaults @ attrs)
            (fun a ->
                let a = this#on_attr a in
                if H2o_attr_printer.is_label a
                then ll := S.add a !ll
                else al := S.add a !al);
        let al = S.elements !al in
        let ll = S.elements !ll in
        let a_label =
            if H2o_list.empty al then ""
            else
              let a_cnt =
                H2o_list.enum al
                ~predicate:H2o_attr_printer.is_not_ignored
                ~fallback_sep:" "
                ~sep:"; "
                H2o_attr_printer.build
              in
              H2o_syntax.make_label ~is_list:true "a" a_cnt
        in
        sprintf "%s %s"
            a_label
            (H2o_list.enum ~sep:" "  ll H2o_attr_printer.build)

    method on_data d =
        sprintf "pcdata %S" d

    method on_comment c =
        sprintf "(* %s *)" c

    method on_eof =
        ""

    method on_begin =
        ""

    method on_end =
        ""

    method on_child ~default node =
        default node

    method defaults : (string * string) list = []

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

    (* html *)
    ignore (object
        inherit node_string "html"
        method kind = `Args [ `Arg; `Arg; ]
    end);

    (* head *)
    ignore (object
        inherit node_string "head"
        method kind = `Args [ `Arg; `List; ]
    end);

    (* title *)
    ignore (object
        inherit node_string "title"
        method kind = `Args [ `Arg ]
    end);

    (* table *)
    ignore (object
        inherit node_string "table"

        method name _ = "tablex"

        method kind = `Args [
            `Expect ("thead", `Label "thead");
            `Option ("tfoot", `Label "tfoot");
            `List
        ]
    end);

    (* hr *)
    ignore (object
        inherit node_string "hr"
        method kind = `Unary
    end);

    (* br *)
    ignore (object
        inherit node_string "br"
        method kind = `Unary
    end);

    (* a *)
    ignore (object
        inherit node_string "a"
        method name _ = "Raw.a"
    end);

    (* link *)
    ignore (object
        inherit node_string "link"
        method kind = `Unary

        method on_attr (n, v) = match n with
        | "rel" -> `label (n, v)
        | "href" -> `label (n, v)
        | _ -> `a (n, v)

    end);

    (* meta *)
    ignore (object
        inherit node_string "meta"
        method kind = `Unary
    end);

    (* script *)
    ignore (object
        inherit node_string "script"
        method kind = `Args [ `Arg ]

        method on_begin = "pcdata \""
        method on_end = "\""
    end);

    (* img *)
    ignore (object
        inherit node_string "img"

        method on_attr (n, v) = match n with
        | "src" -> `label (n, v)
        | "alt" -> `label (n, v)
        | _ -> `a (n, v)

        method kind = `Unary

        (* Temporary add src as optional (this fix bootstrap.html *)
        method defaults = [
            "src", "";
        ]
    end);

    (* button *)
    ignore (object
        inherit node_string "button"

        method on_attr (n, v) = match n with
        | "type" -> `label ("button_type", v)
        | _ -> `a (n, v)

        method defaults = [
            "type", "button";
        ]

    end);

    (* input *)
    ignore (object
        inherit node_string "input"

        method on_attr (n, v) = match n with
        | "type" -> `label ("input_type", v)
        | _ -> `a (n, v)

        method kind = `Unary
    end);
    (* End of register *)
    ()

(* Some helpers *)
let is_comment = function
  | `Comment _ -> true
  | _ -> false

let default_obj = new node_default

(* FIXME TODO
 *
 * This function need some clean up or a lots of commentaries, this is quite
 * difficult to understand right now. This is because of the `Args variants,
 * this could maybe be improved..
 *
 * *)
let build kind =
    let rec aux ?(with_prefix = true) parent kind =
        let prefix = H2o_syntax.make_pad () in
        (if with_prefix then prefix else "")
        ^ (match kind with
        | `Node (name, attrs, children) -> begin
                let obj = cache_find ~default:default_obj name in
                match obj#kind with
                | `Unary ->
                    sprintf "%s %s ()"
                        (obj#name name)
                        (obj#on_attrs attrs)
                | `Args args -> (* TODO use number of args *)
                    sprintf "%s %s\n%s"
                        (obj#name name)
                        (obj#on_attrs attrs)
                        (let children = ref children in
                         let args =
                             let parent = obj in
                             let rec make_arg ?(with_depth = true) ~children arg =
                                 if with_depth then
                                     H2o_syntax.incr_depth ();
                                 let prefix = H2o_syntax.make_pad () in
                                 let default = aux parent ~with_prefix:false in
                                 let node = match arg with
                                 | `Option _ -> failwith "unexpected `Option"
                                 | `Expect _ -> failwith "unexpected `Expect"
                                 | `Label lbl ->
                                         if H2o_list.empty !children
                                         then failwith (sprintf "tag:%s: argument expected" name)
                                         else
                                             let c = H2o_list.next children in
                                             let node = parent#on_child ~default c in
                                             sprintf "%s~%s:(%s)" prefix lbl node
                                 | `Arg ->
                                         if H2o_list.empty !children
                                         then failwith (sprintf "tag:%s: argument expected" name)
                                         else
                                             let c = H2o_list.next children in
                                             let node = parent#on_child ~default c in
                                             sprintf "%s(%s)" prefix node
                                 | `List ->
                                         let children' = !children in
                                         sprintf "%s[%s]" prefix
                                         (H2o_list.enum ~sep:"\n" !children
                                             (fun c ->
                                                 H2o_syntax.incr_depth ();
                                                 let default = aux parent in
                                                 let node = parent#on_child ~default c in
                                                 let node =
                                                   if is_comment c
                                                   then node
                                                   else node ^ ";"
                                                 in
                                                 H2o_syntax.decr_depth ();
                                                 H2o_list.next children;
                                                 node))
                                 | `Unit -> sprintf "%s()" prefix
                                 in
                                 if with_depth then
                                     H2o_syntax.decr_depth ();
                                 node
                             in
                             let make_arg_with_expect arg = match arg with
                                | `Option (m, arg) ->
                                        if H2o_list.empty !children
                                        then failwith (sprintf "tag:%s: argument expected" name)
                                        else begin
                                            let c, cl =
                                                H2o_list.Opt.find_and_remove !children
                                                    (function
                                                        | `Node (n, _, _) ->
                                                                if n = m
                                                                then true
                                                                else false
                                                        | _ -> false)
                                            in
                                            match c with
                                            | None -> ""
                                            | Some c -> begin
                                                children := cl;
                                                make_arg ~children:(ref [c]) arg
                                            end
                                        end
                                | `Expect (m, arg) ->
                                        if H2o_list.empty !children
                                        then failwith (sprintf "tag:%s: argument expected" name)
                                        else begin
                                            let c, cl =
                                                try
                                                    H2o_list.find_and_remove !children
                                                        (function
                                                            | `Node (n, _, _) ->
                                                                    if n = m
                                                                    then true
                                                                    else false
                                                            | _ -> false)
                                                with Not_found ->
                                                    failwith (sprintf "tag:%s: tag:%s expected" name m)
                                            in
                                            children := cl;
                                            make_arg ~children:(ref [c]) arg
                                        end
                                | other -> make_arg ~children other
                             in
                             H2o_list.enum ~sep:"\n" args make_arg_with_expect
                         in
                         if not (H2o_list.empty !children)
                         then failwith (sprintf "tag:%s: unexpected child or children" name)
                         else args)
                | `List ->
                    sprintf "%s %s [%s\n%s%s%s]"
                        (obj#name name)
                        (obj#on_attrs attrs)
                        (obj#on_begin)
                        (H2o_list.enum ~sep:"\n" children
                            (fun c ->
                                H2o_syntax.incr_depth ();
                                let default = aux obj in
                                let node = obj#on_child ~default c in
                                let node =
                                  if is_comment c
                                  then node
                                  else node ^ ";"
                                in
                                H2o_syntax.decr_depth ();
                                node))
                        (if H2o_list.empty children then prefix else "\n" ^ prefix)
                        (obj#on_end)
        end
        | `Data d ->
                parent#on_data d
        | `Comment c ->
                parent#on_comment c
        | `Eof ->
                parent#on_eof
        )
    in aux default_obj kind

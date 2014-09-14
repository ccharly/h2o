open Printf

type t = [
    | `a of (string * string)
    | `label of (string * string)
]

let is_label = function
    | `label _ -> true
    | _ -> false

let registered = ref []

let register obj =
    H2o_list.add registered obj

let cache_find ~default n =
    try H2o_list.find !registered (fun obj -> obj#matches n)
    with Not_found -> default

class type attr_t = object
    method matches: string -> bool

    (* If [true], then the attribute will be commented
     * in the generated code *)
    method ignore: bool

    (* This method is called by both a_value and
     * l_value by default, use that one if you want to
     * overload both at the same time.
     *
     * First parameter is the name of the attribute, this
     * is sometimes used to generate the value (cf. data-.+)
     * *)
    method value: string -> string -> string
    method a_value: string -> string -> string
    method l_value: string -> string -> string

    method a_name: string -> string
    method l_name: string -> string

    method register: bool
end

class attr : attr_t = object(this)
    method matches _ = true

    method ignore = false

    method value _ v =
        sprintf "%S" v

    method a_value = this#value
    method l_value = this#value

    method a_name n =
        sprintf "a_%s" n

    method l_name n =
        n

    method register = true

    initializer
        if this#register then
            register (this :> attr_t)
end

class attr_default = object
    inherit attr

    method register = false
end

class attr_string n = object
    inherit attr

    val n = n

    method matches name =
        (n = name)
end

class attr_regexp r = object
    inherit attr

    val reg = Str.regexp r

    method matches name =
        Str.string_match reg name 0
end

let () =
    (* Register attributes to be ignored *)
    (* role *)
    ignore (object
        inherit attr_string "role"

        method ignore = true
    end);
    (* End of register *)
    ()

let () =
    (* Register all *)
    (* http-equiv *)
    ignore (object
        inherit attr_string "http-equiv"

        method a_name _ =
            "a_http_equiv"
    end);

    (* xml lang *)
    ignore (object
        inherit attr_string "lang"

        method a_name _ =
            "a_xml_lang"
    end);

    (* type *)
    ignore (object
        inherit attr_string "type"

        method value _ v =
            sprintf "`%s" (String.capitalize v)
    end);

    (* class *)
    ignore (object
        inherit attr_string "class"

        method value _ v =
            let v = Str.split (Str.regexp " ") v in
            let v = H2o_list.enum ~sep:"; " v (sprintf "%S") in
            sprintf "[%s]" v
    end);

    (* data-.* *)
    ignore (object
        inherit attr_regexp "data-.+"

        method a_name _ =
            "a_user_data"

        method value n v =
            let n = String.sub n 5 ((String.length n) - 5) in
            sprintf "%S %S" n v
    end);
    (* End of register *)
    ()

(* Default builder, won't be registered *)
let default_obj = new attr_default

let is_ignored kind =
    let n = match kind with
    | `a (n, _) -> n
    | `label (n, _) -> n
    in
    (cache_find ~default:default_obj n)#ignore

let is_not_ignored kind = not (is_ignored kind)

let build kind =
    let find_and_build n v build_name build_value build_attr =
        let obj = cache_find ~default:default_obj n in
        if obj#ignore
        then sprintf "(* %s=%s *)" n v
        else build_attr (build_name obj) (build_value obj)
    in
    match kind with
    | `a (n, v) ->
            find_and_build n v
                (fun obj -> obj#a_name n)
                (fun obj -> obj#a_value n v)
                (sprintf "%s %s")
    | `label (n, v) ->
            find_and_build n v
                (fun obj -> obj#l_name n)
                (fun obj -> obj#l_value n v)
                (sprintf "~%s:%s")

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

    (* Ignore every attributes that we don't know *)
    method ignore = true

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

    method ignore = false
    method matches name =
        (n = name)
end

class attr_regexp r = object
    inherit attr

    val reg = Str.regexp r

    method ignore = false
    method matches name =
        Str.string_match reg name 0
end

let () =
    (* Register all default attributes extracted from tyxml *)
    let make_attr n =
      ignore (new attr_string n)
    in
    List.iter make_attr [
      "autocomplete";
      "async";
      "autofocus";
      "autoplay";
      "challenge";
      "contenteditable";
      "contextmenu";
      "controls";
      "dir";
      "draggable";
      "form";
      "formaction";
      "formenctype";
      "formmethod";
      "formnovalidate";
      "formtarget";
      "hidden";
      "high";
      "icon";
      "ismap";
      "keytype";
      "list";
      "loop";
      "low";
      "max";
      "input_max";
      "min";
      "input_min";
      "novalidate";
      "open";
      "optimum";
      "pattern";
      "placeholder";
      "poster";
      "preload";
      "pubdate";
      "radiogroup";
      "required";
      "reversed";
      "sandbox";
      "spellcheck";
      "scoped";
      "seamless";
      "sizes";
      "span";
      "srcdoc*)";
      "srclang";
      "start";
      "step";
      "wrap";
      (*"class";*)
      (*"user_data";*)
      "id";
      "title";
      (*"xml_lang";*)
      (*
      "onabort";
      "onafterprint";
      "onbeforeprint";
      "onbeforeunload";
      "onblur";
      "oncanplay";
      "oncanplaythrough";
      "onchange";
      "onclick";
      "oncontextmenu";
      "ondblclick";
      "ondrag";
      "ondragend";
      "ondragenter";
      "ondragleave";
      "ondragover";
      "ondragstart";
      "ondrop";
      "ondurationchange";
      "onemptied";
      "onended";
      "onerror";
      "onfocus";
      "onformchange";
      "onforminput";
      "onhashchange";
      "oninput";
      "oninvalid";
      "onmousedown";
      "onmouseup";
      "onmouseover";
      "onmousemove";
      "onmouseout";
      "onmousewheel";
      "onoffline";
      "ononline";
      "onpause";
      "onplay";
      "onplaying";
      "onpagehide";
      "onpageshow";
      "onpopstate";
      "onprogress";
      "onratechange";
      "onreadystatechange";
      "onredo";
      "onresize";
      "onscroll";
      "onseeked";
      "onseeking";
      "onselect";
      "onshow";
      "onstalled";
      "onstorage";
      "onsubmit";
      "onsuspend";
      "ontimeupdate";
      "onundo";
      "onunload";
      "onvolumechange";
      "onwaiting";
      "onkeypress";
      "onkeydown";
      "onkeyup";
      "onload";
      "onloadeddata";
      "onloadedmetadata";
      "onloadstart";
      "onmessage";
      *)
      "version";
      "xmlns";
      "manifest";
      "cite";
      "xml_space";
      "accesskey";
      "charset";
      "accept_charset";
      "accept";
      (*"href";*)
      "hreflang";
      (*"rel";*)
      (*"tabindex";*)
      "mime_type";
      "datetime";
      "action";
      "checked";
      "cols";
      "enctype";
      "for";
      "for_list";
      "maxlength";
      "method";
      "multiple";
      "name";
      "rows";
      "selected";
      "size";
      "src";
      "input_type";
      "text_value";
      "int_value";
      "value";
      "float_value";
      "disabled";
      "readonly";
      (*"button_type";*)
      "command_type";
      "menu_type";
      "label";
      "align";
      "axis";
      "colspan";
      "headers";
      "rowspan";
      "scope";
      "summary";
      "border";
      "cellpadding";
      "cellspacing";
      "datapagesize";
      "rules";
      "char";
      "charoff";
      "alt";
      "height";
      "width";
      "shape";
      "coords";
      "usemap";
      "data";
      "codetype";
      "fs_rows";
      "fs_cols";
      "frameborder";
      "marginheight";
      "marginwidth";
      "scrolling";
      "target";
      "content";
      "http_equiv";
      "defer";
      "media";
      "style";
      "property";
    ]
    (* End of register *)

let to_uri =
    sprintf "(Eliom_content.Html5.F.uri_of_string (fun () -> %S))"

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

    (* src *)
    ignore (object
        inherit attr_string "src"

        method value _ v =
            to_uri v
    end);

    (* href *)
    ignore (object
        inherit attr_string "href"

        method value _ v =
            to_uri v
    end);

    (* rel *)
    ignore (object
        inherit attr_string "rel"

        method value _ v =
            sprintf "[%s]" (match v with
            | "alternate" -> "`Alternate"
            | "archives" -> "`Archives"
            | "author" -> "`Author"
            | "bookmark" -> "`Bookmark"
            | "external" -> "`External"
            | "first" -> "`First"
            | "help" -> "`Help"
            | "icon" -> "`Icon"
            | "index" -> "`Index"
            | "last" -> "`Last"
            | "license" -> "`License"
            | "next" -> "`Next"
            | "nofollow" -> "`Nofollow"
            | "noreferrer" -> "`Noreferrer"
            | "pingback" -> "`Pingback"
            | "prefetch" -> "`Prefetch"
            | "prev" -> "`Prev"
            | "search" -> "`Search"
            | "sidebar" -> "`Sidebar"
            | "stylesheet" -> "`Stylesheet"
            | "tag" -> "`Tag"
            | "up" -> "`Up"
            | other -> sprintf "`Other %S" other)
    end);

    (* class *)
    ignore (object
        inherit attr_string "class"

        method value _ v =
            let v = Str.split (Str.regexp " ") v in
            let v = H2o_list.enum ~sep:"; " v (sprintf "%S") in
            sprintf "[%s]" v
    end);

    (* tabindex *)
    ignore (object
        inherit attr_string "tabindex"

        method value _ = sprintf "(%s)"
    end);

    (* button_type *)
    ignore (object
        inherit attr_string "button_type"

        method value _ = function
            | "button" -> "`Button"
            | "submit" -> "`Submit"
            | "reset" -> "`Reset"
            | btn_type -> failwith (sprintf "don't know what to with button type: %S" btn_type)
    end);

    (* on(event) *)
    ignore (object
        inherit attr_regexp "on[a-zA-Z0-9_]+"

        method value n v =
            let ftype = match n with
            | "onclick" -> "(Dom_html.mouseEvent Js.t -> unit)"
            | _ -> failwith (sprintf "don't know what to with onevent: %s" n)
            in
            sprintf "{%s{ (fun _ _ -> ignore (Js.Unsafe.eval_string %S)) }}" ftype v
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

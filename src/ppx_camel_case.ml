open Ppxlib
open Ast_helper

let camel_of_snake snake =
  match String.split_on_char '_' snake with
  | [] -> "" (* this should not happen *)
  | h :: t ->
    String.lowercase_ascii h ^
    String.concat "" (List.map String.capitalize_ascii t)

let generate attr {txt; loc} =
  with_default_loc loc (fun () ->
    let name = {txt = Attribute.name attr; loc} in
    let f = camel_of_snake in
    let payload = PStr [Str.eval (Exp.constant (Const.string (f txt)))] in
    Attr.mk name payload
  )

let key_attr =
  Attribute.(declare "key" Context.label_declaration)
    Ast_pattern.(single_expr_payload (estring __))
    (fun x -> x)

let name_attr =
  Attribute.(declare "name" Context.constructor_declaration)
    Ast_pattern.(single_expr_payload (estring __))
    (fun x -> x)

let plugin_attr =
  Attribute.(declare "camel_case" Context.type_declaration)
    Ast_pattern.(pstr nil)
    (fun () -> ())

let mapper = object
  inherit Ast_traverse.map as super

  method! type_declaration t =
    match Attribute.get plugin_attr t with
    | None -> t (* nothing to do *)
    | Some _ ->
      Attribute.(remove_seen Context.type_declaration [T plugin_attr] t) |>
      super#type_declaration

  method! label_declaration ({pld_name; pld_attributes = a; _} as d) =
    match Attribute.get key_attr ~mark_as_seen:false d with
    | None -> {d with pld_attributes = generate key_attr pld_name :: a}
    | Some _ -> d

  method! constructor_declaration ({pcd_name; pcd_attributes = a; _} as d) =
    match Attribute.get name_attr ~mark_as_seen:false d with
    | None -> {d with pcd_attributes = generate name_attr pcd_name :: a}
    | Some _ -> d
end

let my_deriver =
  Driver.register_transformation
    "camel_case"
    ~preprocess_impl:(fun st -> mapper#structure st)

(* The house rules a reader of the source can check: run by [dune test], so
   CI and an agent find a broken rule as a failing test naming the line. *)

(* The libraries' directories. The command's [bin/] reads its environment,
   prints and exits, as a command does, and is held to the rules on opens
   and warnings alone. *)
let dirs = [ "src"; "postgres"; "migrate" ]
let command_dirs = [ "migrate/bin" ]

(* An [open] is refused but where the file names why: the walk over a
   query's shape pattern-matches the GADT it opens. *)
let allowed_opens = [ ("src/rowtype.ml", "Shape") ]
let never_prints = "a library never prints: log through Logs"

(* An identifier, dotted path included, and the reason it is refused. A
   name is looked up with a leading [Stdlib.] taken off, so
   [Stdlib.compare] is [compare]. *)
let banned =
  [
    ("failwith", "errors are values: return a result");
    ("invalid_arg", "errors are values: no .mli here documents a raise");
    ("Option.get", "partial: match on the option");
    ("Result.get_ok", "partial: match on the result");
    ("Result.get_error", "partial: match on the result");
    ("List.hd", "partial: match on the list");
    ("List.tl", "partial: match on the list");
    ("List.nth", "partial: use List.nth_opt");
    ("Obj.magic", "unsafe");
    ("compare", "polymorphic: use Int.compare, String.compare, ...");
    ("Sys.getenv", "a library reads no environment: take it as an argument");
    ("Sys.getenv_opt", "a library reads no environment: take it as an argument");
    ("Unix.getenv", "a library reads no environment: take it as an argument");
    ("Unix.environment", "a library reads no environment");
    ("print_string", never_prints);
    ("print_endline", never_prints);
    ("print_bytes", never_prints);
    ("print_char", never_prints);
    ("print_int", never_prints);
    ("print_float", never_prints);
    ("print_newline", never_prints);
    ("prerr_string", never_prints);
    ("prerr_endline", never_prints);
    ("prerr_bytes", never_prints);
    ("prerr_char", never_prints);
    ("prerr_int", never_prints);
    ("prerr_float", never_prints);
    ("prerr_newline", never_prints);
    ("stdout", never_prints);
    ("stderr", never_prints);
    ("Printf.printf", never_prints);
    ("Printf.eprintf", never_prints);
    ("Format.printf", never_prints);
    ("Format.eprintf", never_prints);
    ("Format.print_string", never_prints);
    ("Format.std_formatter", never_prints);
    ("Format.err_formatter", never_prints);
    ("exit", "a library never exits");
  ]

let read path = In_channel.with_open_bin path In_channel.input_all

(* The source with comments, string literals and character literals blanked
   to spaces, newlines kept, so what is left is code and lines still count. *)
let code_of s =
  let n = String.length s in
  let out = Bytes.of_string s in
  let blank i = if not (Char.equal s.[i] '\n') then Bytes.set out i ' ' in
  let rec string i =
    if i >= n then i
    else (
      blank i;
      match s.[i] with
      | '\\' when i + 1 < n ->
          blank (i + 1);
          string (i + 2)
      | '"' -> i + 1
      | _ -> string (i + 1))
  in
  (* {id|...|id}, with [id] lowercase letters or underscores. *)
  let quoted i =
    let j = ref (i + 1) in
    while !j < n && match s.[!j] with 'a' .. 'z' | '_' -> true | _ -> false do
      incr j
    done;
    if !j < n && Char.equal s.[!j] '|' then (
      let close = "|" ^ String.sub s (i + 1) (!j - i - 1) ^ "}" in
      let k = ref (!j + 1) in
      while
        !k + String.length close <= n
        && not (String.equal (String.sub s !k (String.length close)) close)
      do
        incr k
      done;
      let stop = Int.min n (!k + String.length close) in
      for x = i to stop - 1 do
        blank x
      done;
      Some stop)
    else None
  in
  (* 'x', '\n', '\'' and '\123'; a type variable's quote is left alone. *)
  let char i =
    if i + 2 < n && Char.equal s.[i + 1] '\\' then (
      let j = ref (i + 2) in
      while !j < n && not (Char.equal s.[!j] '\'') do
        incr j
      done;
      if !j - i <= 5 then (
        for x = i to !j do
          blank x
        done;
        Some (!j + 1))
      else None)
    else if i + 2 < n && Char.equal s.[i + 2] '\'' then (
      for x = i to i + 2 do
        blank x
      done;
      Some (i + 3))
    else None
  in
  let rec comment depth i =
    if i >= n then i
    else if i + 1 < n && Char.equal s.[i] '(' && Char.equal s.[i + 1] '*' then (
      blank i;
      blank (i + 1);
      comment (depth + 1) (i + 2))
    else if i + 1 < n && Char.equal s.[i] '*' && Char.equal s.[i + 1] ')' then (
      blank i;
      blank (i + 1);
      if depth = 1 then i + 2 else comment (depth - 1) (i + 2))
    else if Char.equal s.[i] '"' then (
      blank i;
      comment depth (string (i + 1)))
    else (
      blank i;
      comment depth (i + 1))
  in
  let rec go i =
    if i >= n then ()
    else
      match s.[i] with
      | '(' when i + 1 < n && Char.equal s.[i + 1] '*' -> go (comment 0 i)
      | '"' ->
          blank i;
          go (string (i + 1))
      | '{' -> ( match quoted i with Some j -> go j | None -> go (i + 1))
      | '\'' -> ( match char i with Some j -> go j | None -> go (i + 1))
      | _ -> go (i + 1)
  in
  go 0;
  Bytes.to_string out

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' | '.' -> true
  | _ -> false

(* Every identifier, dotted path included, with its line. A field access
   ([t.open]) begins with a lowercase name and so never matches a banned
   path, and a path never ends in a dot. A labelled argument's label
   ([~stdout:]) names no value and is left out. *)
let identifiers code =
  let n = String.length code in
  let rec go i line acc =
    if i >= n then List.rev acc
    else if Char.equal code.[i] '\n' then go (i + 1) (line + 1) acc
    else if is_ident_char code.[i] && not (Char.equal code.[i] '.') then (
      let j = ref i in
      while !j < n && is_ident_char code.[!j] do
        incr j
      done;
      let word = String.sub code i (!j - i) in
      let word =
        if String.ends_with ~suffix:"." word then
          String.sub word 0 (String.length word - 1)
        else word
      in
      let label =
        i > 0
        && (Char.equal code.[i - 1] '~' || Char.equal code.[i - 1] '?')
        && !j < n
        && Char.equal code.[!j] ':'
      in
      go !j line (if label then acc else (line, word) :: acc))
    else go (i + 1) line acc
  in
  go 0 1 []

(* Every source file with the given suffix, as a path from the root. *)
let sources ?(dirs = dirs) suffix =
  List.concat_map
    (fun dir ->
      Sys.readdir (Filename.concat ".." dir)
      |> Array.to_list
      |> List.filter (String.ends_with ~suffix)
      |> List.sort String.compare
      |> List.map (Filename.concat dir))
    dirs

let read_source file = read (Filename.concat ".." file)

(* Every banned identifier in [code], with its line and the reason. *)
let refusals code =
  let stdlib = "Stdlib." in
  identifiers code
  |> List.filter_map (fun (line, word) ->
      let name =
        if String.starts_with ~prefix:stdlib word then
          String.sub word (String.length stdlib)
            (String.length word - String.length stdlib)
        else word
      in
      List.assoc_opt name banned |> Option.map (fun why -> (line, word, why)))

let no_banned_identifier () =
  let found =
    List.concat_map
      (fun file ->
        refusals (code_of (read_source file))
        |> List.map (fun (line, word, why) ->
            Printf.sprintf "%s:%d: %s -- %s" file line word why))
      (sources ".ml" @ sources ".mli")
  in
  Alcotest.(check (list string)) "banned identifiers" [] found

(* The names OCAML.md's *Shared shapes* has every library's test_style
   refuse, in its words. *)
let shared_rule =
  [
    "failwith";
    "invalid_arg";
    "Option.get";
    "Result.get_ok";
    "Result.get_error";
    "List.hd";
    "List.tl";
    "List.nth";
    "Obj.magic";
    "compare";
    "Stdlib.compare";
    "Sys.getenv";
    "Sys.getenv_opt";
    "Unix.getenv";
    "Unix.environment";
    "exit";
    "print_string";
    "print_endline";
    "print_bytes";
    "print_char";
    "print_int";
    "print_float";
    "print_newline";
    "prerr_string";
    "prerr_endline";
    "prerr_bytes";
    "prerr_char";
    "prerr_int";
    "prerr_float";
    "prerr_newline";
    "stdout";
    "stderr";
    "Printf.printf";
    "Printf.eprintf";
    "Format.printf";
    "Format.eprintf";
    "Format.print_string";
    "Format.std_formatter";
    "Format.err_formatter";
  ]

let refuses_the_shared_rule () =
  let missed =
    List.concat_map
      (fun name ->
        List.filter
          (fun use -> List.is_empty (refusals ("let _ = " ^ use)))
          (if String.starts_with ~prefix:"Stdlib." name then [ name ]
           else [ name; "Stdlib." ^ name ]))
      shared_rule
  in
  Alcotest.(check (list string)) "names the rule refuses, let through" [] missed;
  let beyond =
    List.filter_map
      (fun (name, _) -> if List.mem name shared_rule then None else Some name)
      banned
  in
  Alcotest.(check (list string)) "names refused beyond the rule" [] beyond

(* A label is no use of the value it is named like; a punned one, [~exit],
   may be, since it passes whatever [exit] is in scope. *)
let a_label_is_no_use () =
  let names code = List.map (fun (_, word, _) -> word) (refusals code) in
  Alcotest.(check (list string))
    "labels" []
    (names "let _ = f ~stdout:a ?stderr:b ~exit:c");
  Alcotest.(check (list string)) "a punned label" [ "exit" ] (names "f ~exit")

(* The names in a dune file's [(libraries ...)] fields, comments taken off.
   A field is read up to its closing bracket, since [src/dune] selects
   nothing. *)
let libraries_of dune =
  let words =
    String.split_on_char '\n' dune
    |> List.concat_map (fun line ->
        match String.split_on_char ';' line with
        | code :: _ -> String.split_on_char ' ' code
        | [] -> [])
    |> List.concat_map (String.split_on_char '(')
    |> List.concat_map (fun w ->
        match String.split_on_char ')' w with
        | [ w ] -> [ w ]
        | w :: _ -> [ w; ")" ]
        | [] -> [])
    |> List.filter (fun w -> not (String.equal w ""))
  in
  let rec field = function
    | "libraries" :: rest -> names rest
    | _ :: rest -> field rest
    | [] -> []
  and names = function
    | ")" :: rest -> field rest
    | name :: rest -> name :: names rest
    | [] -> []
  in
  field words

let rowtype_links_ptime_and_uuidm_alone () =
  Alcotest.(check (list string))
    "src/dune's libraries" [ "ptime"; "uuidm" ]
    (libraries_of (read_source "src/dune"))

(* Every local open, [M.(...)], [M.[...]] or [M.{...}], with its line and
   module: a bracket after a path whose last name is capitalised. An array's
   or a string's index follows a value's name, which is not. An operator
   reached by its path, [M.( + )], is matched too; bind it to a name. *)
let local_opens code =
  let n = String.length code in
  let is_name_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
    | _ -> false
  in
  let rec go i line acc =
    if i + 1 >= n then List.rev acc
    else if Char.equal code.[i] '\n' then go (i + 1) (line + 1) acc
    else if
      Char.equal code.[i] '.'
      && match code.[i + 1] with '(' | '[' | '{' -> true | _ -> false
    then (
      let j = ref i in
      while !j > 0 && is_name_char code.[!j - 1] do
        decr j
      done;
      match code.[!j] with
      | 'A' .. 'Z' when !j < i ->
          go (i + 1) line ((line, String.sub code !j (i - !j)) :: acc)
      | _ -> go (i + 1) line acc)
    else go (i + 1) line acc
  in
  go 0 1 []

let no_open_but_the_named () =
  let allowed file name =
    List.exists
      (fun (f, m) -> String.equal f file && String.equal m name)
      allowed_opens
  in
  let found =
    List.concat_map
      (fun file ->
        let code = code_of (read_source file) in
        let local =
          List.filter_map
            (fun (line, name) ->
              if allowed file name then None
              else
                Some
                  (Printf.sprintf "%s:%d: %s.( ... ) -- use a module alias" file
                     line name))
            (local_opens code)
        in
        let rec opens = function
          | (line, "open") :: (_, name) :: rest ->
              if allowed file name then opens rest
              else
                Printf.sprintf "%s:%d: open %s -- use a module alias" file line
                  name
                :: opens rest
          | _ :: rest -> opens rest
          | [] -> []
        in
        local @ opens (identifiers code))
      (sources ~dirs:(dirs @ command_dirs) ".ml" @ sources ".mli")
  in
  Alcotest.(check (list string)) "opens" [] found

let no_silenced_warning () =
  let found =
    List.concat_map
      (fun file ->
        let code = code_of (read_source file) in
        String.split_on_char '\n' code
        |> List.mapi (fun i l -> (i + 1, l))
        |> List.filter_map (fun (line, l) ->
            let rec has i =
              i + 8 <= String.length l
              && (String.equal (String.sub l i 8) "@warning" || has (i + 1))
            in
            if has 0 then
              Some
                (Printf.sprintf
                   "%s:%d: a silenced warning is a code shape to fix" file line)
            else None))
      (sources ~dirs:(dirs @ command_dirs) ".ml" @ sources ".mli")
  in
  Alcotest.(check (list string)) "silenced warnings" [] found

let every_module_has_an_interface () =
  let missing =
    sources ".ml"
    |> List.filter (fun ml ->
        not (Sys.file_exists (Filename.concat ".." (ml ^ "i"))))
    |> List.map (fun ml -> ml ^ " has no .mli")
  in
  Alcotest.(check (list string)) "modules without an interface" [] missing

let () =
  Alcotest.run "style"
    [
      ( "libraries",
        [
          Alcotest.test_case "no banned identifier" `Quick no_banned_identifier;
          Alcotest.test_case "refuses the shared rule" `Quick
            refuses_the_shared_rule;
          Alcotest.test_case "a label is no use" `Quick a_label_is_no_use;
          Alcotest.test_case "rowtype links ptime and uuidm alone" `Quick
            rowtype_links_ptime_and_uuidm_alone;
          Alcotest.test_case "no open but the named" `Quick
            no_open_but_the_named;
          Alcotest.test_case "no silenced warning" `Quick no_silenced_warning;
          Alcotest.test_case "every module has an interface" `Quick
            every_module_has_an_interface;
        ] );
    ]

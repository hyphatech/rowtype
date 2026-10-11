(* The house rules a reader of the source can check: run by [dune test], so
   CI and an agent find a broken rule as a failing test naming the line. *)

(* The libraries' directories, and the executables': the command's [bin/]
   and the tests, which read their environment, print and exit, as an
   executable does, and are held to every other rule on names, and to the
   rules on opens, warnings and time. *)
let dirs = [ "src"; "postgres"; "migrate" ]
let command_dirs = [ "migrate/bin" ]
let test_dirs = [ "test" ]

(* An [open] is refused but where the file names why: the walk over a
   query's shape pattern-matches the GADT it opens. A span is written as
   OCAML.md writes it, [Mtime.Span.(5 * s)], a module of operators opened
   around the one expression that uses them, in any file. *)
let allowed_opens = [ ("src/rowtype.ml", "Shape") ]
let operator_modules = [ "Mtime.Span" ]
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
   ([t.open]) begins with a lowercase name and so never matches a refused
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
let stdlib = "Stdlib."

let without_stdlib name =
  if String.starts_with ~prefix:stdlib name then
    String.sub name (String.length stdlib)
      (String.length name - String.length stdlib)
  else name

(* The names in OCAML.md's [refused] block, one a line, each with a leading
   [Stdlib.] taken off: the one list every Hypha library refuses, so this
   repository keeps none of its own. *)
let refused_in markdown =
  let rec skip = function
    | line :: rest ->
        if String.equal (String.trim line) "```refused" then take rest
        else skip rest
    | [] -> []
  and take = function
    | line :: rest ->
        let name = String.trim line in
        if String.equal name "```" then []
        else if String.equal name "" then take rest
        else without_stdlib name :: take rest
    | [] -> []
  in
  skip (String.split_on_char '\n' markdown)

let refused () =
  match refused_in (read "../OCAML.md") with
  | [] -> Alcotest.fail "OCAML.md has no ```refused block, or an empty one"
  | names -> names

(* What an executable does and a library never: read its environment, print
   and exit. *)
let what_an_executable_does name =
  List.exists
    (fun prefix -> String.starts_with ~prefix name)
    [ "print_"; "prerr_"; "Printf."; "Format." ]
  || List.mem name
       [
         "stdout";
         "stderr";
         "exit";
         "Sys.getenv";
         "Sys.getenv_opt";
         "Unix.getenv";
         "Unix.environment";
       ]

(* Every refused identifier in [code], with its line. *)
let refusals ~executable names code =
  identifiers code
  |> List.filter (fun (_, word) ->
      let name = without_stdlib word in
      List.mem name names && not (executable && what_an_executable_does name))

let no_refused_name () =
  let names = refused () in
  let found ~executable files =
    List.concat_map
      (fun file ->
        refusals ~executable names (code_of (read_source file))
        |> List.map (fun (line, word) ->
            Printf.sprintf "%s:%d: %s -- refused by OCAML.md" file line word))
      files
  in
  let executables = command_dirs @ test_dirs in
  Alcotest.(check (list string))
    "refused names" []
    (found ~executable:false (sources ".ml" @ sources ".mli")
    @ found ~executable:true
        (sources ~dirs:executables ".ml" @ sources ~dirs:executables ".mli"))

let the_refused_block_is_read () =
  let names = refused () in
  List.iter
    (fun name ->
      Alcotest.(check bool) (name ^ " is refused") true (List.mem name names))
    [ "failwith"; "compare"; "Unix.sleepf"; "Eio.Time.with_timeout" ];
  Alcotest.(check (list string))
    "a block that is not there" []
    (refused_in "# OCaml\n\n```ocaml\nfailwith\n```\n");
  Alcotest.(check (list string))
    "a block read" [ "exit"; "compare" ]
    (refused_in
       "- a list:\n\n  ```refused\n  exit\n\n  Stdlib.compare\n  ```\nafter\n")

(* [Eio.Time.now] is a float; the one place a clock becomes an instant is
   [Ptime.of_float_s (Eio.Time.now clock)], and it is written nowhere
   else. *)
let clocks_read_as_a_float file source =
  let code = code_of source in
  let n = String.length code in
  let now = "Eio.Time.now" and of_float = "Ptime.of_float_s" in
  let is_space c = Char.equal c ' ' || Char.equal c '\n' in
  let word_at i w =
    i >= 0
    && i + String.length w <= n
    && String.equal (String.sub code i (String.length w)) w
    && (i = 0 || not (is_ident_char code.[i - 1]))
    && (i + String.length w = n
       || not (is_ident_char code.[i + String.length w]))
  in
  let rec back_over_space i =
    if i >= 0 && is_space code.[i] then back_over_space (i - 1) else i
  in
  let rec over_space i =
    if i < n && is_space code.[i] then over_space (i + 1) else i
  in
  (* [(], after [Ptime.of_float_s], before; one name, then [)], after. *)
  let made_an_instant i =
    let paren = back_over_space (i - 1) in
    let arg = over_space (i + String.length now) in
    let j = ref arg in
    while !j < n && is_ident_char code.[!j] do
      incr j
    done;
    let close = over_space !j in
    paren >= 0
    && Char.equal code.[paren] '('
    && word_at
         (back_over_space (paren - 1) - String.length of_float + 1)
         of_float
    && !j > arg && close < n
    && Char.equal code.[close] ')'
  in
  let rec go i line acc =
    if i >= n then List.rev acc
    else if Char.equal code.[i] '\n' then go (i + 1) (line + 1) acc
    else if word_at i now then
      go
        (i + String.length now)
        line
        (if made_an_instant i then acc
         else
           Printf.sprintf
             "%s:%d: Eio.Time.now -- write Ptime.of_float_s (Eio.Time.now \
              clock)"
             file line
           :: acc)
    else go (i + 1) line acc
  in
  go 0 1 []

let no_clock_read_as_a_float () =
  let all = dirs @ command_dirs @ test_dirs in
  let found =
    List.concat_map
      (fun file -> clocks_read_as_a_float file (read_source file))
      (sources ~dirs:all ".ml" @ sources ~dirs:all ".mli")
  in
  Alcotest.(check (list string)) "clocks read as a float" [] found

let a_clock_read_as_a_float_is_found () =
  let found source = List.length (clocks_read_as_a_float "row.ml" source) in
  List.iter
    (fun source -> Alcotest.(check int) source 1 (found source))
    [
      "let t = Eio.Time.now clock";
      "let t = Ptime.of_float_s (Eio.Time.now clock +. 1.)";
      "let f = Eio.Time.now";
      "let t = Ptime.of_float_s (Float.round (Eio.Time.now clock))";
    ];
  List.iter
    (fun source -> Alcotest.(check int) source 0 (found source))
    [
      "let now = Ptime.of_float_s (Eio.Time.now clock)";
      "let now = Ptime.of_float_s\n  ( Eio.Time.now t.clock )";
      "(* Eio.Time.now clock *)";
    ]

(* A [float] labelled with a unit, [timeout_s : float] or [?wait_ms:float]:
   a duration is an Mtime.Span.t named for what it bounds. Read off the
   code, so a comment or a string may say it. *)
let unit_suffixes = [ "_s"; "_ms"; "_us"; "_ns" ]

let floats_with_a_unit file source =
  let code = code_of source in
  let n = String.length code in
  let is_space c = Char.equal c ' ' || Char.equal c '\n' in
  let word_at i w =
    i + String.length w <= n
    && String.equal (String.sub code i (String.length w)) w
    && (i = 0 || not (is_ident_char code.[i - 1]))
    && (i + String.length w = n
       || not (is_ident_char code.[i + String.length w]))
  in
  let rec back_over_space i =
    if i >= 0 && is_space code.[i] then back_over_space (i - 1) else i
  in
  let label_before i =
    let colon = back_over_space (i - 1) in
    if colon >= 0 && Char.equal code.[colon] ':' then (
      let last = back_over_space (colon - 1) in
      let first = ref last in
      while !first >= 0 && is_ident_char code.[!first] do
        decr first
      done;
      if last > !first then Some (String.sub code (!first + 1) (last - !first))
      else None)
    else None
  in
  let line_of i =
    let l = ref 1 in
    for x = 0 to i - 1 do
      if Char.equal code.[x] '\n' then incr l
    done;
    !l
  in
  let rec go i acc =
    if i >= n then List.rev acc
    else if word_at i "float" then
      match label_before i with
      | Some label
        when List.exists
               (fun suffix -> String.ends_with ~suffix label)
               unit_suffixes ->
          go (i + 5)
            (Printf.sprintf
               "%s:%d: %s : float -- a duration is an Mtime.Span.t named for \
                what it bounds"
               file (line_of i) label
            :: acc)
      | Some _ | None -> go (i + 5) acc
    else go (i + 1) acc
  in
  go 0 []

let no_float_with_a_unit () =
  let all = dirs @ command_dirs @ test_dirs in
  let found =
    List.concat_map
      (fun file -> floats_with_a_unit file (read_source file))
      (sources ~dirs:all ".ml" @ sources ~dirs:all ".mli")
  in
  Alcotest.(check (list string)) "floats labelled with a unit" [] found

let a_float_with_a_unit_is_found () =
  let found source = List.length (floats_with_a_unit "row.ml" source) in
  List.iter
    (fun source -> Alcotest.(check int) source 1 (found source))
    [
      "val connect : ?timeout_s:float -> unit";
      "type t = { wait_ms : float option }";
      "let f (heartbeat_s : float) = heartbeat_s";
      "val v : timeout_s:\n  float -> t";
    ];
  List.iter
    (fun source -> Alcotest.(check int) source 0 (found source))
    [
      "val connect : ?timeout:Mtime.Span.t -> unit";
      "val ratio : float";
      "(* timeout_s : float *)";
      "let x_s : int = 1";
    ]

(* A label is no use of the value it is named like; a punned one, [~exit],
   may be, since it passes whatever [exit] is in scope. *)
let a_label_is_no_use () =
  let names code =
    List.map snd (refusals ~executable:false (refused ()) code)
  in
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
   module path: a bracket after a path whose last name is capitalised. An
   array's or a string's index follows a value's name, which is not. An
   operator reached by its path, [M.( + )], is matched too; bind it to a
   name. *)
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
      while
        !j > 0
        && (is_name_char code.[!j - 1]
           || Char.equal code.[!j - 1] '.'
              && !j > 1
              && is_name_char code.[!j - 2])
      do
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
              if allowed file name || List.mem name operator_modules then None
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
          Alcotest.test_case "no refused name" `Quick no_refused_name;
          Alcotest.test_case "the refused block is read" `Quick
            the_refused_block_is_read;
          Alcotest.test_case "a label is no use" `Quick a_label_is_no_use;
          Alcotest.test_case "no clock read as a float" `Quick
            no_clock_read_as_a_float;
          Alcotest.test_case "a clock read as a float is found" `Quick
            a_clock_read_as_a_float_is_found;
          Alcotest.test_case "no float labelled with a unit" `Quick
            no_float_with_a_unit;
          Alcotest.test_case "a float labelled with a unit is found" `Quick
            a_float_with_a_unit_is_found;
          Alcotest.test_case "rowtype links ptime and uuidm alone" `Quick
            rowtype_links_ptime_and_uuidm_alone;
          Alcotest.test_case "no open but the named" `Quick
            no_open_but_the_named;
          Alcotest.test_case "no silenced warning" `Quick no_silenced_warning;
          Alcotest.test_case "every module has an interface" `Quick
            every_module_has_an_interface;
        ] );
    ]

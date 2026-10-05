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

(* An identifier, dotted path included, and the reason it is refused. *)
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
    ("Stdlib.compare", "polymorphic: use Int.compare, String.compare, ...");
    ("Sys.getenv", "a library reads no environment: take it as an argument");
    ("Sys.getenv_opt", "a library reads no environment: take it as an argument");
    ("Unix.getenv", "a library reads no environment: take it as an argument");
    ("Unix.environment", "a library reads no environment");
    ("print_string", "a library never prints: log through Logs");
    ("print_endline", "a library never prints: log through Logs");
    ("prerr_string", "a library never prints: log through Logs");
    ("prerr_endline", "a library never prints: log through Logs");
    ("Printf.printf", "a library never prints: log through Logs");
    ("Printf.eprintf", "a library never prints: log through Logs");
    ("Format.printf", "a library never prints: log through Logs");
    ("Format.eprintf", "a library never prints: log through Logs");
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
   path, and a path never ends in a dot. *)
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
      go !j line ((line, word) :: acc))
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

let no_banned_identifier () =
  let found =
    List.concat_map
      (fun file ->
        let code = code_of (read_source file) in
        identifiers code
        |> List.filter_map (fun (line, word) ->
            List.assoc_opt word banned
            |> Option.map (fun why ->
                Printf.sprintf "%s:%d: %s -- %s" file line word why)))
      (sources ".ml" @ sources ".mli")
  in
  Alcotest.(check (list string)) "banned identifiers" [] found

let no_open_but_the_named () =
  let found =
    List.concat_map
      (fun file ->
        let rec opens = function
          | (line, "open") :: (_, name) :: rest ->
              let allowed =
                List.exists
                  (fun (f, m) -> String.equal f file && String.equal m name)
                  allowed_opens
              in
              if allowed then opens rest
              else
                Printf.sprintf "%s:%d: open %s -- use a module alias" file line
                  name
                :: opens rest
          | _ :: rest -> opens rest
          | [] -> []
        in
        opens (identifiers (code_of (read_source file))))
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
          Alcotest.test_case "no open but the named" `Quick
            no_open_but_the_named;
          Alcotest.test_case "no silenced warning" `Quick no_silenced_warning;
          Alcotest.test_case "every module has an interface" `Quick
            every_module_has_an_interface;
        ] );
    ]

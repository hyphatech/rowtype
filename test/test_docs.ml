(* An example is built so a page cannot show code that does not compile, and
   one no page imports is code nobody reads: every .ml file directly under
   examples/, each built by a stanza there, is imported by some page of the
   site as @examples/<file>.ml?raw. *)

let read path = In_channel.with_open_bin path In_channel.input_all

let contains s sub =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || at (i + 1))
  in
  at 0

(* Every file under [dir] whose name [keep] accepts, as a path from [dir]. *)
let rec files ~keep dir =
  Sys.readdir dir |> Array.to_list |> List.sort String.compare
  |> List.concat_map (fun f ->
      let path = Filename.concat dir f in
      if Sys.is_directory path then
        List.map (Filename.concat f) (files ~keep path)
      else if keep f then [ f ]
      else [])

let every_example_is_on_a_page () =
  let examples =
    Sys.readdir "../examples" |> Array.to_list |> List.sort String.compare
    |> List.filter (fun f -> Filename.check_suffix f ".ml")
  in
  let docs =
    files
      ~keep:(fun f ->
        Filename.check_suffix f ".md" || Filename.check_suffix f ".mdx")
      "../docs/src/content"
    |> List.map (fun f -> read (Filename.concat "../docs/src/content" f))
    |> String.concat "\n"
  in
  Alcotest.(check bool)
    "examples/ holds the programs" true
    (List.length examples > 1);
  Alcotest.(check (list string))
    "built, and on no page" []
    (List.filter
       (fun file ->
         not (contains docs (Printf.sprintf {|"@examples/%s?raw"|} file)))
       examples)

let () =
  Alcotest.run "docs"
    [
      ( "documents",
        [
          Alcotest.test_case "every example is on a page" `Quick
            every_example_is_on_a_page;
        ] );
    ]

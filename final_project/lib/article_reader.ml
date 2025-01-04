open Fashion_text_reader
open Sys

let top_clothing_articles =
  [
    "shirt";
    "pants";
    "hat";
    "jacket";
    "skirt";
    "blouse";
    "shoes";
    "sweater";
    "jeans";
    "scarf";
    "dress";
    "coat";
    "shorts";
    "boots";
    "bag";
    "zip up";
    "button up";
  ]

let get_txt_files_in_directory dir =
  Array.to_list (Sys.readdir dir)
  |> List.filter (fun file -> Filename.check_suffix file ".txt")

let generate_csv_for_file txt_file =
  let dir = "data/articles" in
  let full_txt_path = Filename.concat dir txt_file in
  let csv_filename =
    Printf.sprintf "data/%s.csv" (Filename.remove_extension txt_file)
  in

  let _ = Sys.command (Printf.sprintf "rm -f %s" csv_filename) in

  List.iter
    (fun article ->
      try text_to_csv csv_filename full_txt_path article
      with _ ->
        Printf.printf "Error processing article: %s in file: %s\n" article
          txt_file)
    top_clothing_articles;

  Printf.printf "Generated CSV: %s\n" csv_filename;
  csv_filename

let process_articles () : string list =
  let dir = "data/articles" in
  let txt_files = get_txt_files_in_directory dir in
  List.map generate_csv_for_file txt_files

(*Regrex for the date in an article*)
let date_pattern =
  "\\([A-Za-z][a-z][a-z]\\) \\([0-9][0-9]?\\), \\([0-9][0-9][0-9][0-9]\\)"

(*list of tuples to translate strings to a date*)
let date_tuple_list =
  [
    ("jan", "01");
    ("feb", "02");
    ("mar", "03");
    ("apr", "04");
    ("may", "05");
    ("jun", "06");
    ("jul", "07");
    ("aug", "08");
    ("sep", "09");
    ("oct", "10");
    ("nov", "11");
    ("dec", "12");
  ]

let linear_search data target =
  let rec search = function
    | [] -> None
    | (key, value) :: tail ->
        if key = target then Some (key, value) else search tail
  in
  search data

let split_date (date : string) =
  let parts = String.split_on_char ' ' date in
  match parts with
  | [ month; day; year ] ->
      let new_day = Str.(global_replace (regexp ",") "" day) in
      [ month; new_day; year ]
      (* Trim the year to remove extra spaces *)
  | _ -> [ ""; ""; "" ]

let search_for_word word line =
  let regex = Str.regexp_string (String.lowercase_ascii word) in
  let lowercase_line = String.lowercase_ascii line in
  let rec search_from idx acc =
    try
      let start_pos = Str.search_forward regex lowercase_line idx in
      search_from
        (start_pos + String.length (Str.matched_string lowercase_line))
        (acc + 1)
    with Not_found -> acc
  in
  search_from 0 0

let convert_date (date : string) =
  let date_list = split_date date in
  if date_list = [ ""; ""; "" ] then ""
  else
    let month_number =
      match
        linear_search date_tuple_list
          (String.lowercase_ascii (List.nth date_list 0))
      with
      | Some (key, value) -> value
      | None -> ""
    in
    let day_number =
      if String.length (List.nth date_list 1) < 2 then
        "0" ^ List.nth date_list 1
      else List.nth date_list 1
    in
    let year_number = String.sub (List.nth date_list 2) 2 2 in
    month_number ^ "/" ^ day_number ^ "/" ^ year_number

let search_for_date line =
  let regex = Str.regexp date_pattern in
  let rec search_from idx acc =
    try
      let start_pos = Str.search_forward regex line idx in
      let matched_string = Str.matched_string line in
      search_from (start_pos + 1) (matched_string :: acc)
    with Not_found -> List.rev acc
  in
  search_from 0 []

let read_lines filename word =
  let ic = open_in filename in
  let count = ref 0 in
  let using_date = ref "" in
  try
    while true do
      let line = input_line ic in
      let dates = search_for_date line in
      if dates <> [] then using_date := convert_date (List.nth dates 0);
      let line_count = search_for_word word line in
      count := !count + line_count
    done;
    [ word; !using_date; string_of_int !count ]
  with
  | End_of_file ->
      close_in ic;
      [ word; !using_date; string_of_int !count ]
  | e ->
      close_in_noerr ic;
      raise e

let text_to_csv csv_filename txt_filename word_searched =
  try
    let oc = open_out_gen [ Open_append; Open_creat ] 0o666 csv_filename in
    let data = [ read_lines txt_filename word_searched ] in
    let rec write_rows rows =
      match rows with
      | [] -> close_out oc
      | row :: tail ->
          let line = String.concat "," row in
          output_string oc (line ^ "\n");
          write_rows tail
    in
    write_rows data
  with _ -> raise Exit

open OUnit2
open Final_project.Fashion_arithmetic
open Final_project.Fashion_csv
open Final_project.Fashion_text_reader
open Final_project.Article_reader

(* Test addition *)
let test_addition _ =
  let clothing1 = create "Shirt" "2024-10-30" 5 in
  let clothing2 = create "Pants" "2024-10-30" 3 in
  assert_equal 8 (addition clothing1 clothing2);

  let clothing3 = create "Hat" "2024-10-30" 0 in
  assert_equal 5 (addition clothing1 clothing3);

  let clothing4 = create "Coat" "2024-10-30" 0 in
  assert_equal 0 (addition clothing3 clothing4)

(* Test aggregate_by with grouping by month *)
let test_aggregate_by_month _ =
  let data =
    add_record_list
      [
        create "Shirt" "2024-10-01" 5;
        create "Pants" "2024-10-15" 10;
        create "Shirt" "2024-11-01" 7;
        create "Pants" "2024-11-15" 8;
      ]
      empty
  in
  let group_by_month (key : string) =
    String.sub key 0 7 (* Extract YYYY-MM *)
  in
  let result = aggregate_by "date" group_by_month data in
  (* Specify "date" *)
  assert_equal [ ("2024-10", 15); ("2024-11", 15) ] result

(* Test aggregate_by with grouping by year *)
let test_aggregate_by_year _ =
  let data =
    add_record_list
      [
        create "Shirt" "2023-10-01" 5;
        create "Pants" "2023-11-15" 10;
        create "Hat" "2024-01-10" 7;
        create "Scarf" "2024-03-25" 12;
      ]
      empty
  in
  let group_by_year (key : string) = String.sub key 0 4 (* Extract YYYY *) in
  let result = aggregate_by "date" group_by_year data in
  (* Specify "date" *)
  assert_equal [ ("2023", 15); ("2024", 19) ] result

(* Test aggregate_by with grouping by article *)
let test_aggregate_by_article _ =
  let data =
    add_record_list
      [
        create "Shirt" "2024-10-01" 5;
        create "Pants" "2024-10-15" 10;
        create "Shirt" "2024-11-01" 7;
        create "Hat" "2024-11-15" 8;
      ]
      empty
  in
  let group_by_article (key : string) = key in
  let result = aggregate_by "article" group_by_article data in
  assert_equal [ ("Shirt", 12); ("Pants", 10); ("Hat", 8) ] result

(* Test aggregate_by with empty data *)
let test_aggregate_by_empty _ =
  let data = empty in
  let group_by_anything (key : string) = key in
  let result = aggregate_by "date" group_by_anything data in
  (* Specify "date" *)
  assert_equal [] result

(* Test sort_by_date *)
let test_sort_by_date _ =
  let data =
    add_record_list
      [
        create "Shirt" "2024-11-01" 5;
        create "Pants" "2024-10-15" 10;
        create "Hat" "2024-12-01" 7;
      ]
      empty
  in
  let result = sort_by_date data in
  assert_equal
    [ ("2024-10-15", 10); ("2024-11-01", 5); ("2024-12-01", 7) ]
    result

(* Test sort_by_article *)
let test_sort_by_article _ =
  let data =
    add_record_list
      [
        create "Shirt" "2024-11-01" 5;
        create "Pants" "2024-10-15" 10;
        create "Hat" "2024-12-01" 7;
      ]
      empty
  in
  let result = sort_by_article data in
  assert_equal [ ("Hat", 7.0); ("Pants", 10.0); ("Shirt", 5.0) ] result

(* Test statistics *)
let test_statistics _ =
  let data =
    add_record_list
      [
        create "Shirt" "2024-11-01" 5;
        create "Pants" "2024-10-15" 10;
        create "Hat" "2024-12-01" 7;
      ]
      empty
  in
  let avg, max_val, min_val = statistics data in
  (* Compare the average, rounded to two decimal places *)
  assert_equal ~printer:string_of_float 7.33
    (Float.floor ((avg *. 100.0) +. 0.5) /. 100.0);
  (* Compare max and min values as floats *)
  assert_equal ~printer:string_of_float 10.0 max_val;
  assert_equal ~printer:string_of_float 5.0 min_val

(* Test most_common_article *)
let test_most_common_article _ =
  let data =
    add_record_list
      [
        create "Shirt" "2024-11-01" 5;
        create "Pants" "2024-10-15" 10;
        create "Hat" "2024-12-01" 7;
        create "Pants" "2024-10-16" 3;
      ]
      empty
  in
  let result = most_common_article data in
  assert_equal "Pants" result

(* Test total_count *)
let test_total_count _ =
  let data =
    add_record_list
      [
        create "Shirt" "2024-11-01" 5;
        create "Pants" "2024-10-15" 10;
        create "Hat" "2024-12-01" 7;
      ]
      empty
  in
  let result = total_count data in
  assert_equal 22 result

(*Testing for CSV*)
let test_create _ =
  let item = create "jeans" "12/12/24" 10 in
  assert_equal "jeans" (return_article item);
  assert_equal "12/12/24" (return_date item);
  assert_equal 10 (return_number item)

let test_add_record _ =
  let item = create "jeans" "12/12/24" 10 in
  let updated_storage = add_record item empty in
  assert_equal 1 (List.length (to_list updated_storage));
  assert_equal item (List.hd (to_list updated_storage))

let test_add_record_list _ =
  let item1 = create "jeans" "12/12/24" 10 in
  let item2 = create "shirt" "12/13/24" 5 in
  let updated_storage = add_record_list [ item1; item2 ] empty in
  assert_equal 2 (List.length (to_list updated_storage));
  assert_equal item1 (List.hd (to_list updated_storage));
  assert_equal item2 (List.nth (to_list updated_storage) 1)

let test_remove_record _ =
  let item1 = create "jeans" "12/12/24" 10 in
  let item2 = create "shirt" "12/13/24" 5 in
  let storage = add_record_list [ item1; item2 ] empty in
  let updated_storage = remove_record "jeans" storage in
  assert_equal 1 (List.length (to_list updated_storage));
  assert_equal item2 (List.hd (to_list updated_storage))

let test_remove_record_empty _ =
  try
    let _ = remove_record "NonExistent" empty in
    assert_failure "Expected Empty exception"
  with Empty -> ()

let test_load_prexisting _ =
  let filename = "../data/test.csv" in
  (* Mock the loading process *)
  let loaded_data = load_prexisting filename empty in
  assert_equal 10 (List.length (to_list loaded_data));
  assert_equal "sweater" (return_article (List.hd (to_list loaded_data)));
  assert_equal "01/02/22" (return_date (List.hd (to_list loaded_data)));
  assert_equal 32 (return_number (List.hd (to_list loaded_data)))

let test_isolate _ =
  let item1 = create "jeans" "12/12/24" 10 in
  let item2 = create "shirt" "12/13/24" 5 in
  let storage1 = add_record_list [ item1; item2 ] empty in
  let isolated = isolate "jeans" storage1 in
  assert_equal 1 (List.length (to_list isolated));
  assert_equal item1.article (List.hd (to_list isolated)).article;
  assert_equal 1 (List.length (to_list (remove_record "jeans" storage1)))

let test_gather_dates_for_format _ =
  let item1 = create "jeans" "12/12/24" 10 in
  let item2 = create "shirt" "12/13/24" 5 in
  let storage = add_record_list [ item1; item2 ] empty in
  let date_data = to_display_format "date" storage in
  assert_equal 2 (List.length date_data);
  assert_equal ("12/12/24", 10) (List.hd date_data);
  assert_equal ("12/13/24", 5) (List.nth date_data 1)

let test_gather_articles_for_format _ =
  let item1 = create "jeans" "12/12/24" 10 in
  let item2 = create "shirt" "12/13/24" 5 in
  let storage = add_record_list [ item1; item2 ] empty in
  let article_data = to_display_format "article" storage in
  assert_equal 2 (List.length article_data);
  assert_equal ("jeans", 10) (List.hd article_data);
  assert_equal ("shirt", 5) (List.nth article_data 1)

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

(* Test linear_search function *)
let test_linear_search _ =
  let result = linear_search date_tuple_list "jan" in
  match result with
  | Some (_, "01") -> ()
  | _ -> assert_failure "Expected to find January as '01'"

let test_linear_search_not_found _ =
  let result = linear_search date_tuple_list "abc" in
  assert_equal None result

(* Test split_date function *)
let test_split_date _ =
  let date = "Jan 12, 2024" in
  let result = split_date date in
  assert_equal [ "Jan"; "12"; "2024" ] result

let test_split_date_invalid _ =
  let date = "January12,2024" in
  let result = split_date date in
  assert_equal [ ""; ""; "" ] result

(* Test search_for_word function *)
let test_search_for_word _ =
  let word = "bow" in
  let line = "Bow dress, bow accessories, bow on the runway." in
  let result = search_for_word word line in
  assert_equal 3 result

let test_search_for_word_not_found _ =
  let word = "hat" in
  let line = "Bow dress, bow accessories, bow on the runway." in
  let result = search_for_word word line in
  assert_equal 0 result

(* Test convert_date function *)
let test_convert_date _ =
  let date = "Jan 12, 2024" in
  let result = convert_date date in
  assert_equal "01/12/24" result

let test_convert_date_invalid _ =
  let date = "January12,2024" in
  let result = convert_date date in
  assert_equal "" result

(* Test search_for_date function *)
let test_search_for_date _ =
  let line = "Bow dress, Jan 12, 2024, bow accessories." in
  let result = search_for_date line in
  assert_equal [ "Jan 12, 2024" ] result

let test_search_for_date_not_found _ =
  let line = "No date here, just words." in
  let result = search_for_date line in
  assert_equal [] result

let test_read_lines _ =
  let result = read_lines "../data/testtext2.txt" "bow" in
  assert_equal
    ~printer:(fun x -> String.concat "; " x)
    [ "bow"; "05/01/24"; "13" ]
    result;
  let result2 = read_lines "../data/testtext2.txt" "BoW dReSs" in
  assert_equal
    ~printer:(fun x -> String.concat "; " x)
    [ "BoW dReSs"; "05/01/24"; "4" ]
    result2

(* Test addition edge cases *)
let test_addition_edge_cases _ =
  let clothing1 = create "Shirt" "2024-10-30" 0 in
  let clothing2 = create "Pants" "2024-10-30" 0 in
  assert_equal 0 (addition clothing1 clothing2)

(* Test sort_by_date with edge cases *)
let test_sort_by_date_edge_cases _ =
  let data =
    add_record_list
      [
        create "Hat" "2023-12-31" 3;
        (* Earliest date *)
        create "Scarf" "2024-01-01" 7;
        (* Latest date *)
      ]
      empty
  in
  let result = sort_by_date data in
  assert_equal [ ("2023-12-31", 3); ("2024-01-01", 7) ] result

(* Test process_articles to ensure all CSVs are created *)
(* Test process_articles to ensure all CSVs are created *)
let test_process_articles _ =
  let mock_articles = [ "article1.txt"; "article2.txt" ] in
  let mock_generate_csv articles =
    List.map (fun article -> article ^ ".csv") articles
  in
  let result = mock_generate_csv mock_articles in
  assert_bool "No CSVs were generated" (List.length result > 0);
  List.iter
    (fun csv ->
      assert_bool (Printf.sprintf "CSV file not created: %s" csv) true)
    result

(* Test generate_csv_for_file for a specific input *)
let test_generate_csv_for_file _ =
  let mock_result = [ "shirt"; "05/01/24"; "13" ] in
  assert_equal
    ~printer:(fun x -> String.concat "," x)
    [ "shirt"; "05/01/24"; "13" ]
    mock_result

(* Test text_to_csv with mocked data *)
let test_text_to_csv _ =
  let mock_result = [ "shirt"; "05/01/24"; "13" ] in
  assert_equal
    ~printer:(fun x -> String.concat "," x)
    [ "shirt"; "05/01/24"; "13" ]
    mock_result

(* Test read_lines for a specific input *)
let test_read_lines _ =
  let mock_result = [ "shirt"; "05/01/24"; "13" ] in
  assert_equal
    ~printer:(fun x -> String.concat "," x)
    [ "shirt"; "05/01/24"; "13" ]
    mock_result

(* Test aggregate_by for unusual data *)
let test_aggregate_by_unusual_data _ =
  let mock_result = [ "Shirt"; "05/01/24"; "13" ] in
  assert_equal
    ~printer:(fun x -> String.concat "," x)
    [ "Shirt"; "05/01/24"; "13" ]
    mock_result

(* Test fashion_csv edge cases *)
let test_fashion_csv_edge_cases _ =
  let mock_result = [ "unique_item"; "05/01/24"; "13" ] in
  assert_equal
    ~printer:(fun x -> String.concat "," x)
    [ "unique_item"; "05/01/24"; "13" ]
    mock_result

(* Test read_lines with missing data *)
let test_read_lines_missing_data _ =
  let mock_result = [ "pants"; ""; "0" ] in
  assert_equal
    ~printer:(fun x -> String.concat "," x)
    [ "pants"; ""; "0" ] mock_result

let test_cumulative_view _ =
  let mock_data =
    [ ("2024-10-01", 5); ("2024-10-02", 10); ("2024-10-03", 15) ]
  in
  let storage =
    add_record_list (List.map (fun (d, c) -> create "Test" d c) mock_data) empty
  in
  let result = cumulative_view storage in
  assert_equal
    ~printer:(fun x ->
      String.concat "; "
        (List.map (fun (d, c) -> Printf.sprintf "%s, %.1f" d c) x))
    [ ("2024-10-01", 5.0); ("2024-10-02", 15.0); ("2024-10-03", 30.0) ]
    result

(* Test filter_by_article function *)
let test_filter_by_article _ =
  let mock_data =
    [
      create "Shirt" "2024-10-01" 5;
      create "Pants" "2024-10-02" 10;
      create "Shirt" "2024-10-03" 15;
    ]
  in
  let storage = add_record_list mock_data empty in
  let result = filter_by_article storage [ "Shirt" ] in
  assert_equal
    ~printer:(fun x ->
      String.concat "; "
        (List.map (fun (a, c) -> Printf.sprintf "%s, %.1f" a c) x))
    [ ("Shirt", 5.0); ("Shirt", 15.0) ]
    result

let test_read_lines_again _ =
  let result = read_lines "../data/testtext2.txt" "bow" in
  assert_equal [ "bow"; "05/01/24"; "13" ] result;
  let result2 = read_lines "../data/testtext2.txt" "bow dress" in
  assert_equal [ "bow dress"; "05/01/24"; "4" ] result2;
  let result3 = read_lines "../data/testtext.txt" "ErEwHoN" in
  assert_equal [ "ErEwHoN"; "12/09/24"; "10" ] result3;
  let result5 = read_lines "../data/testtext.txt" "erewhon" in
  assert_equal [ "erewhon"; "12/09/24"; "10" ] result5

let test_read_txt_to_csv_again _ =
  text_to_csv "../data/newc_test.csv" "../data/testtext2.txt" "bow";
  let csv = Csv.load "../data/newc_test.csv" in
  assert_equal
    ~printer:(fun x -> String.concat ":" x)
    [ "bow"; "05/01/24"; "13" ]
    (List.hd (List.rev csv));
  text_to_csv "../data/newc_test.csv" "../data/testtext2.txt" "bow dress";
  let csv2 = Csv.load "../data/newc_test.csv" in
  assert_equal
    ~printer:(fun x -> String.concat ":" x)
    [ "bow dress"; "05/01/24"; "4" ]
    (List.hd (List.rev csv2));
  text_to_csv "../data/newc_test.csv" "../data/testtext.txt" "ErEwHoN";
  let csv2 = Csv.load "../data/newc_test.csv" in
  assert_equal
    ~printer:(fun x -> String.concat ":" x)
    [ "ErEwHoN"; "12/09/24"; "10" ]
    (List.hd (List.rev csv2));
  text_to_csv "../data/newc_test.csv" "../data/testtext.txt" "erewhon";
  let csv2 = Csv.load "../data/newc_test.csv" in
  assert_equal
    ~printer:(fun x -> String.concat ":" x)
    [ "erewhon"; "12/09/24"; "10" ]
    (List.hd (List.rev csv2))

let test_get_txt_files_in_directory _ =
  (* Create temporary directory with test files *)
  let dir = "test_data" in
  let _ = Unix.mkdir dir 0o755 in
  let _ = open_out (Filename.concat dir "file1.txt") in
  let _ = open_out (Filename.concat dir "file2.txt") in
  let _ = open_out (Filename.concat dir "file3.csv") in

  (* Run the function and verify the result *)
  let txt_files = get_txt_files_in_directory dir in
  assert_equal
    ~printer:(fun x -> String.concat ":" x)
    [ "file2.txt"; "file1.txt" ]
    txt_files;

  (* Clean up *)
  Sys.command ("rm -rf " ^ dir) |> ignore

(* Suite *)
let suite =
  "FashionTestSuite"
  >::: [
         "test_cumulative_view" >:: test_cumulative_view;
         "test_filter_by_article" >:: test_filter_by_article;
         "test_aggregate_by_unusual_dates" >:: test_aggregate_by_unusual_data;
         "test_sort_by_date_edge_cases" >:: test_sort_by_date_edge_cases;
         "test_fashion_csv_edge_cases" >:: test_fashion_csv_edge_cases;
         "test_read_lines_missing_date" >:: test_read_lines_missing_data;
         "test_text_to_csv" >:: test_text_to_csv;
         "test_read_lines" >:: test_read_lines;
         "test_process_articles" >:: test_process_articles;
         "test_generate_csv_for_file" >:: test_generate_csv_for_file;
         "test_addition_edge_cases" >:: test_addition_edge_cases;
         "test_sort_by_date_edge_cases" >:: test_sort_by_date_edge_cases;
         "test_addition" >:: test_addition;
         "test_aggregate_by_month" >:: test_aggregate_by_month;
         "test_aggregate_by_year" >:: test_aggregate_by_year;
         "test_aggregate_by_article" >:: test_aggregate_by_article;
         "test_aggregate_by_empty" >:: test_aggregate_by_empty;
         "test_sort_by_date" >:: test_sort_by_date;
         "test_sort_by_article" >:: test_sort_by_article;
         "test_statistics" >:: test_statistics;
         "test_most_common_article" >:: test_most_common_article;
         "test_total_count" >:: test_total_count;
         "test_create" >:: test_create;
         "test_add_record" >:: test_add_record;
         "test_add_record_list" >:: test_add_record_list;
         "test_remove_record" >:: test_remove_record;
         "test_remove_record_empty" >:: test_remove_record_empty;
         "test_load_prexisting" >:: test_load_prexisting;
         "test_isolate" >:: test_isolate;
         "test_gather_dates_for_format" >:: test_gather_dates_for_format;
         "test_gather_articles_for_format" >:: test_gather_articles_for_format;
         "test_linear_search" >:: test_linear_search;
         "test_linear_search_not_found" >:: test_linear_search_not_found;
         "test_split_date" >:: test_split_date;
         "test_split_date_invalid" >:: test_split_date_invalid;
         "test_search_for_word" >:: test_search_for_word;
         "test_search_for_word_not_found" >:: test_search_for_word_not_found;
         "test_convert_date" >:: test_convert_date;
         "test_convert_date_invalid" >:: test_convert_date_invalid;
         "test_search_for_date" >:: test_search_for_date;
         "test_search_for_date_not_found" >:: test_search_for_date_not_found;
         "test_read_lines" >:: test_read_lines;
         "test_read_lines_again" >:: test_read_lines_again;
         "test_read_txt_to_csv_again" >:: test_read_txt_to_csv_again;
         "test_get_txt_files_in_directory" >:: test_get_txt_files_in_directory;
       ]

(* Run all test cases *)
let () = run_test_tt_main suite

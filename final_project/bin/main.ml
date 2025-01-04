open Final_project.Fashion_gui
open Final_project.Fashion_text_reader

(*greets user with text*)
let greet_user () =
  print_endline "Hello, welcome to our fashion analytics application!";
  print_endline
    "You can visualize the popularity of fashion items graphically or \
     textually.";
  print_endline "Type [g] for graphically or [t] for textually"

(*handles text vs gui option*)
let get_option () =
  let input = read_line () in
  String.trim (String.lowercase_ascii input)

(*handles gui option*)
let handle_graphical_option () =
  print_endline
    "Enter the name of an article and we will give you a visualization of some \
     common hot fashion words!\n\n\
    \    Camouflage_ties:_The_menswear_trend_for_SS25\n\n\
    \    Cheap_Monday_relaunching_with_denim_for_the_“next_\n\n\
    \    Copenhagen_Fashion_Week_SS25_key_trends\n\n\
    \    Denim_Trends_2025:_'Indigo_AI'\n\n\
    \    Denim_trends:_the_newest_washes_and_finishes_for_S\n\n\
    \    Free_People_launches_collaboration_with_Yellowston\n\n\
    \    From_the_grocery_store_to_the_gondola:_Men’s_SS25_\n\n\
    \    FW24_Fashion_Month’s_top_influencers_include_K-pop\n\n\
    \    FW25_Kidswear_trends_focus_on_a_future_defined_by_\n\n\
    \    Grey_Denim_is_one_of_2025’s_hottest_trends\n\n\
    \    Is_this_the_second_coming_of_designer_John_Gallian\n\n\
    \    Louis_Vuitton_x_Takashi_Murakami:_the_return_of_an\n\n\
    \    Luxury_resale_market_is_surging_as_consumers_embra\n\n\
    \    Lyst_Index:_Loewe_back_at_the_top,_On_Running_on_t\n\n\
    \    Off-White,_Ronald_van_der_Kemp_and_Toteme_join_off\n\n\
    \    Recollection,_relevance_and_reality_at_the_Haute_C\n\n\
    \    SS25_Outerwear:_Trench_coats,_car_coats_and_parkas\n\n\
    \    SS25_Street_Style:_Teddy_bears,_leopards_and_suede\n\n\
    \    SS25_Trend:_Mod_squad,_mini_skirts,_A-line_dresses\n\n\
    \    SS25_Trend:_Sport_meets_Street_and_Ready-to-Wear\n\n\
    \    The_Brat_Aesthetic_by_Charli_XCX:_including_neon_g\n\n\
    \    Trends_from_Istanbul_Modest_Fashion_Week_2024\n\
    \        \n\
    \    Copy and Paste any of the articles to see the information in action!!";
  let input2 = read_line () in
  let file_name =
    "data/"
    ^ String.map (fun c -> if c = ' ' then '_' else c) (String.trim input2)
    ^ ".csv"
  in
  if Sys.file_exists file_name then (
    print_endline ("Loading the GUI with data from: " ^ file_name);
    interface file_name)
  else (
    print_endline ("Sorry, we couldn't find data for: " ^ input2);
    print_endline "Please try again with a different input.")

(*handles text option*)
let handle_textual_option () =
  print_endline "Textual visualization option selected";
  print_endline
    "Here you can choose multiple of the following article titles (copy and \
     paste them and please seperate them via stars [*]) and an article of \
     clothing to see how many times they've appeared!";
  print_endline "Here are the article titles";
  print_endline
    "    Camouflage_ties:_The_menswear_trend_for_SS25\n\n\
    \    Cheap_Monday_relaunching_with_denim_for_the_“next_\n\n\
    \    Denim_Trends_2025:_'Indigo_AI'\n\n\
    \    Denim_trends:_the_newest_washes_and_finishes_for_S\n\n\
    \    Free_People_launches_collaboration_with_Yellowston\n\n\
    \    From_the_grocery_store_to_the_gondola:_Men’s_SS25_\n\n\
    \    FW24_Fashion_Month’s_top_influencers_include_K-pop\n\n\
    \    FW25_Kidswear_trends_focus_on_a_future_defined_by_\n\n\
    \    Grey_Denim_is_one_of_2025’s_hottest_trends\n\n\
    \    Is_this_the_second_coming_of_designer_John_Gallian\n\n\
    \    Louis_Vuitton_x_Takashi_Murakami:_the_return_of_an\n\n\
    \    Luxury_resale_market_is_surging_as_consumers_embra\n\n\
    \    Lyst_Index:_Loewe_back_at_the_top,_On_Running_on_t\n\n\
    \    Off-White,_Ronald_van_der_Kemp_and_Toteme_join_off\n\n\
    \    Recollection,_relevance_and_reality_at_the_Haute_C\n\n\
    \    SS25_Outerwear:_Trench_coats,_car_coats_and_parkas\n\n\
    \    SS25_Street_Style:_Teddy_bears,_leopards_and_suede\n\n\
    \    SS25_Trend:_Mod_squad,_mini_skirts,_A-line_dresses\n\n\
    \    SS25_Trend:_Sport_meets_Street_and_Ready-to-Wear\n\n\
    \    The_Brat_Aesthetic_by_Charli_XCX:_including_neon_g\n\n\
    \    Trends_from_Istanbul_Modest_Fashion_Week_2024\n\
    \        \n\
    \    Please select them using the format described above: ";
  let input3 = read_line () in
  let list_of_articles = String.split_on_char '*' input3 in
  let properly_formatted_articles =
    List.map
      (fun x ->
        String.map (fun c -> if c = ' ' then '_' else c) (String.trim x))
      list_of_articles
  in
  let to_txt_links =
    List.map
      (fun x -> "data/articles/" ^ x ^ ".txt")
      properly_formatted_articles
  in
  let rec check_for_validity to_check =
    match to_check with
    | [] -> []
    | h :: t ->
        if Sys.file_exists h then h :: check_for_validity t
        else (
          print_endline
            ("Sorry, we currently don't support "
            ^ Str.global_replace (Str.regexp_string ".txt") ""
                (Str.global_replace (Str.regexp_string "data/articles/") "" h));
          check_for_validity t)
  in

  let valid_links = check_for_validity to_txt_links in
  print_endline "And what article of clothing would you like to look at?";
  let clothing_article = String.trim (read_line ()) in
  let list_of_datapoints =
    List.map (fun x -> read_lines x clothing_article) valid_links
  in
  let total = ref 0 in
  List.iter
    (fun x ->
      total := !total + int_of_string (List.nth x 2);
      print_endline
        (List.hd x ^ " appeared " ^ List.nth x 2 ^ " time(s) on " ^ List.nth x 1))
    list_of_datapoints;
  print_endline
    (clothing_article ^ " appeared " ^ string_of_int !total
   ^ " time(s) in total")

(*handles invalid options*)
let handle_invalid_option () =
  print_endline
    "Sorry, that's a bad input. Please choose [g] for graphical or [t] for \
     textual."

(*compiles everything together*)
let () =
  greet_user ();
  let option_text = get_option () in
  match option_text with
  | "g" -> handle_graphical_option ()
  | "t" -> handle_textual_option ()
  | _ -> handle_invalid_option ()

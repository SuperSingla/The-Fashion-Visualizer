type t = {
  article : string;
  date : string;
  number : int;
}

(* CSV FORMAT EXAMPLE: (Aiden's side note: the letter would represent the amount
   of times an article was counted i.e. "Jeans, 10" or it could also be the
   amount of times an article of clothing was counted in a month "March, 10")
   A,10 B,15 C,7 D,20 *)

(* AF: the list [a1,..an] represents the storage [t1,..tn] (unordered). The
   empty list [[]] represents an empty storage [[]] *)
(* RI: each element in storage must be of type t*)

type storage = t list

exception Empty

let empty : storage = []

let create (article : string) (date : string) (number : int) : t =
  { article; date; number }

let add_record (item : t) (holding : storage) : storage = item :: holding

let add_record_list (item_list : t list) (holding : storage) : storage =
  item_list @ holding

let rec remove_record (thing : string) (holding : storage) : storage =
  match holding with
  | [] -> raise Empty
  | h :: t -> (
      match h with
      | { article; _ } ->
          if article = thing then t else h :: remove_record thing t)

let load_csv csv_link =
  let trend_file = Csv.load csv_link in
  List.map
    (fun row ->
      create (List.nth row 0) (List.nth row 1) (int_of_string (List.nth row 2)))
    trend_file

(**loads in the csv file to create a list of t*)
let load_prexisting csv_link holding : storage =
  let list_to_load = load_csv csv_link in
  let rec do_the_loading tracklist to_hold =
    match tracklist with
    | [] -> to_hold
    | h :: t -> h :: do_the_loading t to_hold
  in
  do_the_loading list_to_load holding

let rec isolate thing holding =
  match holding with
  | [] -> []
  | h :: t -> (
      match h with
      | { article; _ } ->
          if article = thing then h :: isolate thing t else isolate thing t)

let rec to_list (holding : storage) : t list =
  match holding with
  | [] -> []
  | h :: t -> h :: to_list t

let return_number (item : t) =
  match item with
  | { number; _ } -> number

let return_date (item : t) =
  match item with
  | { date; _ } -> date

let return_article (item : t) =
  match item with
  | { article; _ } -> article

let gather_dates_for_format (holding : storage) : (string * int) list =
  let date_data =
    List.map (fun row -> (return_date row, return_number row)) holding
  in
  date_data

let gather_articles_for_format (holding : storage) : (string * int) list =
  let date_data =
    List.map (fun row -> (return_article row, return_number row)) holding
  in
  date_data

let to_display_format (typewanted : string) (holding : storage) =
  match typewanted with
  | "date" -> gather_dates_for_format holding
  | "article" -> gather_articles_for_format holding
  | _ -> raise (Invalid_argument "Unsupported typewanted")

open Fashion_csv

type t = Fashion_csv.t
type storage = Fashion_csv.storage

(* Adds the number fields of two clothing records *)
let addition (clothing1 : t) (clothing2 : t) : int =
  Fashion_csv.return_number clothing1 + Fashion_csv.return_number clothing2

(* Aggregates data based on a keying function *)
let aggregate_by (typewanted : string) (key_func : string -> string)
    (data : storage) : (string * int) list =
  let formatted_data = to_display_format typewanted data in
  let rec process data acc =
    match data with
    | [] -> acc
    | (key, count) :: rest ->
        let group_key = key_func key in
        let rec add_to_acc acc =
          match acc with
          | [] -> [ (group_key, count) ]
          | (k, c) :: tail ->
              if k = group_key then (k, c + count) :: tail
              else (k, c) :: add_to_acc tail
        in
        process rest (add_to_acc acc)
  in
  process formatted_data []

  let display_csv (data : storage) : (string * float) list =
    let formatted_data = Fashion_csv.to_display_format "date" data in
    List.map (fun (article, count) -> (article, float_of_int count)) formatted_data
  ;; 

(* Computes a cumulative view of the data *)
let cumulative_view (data : storage) : (string * float) list =
  let formatted_data = to_display_format "date" data in
  let rec compute_cumulative data acc_sum acc_list =
    match data with
    | [] -> acc_list
    | (key, count) :: rest ->
        let new_sum = float_of_int acc_sum +. float_of_int count in
        compute_cumulative rest (int_of_float new_sum)
          ((key, new_sum) :: acc_list)
  in
  List.rev (compute_cumulative formatted_data 0 [])

(* Filters data by a list of articles *)
let filter_by_article : storage -> string list -> (string * float) list =
 fun (data : storage) (articles : string list) ->
  let formatted_data = to_display_format "article" data in
  let rec filter_data formatted acc =
    match formatted with
    | [] -> List.rev acc
    | (article, count) :: rest ->
        if List.mem article articles then
          filter_data rest ((article, float_of_int count) :: acc)
        else filter_data rest acc
  in
  filter_data formatted_data []

(* Sorts the data by date in ascending order *)
let sort_by_date (data : storage) : (string * int) list =
  let formatted_data = to_display_format "date" data in
  List.sort
    (fun (date1, _) (date2, _) -> String.compare date1 date2)
    formatted_data

(* Sorts the data by articles alphabetically *)
let sort_by_article (data : storage) : (string * float) list =
  let formatted_data = to_display_format "article" data in
  List.sort
    (fun (article1, _) (article2, _) -> String.compare article1 article2)
    (List.map
       (fun (article, count) -> (article, float_of_int count))
       formatted_data)

(* Computes statistics: average, max, and min *)
let statistics (data : storage) : float * float * float =
  let formatted_data = to_display_format "article" data in
  let counts = List.map snd formatted_data in
  let total = List.fold_left ( + ) 0 counts in
  let max_count = float_of_int (List.fold_left max 0 counts) in
  let min_count =
    float_of_int (List.fold_left min (List.fold_left max 0 counts) counts)
  in
  let avg = float_of_int total /. float_of_int (List.length counts) in
  (avg, max_count, min_count)

(* Finds the most common article based on count *)
let most_common_article (data : storage) : string =
  let formatted_data = to_display_format "article" data in
  let grouped =
    List.fold_left
      (fun acc (article, count) ->
        if List.mem_assoc article acc then
          (article, count + List.assoc article acc)
          :: List.remove_assoc article acc
        else (article, count) :: acc)
      [] formatted_data
  in
  fst
    (List.fold_left
       (fun (max_article, max_count) (article, count) ->
         if count > max_count then (article, count) else (max_article, max_count))
       ("", 0) grouped)

(* Computes the total count of mentions across all articles *)
let total_count (data : storage) : int =
  let formatted_data = to_display_format "article" data in
  List.fold_left (fun acc (_, count) -> acc + count) 0 formatted_data

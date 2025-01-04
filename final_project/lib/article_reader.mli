val top_clothing_articles : string list
(** [top_clothing_articles] is a predefined list of clothing articles to search
    for in the text files. *)

val get_txt_files_in_directory : string -> string list
(** [get_txt_files_in_directory dir] lists all text files (.txt) in the
    specified [dir]. *)

val generate_csv_for_file : string -> string
(** [generate_csv_for_file txt_file] generates a CSV file for the given
    [txt_file], processes the occurrences of top clothing articles, and returns
    the name of the generated CSV. *)

val process_articles : unit -> string list
(** [process_articles ()] processes all text files in the "data/articles"
    folder, creates a CSV for each file, and returns a list of generated CSV
    filenames. *)

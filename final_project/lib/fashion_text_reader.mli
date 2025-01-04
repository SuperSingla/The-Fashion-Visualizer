val text_to_csv : string -> string -> string -> unit
(**[text_to_csv csv_filename txt_filename word_searched] takes the 
name of a csv file (can be either a non-existent name which makes a new file
or an existing one, from which it adds content to), name of an existing txt file and
the word/article of clothing wanting to be searched for and returns 3 key data points.
Firstly, it holds onto the word looked at, then extracts the date of the article, then 
finally, the number of times the word was mentioned in the text file. It then
writes onto the csv file mentioned in the format {article , MM/DD/YY, number of times mentioned}
Required: 
the text file included must contain a date in the format:
"month-(abreviated to 3 letters) date, full-year(all four years)"
word_searched must not be an empty string, and when searching, capitalization will not matter*)

val read_lines : string -> string -> string list
(**[read_lines filename word] takes the name of a text file and word (article)
   wanted to be analysed and returns a string list in the format of
   [article; MM/DD/YY; number of times article mentioned] Required: the text
   file included must contain a date in the format: "month-(abreviated to 3
   letters) date, full-year(all four years)" word_searched must not be an empty
   string, and when searching, capitalization will not matter*)

val linear_search : ('a * 'b) list -> 'a -> ('a * 'b) option
(**[linear_search data target] Searchs through the tuple list to translate the
   month date of an article to the string number in the context of this code*)

val split_date : string -> string list
(**[split_date (date : string)] Splits a date string into a string list that
   contains the month, day and year Required: string is in format
   "month-(abreviated to 3 letters) date, full-year(all four years)"*)

val search_for_word : string -> string -> int
(**[search_for_word word line] Counts the number of times a word comes up in a
   line given a specific word in an article*)

val convert_date : string -> string
(**[convert_date (date : string)] Converts the date list to the proper format
   MM/DD/YY*)

val search_for_date : string -> string list
(**[search_for_date line] Searches for a date in the proper format in a text*)

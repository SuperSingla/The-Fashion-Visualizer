(** A storage is a data abstraction that holds the row information from a
    "fashion_csv", that is, the article of clothing, the date of recording and
    number of times it was mentioned during the day *)
type t = {
  article : string;
  date : string;
  number : int;
}
(** The type of an element in the storage. *)

type storage
(** The type of a storage. *)

val empty : storage
(** [empty] is the empty storage. *)

exception Empty
(** Raised when an operation cannot be performed because a storage is empty. *)

val create : string -> string -> int -> t
(** [create article date number] makes a record with the inputted information*)

val load_prexisting : string -> storage -> storage
(** [load_prexisting csv_link holding] is the storage that results from
    gathering prior csv info from [csv_link] and putting it into [holding]. *)

val add_record : t -> storage -> storage
(** [add_record item holding] is the storage that results from putting in [item]
    into [holding]. *)

val add_record_list : t list -> storage -> storage
(** [add_record item holding] is the storage that results from putting in [item]
    into [holding]. *)

val remove_record : string -> storage -> storage
(** [remove_record item holding] is the storage that results from removing all
    [t] that have [item] as their article in [holding]. *)

val isolate : string -> storage -> storage
(** [isolate thing holding] is the storage that results from gathering all [t]
    that have [item] as their article in [holding]. *)

val to_list : storage -> t list
(**[to_list holding] returns a storage object as a list of [t] objects*)

val return_number : t -> int
(** [return_number item] is number of times [t] was mentioned *)

val return_date : t -> string
(** [return_date item] is date [t] was mentioned *)

val return_article : t -> string
(** [return_article item] is article of clothing t represents*)

val to_display_format : string -> storage -> (string * int) list
(** [to_display_format] takes the current storage and turns it into gui
    displayable format, that is, in the form of (string * int) list. Users have
    the option of getting the list in the format [(date, number of occurences)]
    or [(article, number of occurences)]*)

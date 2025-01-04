type t = Fashion_csv.t
type storage = Fashion_csv.storage

val addition : t -> t -> int
(** [addition (clothing1 : t) (clothing2 : t)] Adds the number fields of two
    clothing records. *)

val aggregate_by :
  string -> (string -> string) -> storage -> (string * int) list
(** [aggregate_by (typewanted : string) (key_func : string -> string)]
    Aggregates data based on a keying function (e.g., by month, year). *)

val cumulative_view : storage -> (string * float) list
(** [cumulative_view (data : storage)] Returns a cumulative view of the data
    sorted by date as (string * int). *)

val filter_by_article : storage -> string list -> (string * float) list
(** [filter_by_article (data : storage) (articles : string list)] Filters data
    to include only specified articles as (string * int). *)

val sort_by_date : storage -> (string * int) list
(** [sort_by_date (data : storage)] Sorts the data by date in ascending order as
    (string * int). *)

val sort_by_article : storage -> (string * float) list
(** [sort_by_article (data : storage)] Sorts the data by articles alphabetically
    as (string * int). *)

val statistics : storage -> float * float * float
(** [statistics (data : storage)] Computes statistics for the data: average,
    max, and min. *)

val most_common_article : storage -> string
(** [most_common_article (data : storage)] Finds the most common article based
    on count. *)

val total_count : storage -> int
(** [total_count (data : storage)] Computes the total count of mentions across
    all articles. *)

val display_csv : storage -> (string * float) list
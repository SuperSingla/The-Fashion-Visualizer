open GMain
open Fashion_csv
open Fashion_arithmetic
open Gtk

(* Define a type to represent the active graph *)
type graph_type =
  | BarGraph of (string * float) list * (float * float * float)
    (* (data, color) *)
  | PieChart of (string * float) list (* (data) *)

let add_statistics_label vbox storage =
  let avg, max_val, min_val = Fashion_arithmetic.statistics storage in
  let stats_text =
    Printf.sprintf "Statistics:\nAverage: %.2f\nMax: %.2f\nMin: %.2f" avg
      max_val min_val
  in
  let label = GMisc.label ~text:stats_text ~packing:vbox#add () in
  label#set_justify `LEFT;
  label#set_line_wrap true;
  ()

(* Main interface function *)
let interface csv_file =
  let _ = GtkMain.Main.init () in

  (* Create the main window *)
  let window =
    GWindow.window ~width:1000 ~height:800 ~title:"Fashion Tracker" ()
  in
  ignore (window#connect#destroy ~callback:GMain.quit);

  (* Create a horizontal box to organize widgets *)
  let hbox = GPack.hbox ~packing:window#add () in

  (* Create a vertical box for the buttons on the right *)
  let vbox_buttons = GPack.vbox ~packing:hbox#pack () in

  (* Create a drawing area for the graph *)
  let drawing_area = GMisc.drawing_area ~packing:hbox#pack () in
  drawing_area#misc#set_size_request ~width:800 ~height:600 ();

  let storage = Fashion_csv.load_prexisting csv_file Fashion_csv.empty in

  (* Add statistics display *)
  add_statistics_label vbox_buttons storage;

  (* Create buttons for displaying graphs *)
  let button_graph_1 =
    GButton.button ~label:"Display CSV" ~packing:vbox_buttons#add ()
  in
  let button_graph_2 =
    GButton.button ~label:"Display By Article" ~packing:vbox_buttons#add ()
  in
  let button_pie_chart =
    GButton.button ~label:"Display Pie Chart" ~packing:vbox_buttons#add ()
  in

  (* State variable to track the active graph *)
  let active_graph = ref None in

  (* Function to load CSV and convert it to storage *)
  let load_storage file_name = load_prexisting csv_file Fashion_csv.empty in

  (* Function to redraw the pie chart *)
  let draw_pie_chart cr data =
    let total = List.fold_left (fun acc (_, value) -> acc +. value) 0.0 data in
    let radius = 200.0 in
    let x_center = 400.0 in
    let y_center = 300.0 in
    let start_angle = 0.0 in

    (* List of 12 distinct colors *)
    let colors =
      [
        (0.6, 0.8, 0.6);
        (* Light green *)
        (0.8, 0.6, 0.6);
        (* Light red *)
        (0.6, 0.6, 0.8);
        (* Light blue *)
        (0.8, 0.8, 0.2);
        (* Yellow *)
        (0.6, 0.4, 0.8);
        (* Purple *)
        (0.4, 0.8, 0.4);
        (* Bright green *)
        (0.8, 0.4, 0.4);
        (* Salmon *)
        (0.4, 0.4, 0.8);
        (* Indigo *)
        (0.8, 0.6, 0.2);
        (* Orange *)
        (0.6, 0.8, 0.2);
        (* Lime *)
        (0.2, 0.6, 0.8);
        (* Cyan *)
        (0.8, 0.2, 0.6);
        (* Magenta *)
      ]
    in

    (* Recursive function to draw slices and build the legend *)
    let rec draw_slices_and_legend data start_angle colors y_legend =
      match (data, colors) with
      | [], _ -> ()
      | (label, value) :: rest_data, (r, g, b) :: rest_colors ->
          let angle = value /. total *. (2.0 *. Float.pi) in

          (* Draw the pie slice *)
          Cairo.move_to cr x_center y_center;
          Cairo.arc cr x_center y_center ~r:radius ~a1:start_angle
            ~a2:(start_angle +. angle);
          Cairo.line_to cr x_center y_center;
          Cairo.set_source_rgb cr r g b;
          Cairo.fill_preserve cr;
          Cairo.set_source_rgb cr 1.0 1.0 1.0;
          Cairo.stroke cr;

          (* Draw the legend *)
          Cairo.rectangle cr 650.0 y_legend ~w:20.0 ~h:20.0;
          Cairo.set_source_rgb cr r g b;
          Cairo.fill cr;
          Cairo.set_source_rgb cr 0.0 0.0 0.0;
          Cairo.stroke cr;

          (* Draw the label next to the color box *)
          Cairo.move_to cr 680.0 (y_legend +. 15.0);
          Cairo.show_text cr label;

          (* Recursive call for the next slice and legend entry *)
          draw_slices_and_legend rest_data (start_angle +. angle) rest_colors
            (y_legend +. 30.0)
      | (label, value) :: rest_data, [] ->
          let angle = value /. total *. (2.0 *. Float.pi) in

          (* Draw the pie slice *)
          Cairo.move_to cr x_center y_center;
          Cairo.arc cr x_center y_center ~r:radius ~a1:start_angle
            ~a2:(start_angle +. angle);
          Cairo.line_to cr x_center y_center;
          Cairo.set_source_rgb cr 0.6 0.8 0.6;
          Cairo.fill_preserve cr;
          Cairo.set_source_rgb cr 0.0 0.0 0.0;
          Cairo.stroke cr;

          (* Legend entry for the repeated color *)
          Cairo.rectangle cr 650.0 y_legend ~w:20.0 ~h:20.0;
          Cairo.set_source_rgb cr 0.6 0.8 0.6;
          Cairo.fill cr;
          Cairo.set_source_rgb cr 0.0 0.0 0.0;
          Cairo.stroke cr;

          (* Draw the label for repeated color *)
          Cairo.move_to cr 680.0 (y_legend +. 15.0);
          Cairo.show_text cr label;

          (* Recursive call for the next slice and legend entry *)
          draw_slices_and_legend rest_data (start_angle +. angle)
            [ (0.6, 0.8, 0.6) ]
            (y_legend +. 30.0)
    in

    (* Start drawing slices and the legend *)
    draw_slices_and_legend data start_angle colors 50.0
  in

  (* Function to redraw the graph based on the active graph *)
  let redraw_graph cr =
    match !active_graph with
    | Some (BarGraph (data, color)) ->
        let max_value =
          List.fold_left (fun acc (_, value) -> max acc value) 0.0 data
        in
        let scale = 300.0 /. max_value in
        let bar_width = 50.0 in
        let spacing = 20.0 in
        let x_start = 50.0 in
        let y_base = 600.0 in

        List.iteri
          (fun i (label, value) ->
            let x = x_start +. (float_of_int i *. (bar_width +. spacing)) in
            let bar_height = value *. scale in
            Cairo.rectangle cr x (y_base -. bar_height) ~w:bar_width
              ~h:bar_height;
            let r, g, b = color in
            Cairo.set_source_rgb cr r g b;
            Cairo.fill cr;
            Cairo.move_to cr (x +. 5.0) (y_base +. 20.0);
            Cairo.set_source_rgb cr 0.0 0.0 0.0;
            Cairo.show_text cr label)
          data
    | Some (PieChart data) -> draw_pie_chart cr data
    | None -> ()
  in

  (* Callback for drawing the graph *)
  let draw_graph () =
    ignore
      (drawing_area#misc#connect#draw ~callback:(fun cr ->
           redraw_graph cr;
           true));
    drawing_area#misc#queue_draw ()
  in

  (* Button click callbacks for graphs *)
  let on_button_graph_1 () =
    active_graph :=
      Some
        (BarGraph (load_storage "data/test.csv" |> display_csv, (0.8, 0.6, 0.8)));
    draw_graph ()
  in

  let on_button_graph_2 () =
    active_graph :=
      Some
        (BarGraph
           (load_storage "data/test.csv" |> sort_by_article, (1.0, 0.8, 0.6)));
    draw_graph ()
  in

  let on_button_pie_chart () =
    active_graph :=
      Some (PieChart (load_storage "data/test.csv" |> sort_by_article));
    draw_graph ()
  in

  (* Connect button signals *)
  ignore (button_graph_1#connect#clicked ~callback:on_button_graph_1);
  ignore (button_graph_2#connect#clicked ~callback:on_button_graph_2);
  ignore (button_pie_chart#connect#clicked ~callback:on_button_pie_chart);

  (* Initialize with no graph *)
  active_graph := None;

  window#show ();
  GtkMain.Main.main ()

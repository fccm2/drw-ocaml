#load "svg.cma"
#load "lst.cmo"
let ns = ref []
let env = ref []
let print_prm_1 prm =
  match prm with
  | (prm, v) -> Printf.sprintf " [%s %d]%!" prm v
let print_env_1 env =
  match env with
  | (key, lst) ->
    Printf.printf "# %s %s\n%!" key (String.concat "" (List.map print_prm_1 lst))
let input_line_opt ic =
  try Some(input_line ic)
  with End_of_file -> None
let input_char_opt ic =
  try Some(input_char ic)
  with End_of_file -> None
let read_file fn =
  let ic = open_in fn in
  let rec aux acc =
    match input_char_opt ic with
    | Some c -> aux (c::acc)
    | None -> close_in ic; String.concat "" (List.rev_map (String.make 1) acc)
  in
  aux []
let read_lines fn =
  let ic = open_in fn in
  let rec aux acc =
    match input_line_opt ic with
    | Some ln -> aux (ln::acc)
    | None -> close_in ic; List.rev(acc)
  in
  aux []
let save_file xml fn =
  let oc = open_out fn in
  String.iter (fun c -> output_char oc c) xml;
  close_out oc
let print_ns () =
  List.iter (fun (n,d) -> Printf.printf "> %d %s\n%!" d n) !ns
let print_env () =
  List.iter print_env_1 !env
let push_env prm =
  env := prm :: !env
let get_env () = !env
let pop_env () =
  match !env with
  | prm :: _ -> prm
  | [] -> ("void", [])
let shift_env () =
  match !env with
  | prm :: env_ -> env := env_ ; prm
  | [] -> assert false
  (*
# String.sub ;;
- : string -> int -> int -> string = <fun>
# let s = "#ff0080";;
val s : string = "#ff0080"
# String.sub s ;;
- : int -> int -> string = <fun>
# String.sub s 1 6;;
- : string = "ff0080"
  *)
let int_of_rgb_4 rgb =
  let n = String.length rgb in
  Printf.printf "# RGB: %d %s\n%!" n rgb;
  if n <> 4 then 0x00 else
  let r = String.sub rgb 1 1 in
  let g = String.sub rgb (1+1) 1 in
  let b = String.sub rgb (1+1+1) 1 in
  Printf.printf "# RGB: %s %s %s\n%!" r g b;
  let r, g, b =
   (int_of_string ("0x"^r),
    int_of_string ("0x"^g),
    int_of_string ("0x"^b))
  in
  Printf.printf "# RGB: %02x %02x %02x\n%!" r g b;
  let d =
   (((r)lsl 8) lor
    ((g)lsl 4) lor
     (b))
  in
  Printf.printf "# RGB: %03x\n%!" d;
  (d)
let int_of_rgb_7 rgb =
  let n = String.length rgb in
  Printf.printf "# RGB: %d %s\n%!" n rgb;
  if n <> 7 then 0x00 else
  let r = String.sub rgb 1 2 in
  let g = String.sub rgb (1+2) 2 in
  let b = String.sub rgb (1+2+2) 2 in
  Printf.printf "# RGB: %s %s %s\n%!" r g b;
  let r, g, b =
   (int_of_string ("0x"^r),
    int_of_string ("0x"^g),
    int_of_string ("0x"^b))
  in
  Printf.printf "# RGB: %02x %02x %02x\n%!" r g b;
  let d =
   (((r / 16)lsl 8) lor
    ((g / 16)lsl 4) lor
     (b / 16))
  in
  Printf.printf "# RGB: %03x\n%!" d;
  (d)
let int_of_rgb rgb =
  let n = String.length rgb in
  match n with
  | 4 -> int_of_rgb_4 rgb
  | 7 -> int_of_rgb_7 rgb
  | _ -> assert false
let name_elm elm name =
  let n = List.length !ns in
  ns := (name, n) :: !ns;
  match elm with
  | (elm, prm) -> let prm = Lst.add_last ("n", n) prm in (elm, prm)
let prm_kind prm =
  match prm with
  | ("circ", _) -> "circle"
  | ("rect", _) -> "rectangle"
  | _ -> ""
let circ_1 =
  ("circ", [("xc", 20); ("yc", 20); ("rc", 30); ("f", 0x00f);])
let rect_1 =
  ("rect", [("x", 20); ("y", 20); ("w", 60); ("h", 30); ("f", 0x00f);])
  (*
let rgb_circ prm (rgb) =
  match prm with
  | ("circ", [("xc",xc); ("yc",yc); ("rc",rc); ("f",_);]) ->
    let f = match rgb with | f -> f in
    ("circ", [("xc",xc); ("yc",yc); ("rc",rc); ("f",int_of_rgb f);])
  | v -> (v)
  *)
let rgb_elem prm (rgb) =
  match prm with
  | (elem, prms) ->
    (elem, Lst.ass_repl "f" (int_of_rgb rgb) prms)
let rgb_circ = rgb_elem
let rgb_rect = rgb_elem
  (*
let rgb_rect prm (rgb) =
  match prm with
  | ("rect", [("x",x); ("y",y); ("w",w); ("h",h); ("f",_)]) ->
    let f = match rgb with | f -> f in
    ("rect", [("x",x); ("y",y); ("w",w); ("h",h); ("f",int_of_rgb f)])
  | v -> (v)
  *)
let recolor_rect prm (color) =
  match prm with
  | ("rect", [("x",x); ("y",y); ("w",w); ("h",h); ("f",f)]) ->
    let f =
      match color with
      | "green" -> 0x0f0
      | "red" -> 0xf00
      | _ -> f
    in
    ("rect", [("x",x); ("y",y); ("w",w); ("h",h); ("f",f)])
  | v -> (v)
let height_rect prm (h) =
  match prm with
  | ("rect", [("x",x); ("y",y); ("w",w); ("h",_); ("f",f)]) ->
    ("rect", [("x",x); ("y",y); ("w",w); ("h",int_of_string h); ("f",f)])
  | v -> (v)
let radius_circ prm (r) =
  match prm with
  | ("circ", [("xc",xc); ("yc",yc); ("rc",_); ("f",cf)]) ->
    ("circ", [("xc",xc); ("yc",yc); ("rc",int_of_string r); ("f",cf)])
  | v -> (v)
let width_rect prm (w) =
  match prm with
  | ("rect", [("x",x); ("y",y); ("w",_); ("h",h); ("f",f)]) ->
    ("rect", [("x",x); ("y",y); ("w",int_of_string w); ("h",h); ("f",f)])
  | v -> (v)
let move_circ prm (xp, yp) =
  match prm with
  | ("circ", [("xc",xc); ("yc",yc); ("rc",rc); ("f",cf)]) ->
    ("circ", [("xc",xc+xp); ("yc",yc+yp); ("rc",rc); ("f",cf)])
  | v -> (v)
let move_rect prm (xp, yp) =
  match prm with
  | ("rect", [("x",x); ("y",y); ("w",w); ("h",h); ("f",f)]) ->
    ("rect", [("x",x+xp); ("y",y+yp); ("w",w); ("h",h); ("f",f)])
  | v -> (v)
let split_on s =
  let n = String.length s in
  let rec aux d acc1 acc2 =
    if d >= n then
       let s = (String.concat "") (List.rev_map (String.make 1) acc1) in
       let acc2 = (s::acc2) in (List.rev acc2)
    else match String.get s d with
    | ' ' ->
       let s = (String.concat "") (List.rev_map (String.make 1) acc1) in
       aux (d+1) ([]) (s::acc2)
    | c ->
       aux (d+1) (c::acc1) (acc2)
  in
  aux 0 [] []
type circ = { xc:int; yc:int; rc:int; cf:int; }
type rect = { x:int; y:int; w:int; h:int; f:int; }
type instr =
  | Circ of circ
  | Rect of rect
  | Void
let instr_of_prm prm =
  match prm with
  | ("rect", prms) ->
      let x = List.assoc "x" prms in
      let y = List.assoc "y" prms in
      let w = List.assoc "w" prms in
      let h = List.assoc "h" prms in
      let f = List.assoc "f" prms in
      Rect({ x; y; w; h; f; })
  | ("circ", prms) ->
      let xc = List.assoc "xc" prms in
      let yc = List.assoc "yc" prms in
      let rc = List.assoc "rc" prms in
      let cf = List.assoc "f" prms in
      Circ({ xc; yc; rc; cf; })
  | _ ->
      Void
  (*
let instr_of_prm prm =
  match prm with
  | ("rect", [("x",x); ("y",y); ("w",w); ("h",h); ("f",f)]) ->
      Rect({ x; y; w; h; f; })
  | ("circ", [("xc",xc); ("yc",yc); ("rc",rc); ("f",cf)]) ->
      Circ({ xc; yc; rc; cf; })
  | _ ->
      Void
  *)
let ret_svg instr () =
  let svg = Svg.new_svg_document ~width:120 ~height:90 () in
  begin
    let instr = List.rev instr in
    let rec aux instr =
      match instr with
      | instr :: next_instr ->
          begin match instr with
          | Circ circ -> let c = circ in
              let f = Printf.sprintf "#%03x" c.cf in
              Svg.Int.add_circle svg ~cx:c.xc ~cy:c.yc ~r:c.rc ~fill:f ();
          | Rect rect -> let r = rect in
              let f = Printf.sprintf "#%03x" r.f in
              Svg.add_rect svg ~x:r.x ~y:r.y ~width:r.w ~height:r.h ~fill:f ();
          | Void -> ()
          end;
          aux next_instr
      | [] -> ()
    in
    aux instr
  end;
  Svg.finish_svg svg;
  Svg.get_svg_document svg;
;;
let usage () =
  Printf.printf "draw a rectangle\n%!";
  Printf.printf "draw a circle\n%!";
  Printf.printf "\n%!";

  Printf.printf "recolor the rectangle rgb #8f2\n%!";
  Printf.printf "recolor the circle rgb #f82\n%!";
  Printf.printf "\n%!";

  Printf.printf "width the rectangle 25\n%!";
  Printf.printf "radius the circle 21\n%!";
  Printf.printf "\n%!";

  Printf.printf "move the rectangle left 1\n%!";
  Printf.printf "move the circle right\n%!";
  Printf.printf "\n%!";

  Printf.printf "print\n%!";
  Printf.printf "print env\n%!";
  Printf.printf "\n%!";

  Printf.printf "reload draw-05.svg\n%!";
  Printf.printf "save draw-01.svg\n%!";
  Printf.printf "\n%!";

  Printf.printf "list files\n%!";
  Printf.printf "\n%!";

  Printf.printf "shift left\n%!";
  Printf.printf "shift right\n%!";
  Printf.printf "\n%!";

  Printf.printf "name the rectangle Rcngl-01\n%!";
  Printf.printf "name the circle Crcl-02\n%!";
  Printf.printf "call Rcngl-01\n%!";
  Printf.printf "call Crcl-02\n%!";
  Printf.printf "\n%!";

  Printf.printf "quit\n%!";
;;
let polite ws =
  let ws = match ws with "please" :: ws -> ws | ws -> ws in
  begin match ws with
  | "move" :: "the" :: item :: "down" :: rem ->
    "move" :: "the" :: item :: "bottom" :: rem
  | "move" :: "the" :: item :: "up" :: rem ->
    "move" :: "the" :: item :: "top" :: rem

  | "quit" :: "please" :: [] -> exit 0
  | "recolor" :: "the" :: item :: "with" :: "rgb" :: rgb :: [] ->
    "recolor" :: "the" :: item :: "rgb" :: rgb :: []

  | "name" :: "the" :: "circle" :: name :: [] ->
    "name" :: "the" :: "element" :: name :: []

  | "name" :: "the" :: "rectangle" :: name :: [] ->
    "name" :: "the" :: "element" :: name :: []

  | "load" :: svg_file :: [] ->
    "reload" :: svg_file :: []

  | "move" :: "right" :: rem ->
     let item = prm_kind (pop_env ()) in
    "move" :: "the" :: item :: "right" :: rem

  | "move" :: "left" :: rem ->
     let item = prm_kind (pop_env ()) in
    "move" :: "the" :: item :: "left" :: rem

  | "move" :: "down" :: rem ->
     let item = prm_kind (pop_env ()) in
    "move" :: "the" :: item :: "bottom" :: rem

  | "move" :: "up" :: rem ->
     let item = prm_kind (pop_env ()) in
    "move" :: "the" :: item :: "top" :: rem

  | ws -> ws
  end
let () =
  let read = ref "" in
  while !read <> "quit" do
    read := read_line () ;
    if !read <> "quit"
    then begin
      let ws = split_on !read in
      List.iter (fun w -> Printf.printf " ('%s')" w) ws;
      print_newline ();
      let ws = polite ws in
      begin match ws with
      | "shift" :: shift_instr ->
          begin match shift_instr with
          | "left" :: [] -> env := Lst.shift_lft !env
          | "right" :: [] -> env := Lst.shift_rgh !env
          | _ -> ()
          end;
          (*
# Scanf.sscanf
   {|<rect x="20" y="20" width="60" height="30" fill="#00f" />|}
   {|<rect x="%d" y="%d" width="%d" height="%d" fill="%s />|} (fun x y w h f -> (x,y,w,h,f));;
- : int * int * int * int * string = (20, 20, 60, 30, "#00f\"")
          *)
      | "reload" :: svg_file :: [] ->
          let scan_circ nl =
          Scanf.sscanf nl
           {|<circle cx="%d" cy="%d" r="%d" fill="%s />|}
            (fun cx cy r f -> (cx,cy,r,f))
          in
          let scan_rect nl =
          Scanf.sscanf nl
           {|<rect x="%d" y="%d" width="%d" height="%d" fill="%s />|}
            (fun x y w h f -> (x,y,w,h,f))
          in
          let re_color rgb =
            let n = String.length rgb in
            if (String.get rgb (n-1))='"' then String.sub rgb 0 (n-1) else rgb
          in
          let s = read_file svg_file in
          print_endline s;
          let sl = read_lines svg_file in
          List.iter (fun nl ->
            let n = String.length nl in
            if n >= 5 then
            begin match String.sub nl 0 5 with
            | "<svg " -> ()
            | "<rect" ->
              let x, y, w, h, f = scan_rect nl in
              Printf.printf "# RECT: %d %d %d %d : %s\n%!" x y w h f;
              let f = re_color f in
              let rect_0 =
               ("rect", [("x", x); ("y", y); ("w", w); ("h", h); ("f", int_of_rgb f);])
              in
              push_env rect_0;
            | "<circ" ->
              let cx, cy, r, f = scan_circ nl in
              Printf.printf "# CIRC: %d %d %d : %s\n%!" cx cy r f;
              let f = re_color f in
              let circ_0 =
               ("circ", [("xc", cx); ("yc", cy); ("rc", r); ("f", int_of_rgb f);])
              in
              push_env circ_0;
            | _ -> ()
            end
          ) sl;
      | "save" :: filename :: [] ->
          let prms = get_env () in
          let instrs = List.map instr_of_prm prms in
          let svg_xml = ret_svg instrs () in
          save_file svg_xml filename;
      | "list" :: "files" :: [] ->
          let files = Sys.readdir "." in
          let files = Array.to_list files in
          let files = List.filter (fun file -> Filename.check_suffix file ".svg") files in
          List.iter (fun file ->
            Printf.printf "~ %s\n%!" file;
          ) files;
      | "print" :: [] ->
          let prms = get_env () in
          let instrs = List.map instr_of_prm prms in
          let svg_xml = ret_svg instrs () in
          print_endline svg_xml;
      | "print" :: "env" :: [] -> print_env (); print_ns ()
      | "usage" :: [] -> usage ()
      | "duplicate" :: [] ->
          let dup_prm = pop_env () in
          push_env dup_prm;
      | "draw" :: draw_instruct ->
          begin match draw_instruct with
          | "a" :: "circle" :: [] ->
              push_env circ_1
          | "a" :: "rectangle" :: [] ->
              push_env rect_1;
          | _ -> ()
          end;
      | "recolor" :: appr_instruct ->
          begin match appr_instruct with
          | "the" :: "circle" :: "rgb" :: rgb :: [] ->
              let prm = shift_env () in
              let mod_prm = rgb_circ prm rgb in
              push_env mod_prm;
          | "the" :: "rectangle" :: "rgb" :: rgb :: [] ->
              let prm = shift_env () in
              let mod_prm = rgb_rect prm rgb in
              push_env mod_prm;
          | "the" :: "rectangle" :: "in" :: color :: [] ->
              let prm = shift_env () in
              let mod_prm = recolor_rect prm color in
              push_env mod_prm;
          | _ -> ()
          end;
      | "radius" :: dims_instruct ->
          begin match dims_instruct with
          | "the" :: "circle" :: radius :: [] ->
              let prm = shift_env () in
              let mod_prm = radius_circ prm radius in
              push_env mod_prm;
          | _ -> ()
          end;
      | "height" :: dims_instruct ->
          begin match dims_instruct with
          | "the" :: "rectangle" :: height :: [] ->
              let prm = shift_env () in
              let mod_prm = height_rect prm height in
              push_env mod_prm;
          | _ -> ()
          end;
      | "width" :: dims_instruct ->
          begin match dims_instruct with
          | "the" :: "rectangle" :: width :: [] ->
              let prm = shift_env () in
              let mod_prm = width_rect prm width in
              push_env mod_prm;
          | _ -> ()
          end;
      | "call" :: call_instr ->
          begin match call_instr with
          | call_name :: [] ->
              let d = List.assoc call_name !ns in
              Printf.printf "# %d\n%!" d;
              let c_elm =
                List.find (fun (elm, prms) ->
                  List.exists (fun (p0,v0) ->
                    (p0 = "n") && (v0 = d)
                  ) prms
                ) !env
              in
              print_env_1 c_elm;
              let elm, env_ = Lst.rem_f (fun (elm) -> (elm=c_elm)) !env in
              env := elm::env_;
              print_env ();
          | _ -> ()
          end;
      | "name" :: name_instr ->
          begin match name_instr with
          | "the" :: "element" :: name :: [] ->
              let prm = shift_env () in
              let mod_prm = name_elm prm name in
              push_env mod_prm;
              print_env ();
              print_ns ()
          | _ -> ()
          end;
      | "move" :: move_instruct ->
          begin match move_instruct with
          | "the" :: "circle" :: dir :: dim :: [] ->
              let prm = shift_env () in
              let dir = match dir with "left" -> (-(int_of_string dim), 0) | "right" -> (int_of_string dim, 0)
              | "top" -> (0, -(int_of_string dim)) | "bottom" -> (0, int_of_string dim) | _ -> (0, 0)
              in
              let mod_prm = move_circ prm dir in
              push_env mod_prm;
          | "the" :: "circle" :: dir :: [] ->
              let prm = shift_env () in
              let dir = match dir with "left" -> (-10, 0) | "right" -> (10, 0)
              | "top" -> (0, -10) | "bottom" -> (0, 10) | _ -> (0, 0)
              in
              let mod_prm = move_circ prm dir in
              push_env mod_prm;
          | "the" :: "rectangle" :: dir :: dim :: [] ->
              let prm = shift_env () in
              let dir = match dir with "left" -> (-(int_of_string dim), 0) | "right" -> (int_of_string dim, 0)
              | "top" -> (0, -(int_of_string dim)) | "bottom" -> (0, int_of_string dim) | _ -> (0, 0)
              in
              let mod_prm = move_rect prm dir in
              push_env mod_prm;
          | "the" :: "rectangle" :: dir :: [] ->
              let prm = shift_env () in
              let dir = match dir with "left" -> (-10, 0) | "right" -> (10, 0)
              | "top" -> (0, -10) | "bottom" -> (0, 10) | _ -> (0, 0)
              in
              let mod_prm = move_rect prm dir in
              push_env mod_prm;
          | _ -> ()
          end;
      | _ -> ()
      end;
      let prms = get_env () in
      let instrs = List.map instr_of_prm prms in
      let svg_xml = ret_svg instrs () in
      let d = Sys.command (Printf.sprintf "echo '%s' | feh -" svg_xml) in
      assert (d=0);
    end
  done;
;;

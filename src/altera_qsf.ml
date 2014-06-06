(********************************************************************)
(* qsf file parser *)
(********************************************************************)

type t = 
    {
        mutable comment : string option;
        mutable disable : bool option;
        mutable entity : string option;
        mutable from : string option;
        mutable io_standard : string option;
        mutable remove : bool option;
        mutable rise : bool option;
        mutable fall : bool option;
        mutable section_id : string option;
        mutable tag : string option;
        mutable to_ : string option;
        mutable name : string option;
        mutable value : string option;
    }
    deriving(Show)

let empty () = 
    {
        comment = None;
        disable = None;
        entity = None;
        from = None;
        io_standard = None;
        remove = None;
        rise = None;
        fall = None;
        section_id = None;
        tag = None;
        to_ = None;
        name = None;
        value = None;
    }

let args t =
    let open Arg in 
    [
        "-comment", String(fun s -> t.comment <- Some s), "";
        "-disable", Unit(fun () -> t.disable <- Some true), "";
        "-entity", String(fun s -> t.entity <- Some s), "";
        "-from", String(fun s -> t.from <- Some s), "";
        "-io_standard", String(fun s -> t.io_standard <- Some s), "";
        "-remove", Unit(fun () -> t.remove <- Some true), "";
        "-rise", Unit(fun () -> t.rise <- Some true), "";
        "-fall", Unit(fun () -> t.fall <- Some true), "";
        "-section_id", String(fun s -> t.section_id <- Some s), "";
        "-tag", String(fun s -> t.tag <- Some s), "";
        "-to", String(fun s -> t.to_ <- Some s), "";
        "-name", String(fun s -> t.name <- Some s), "";
        "-value", String(fun s -> t.value <- Some s), "";
    ]

let anon t = (fun s -> t.value <- Some s)

type qsf = 
      Comment of string
    | Set_global_assignment of t
    | Set_location_assignment of t
    | Set_io_assignment of t
    | Set_instance_assignment of t 
    deriving(Show)

RE endl = space* ("#" _* )* ['\r' '\n']* eol
RE chr = graph#['\"']
RE chrs = chr|[' ']
RE ident0 = ('\"' (chrs* as name) '\"')
RE ident1 = (chr+ as name)
RE ident = ident1 | ident0 

let prep_line line = 
    let rec fmap = function
        | [] -> []
        | `Name n :: t-> n :: fmap t
        | `Text _ :: t -> fmap t
    in
    Array.of_list (fmap ((MAP ident -> `Name name) line))

let parse line = 
    match line with
    (* skip comments and blanks *)
    | RE [' ' '\t']* '#' (_* as x) -> Comment(x)
    | RE "" endl -> Comment("")
    | _ ->
        let line' = prep_line line in
        let parse () = 
            let e = empty () in
            Arg.parse_argv ~current:(ref 0) line' (args e) (anon e) "parse_line error";
            e
        in 
        match line'.(0) with
        | "set_global_assignment" -> Set_global_assignment( parse () )
        | "set_location_assignment" -> Set_location_assignment( parse () )
        | "set_io_assignment" -> Set_io_assignment( parse () )
        | "set_instance_assignment" -> Set_instance_assignment( parse () )
        | _ -> failwith line

let parse_stream_to_file board file_in file_out =
    lwt file_in = Lwt_io.(open_file ~mode:input file_in) in
    
    let stream = Lwt_io.read_lines file_in in
    let stream = Lwt_stream.map (fun s -> ("    " ^ Show.show<qsf> (parse s)) ^ ";") stream in
    
    lwt () = Lwt_io.write_line file_out ("let " ^ board ^ " = [") in
    lwt () = Lwt_io.write_lines file_out stream in
    lwt () = Lwt_io.write_line file_out "]" in

    lwt () = Lwt_io.close file_in in
    Lwt.return ()

let parse_stream file_in = 
    lwt file_in = Lwt_io.(open_file ~mode:input file_in) in
    
    let stream = Lwt_io.read_lines file_in in
    let stream = Lwt_stream.map parse stream in
    lwt list = Lwt_stream.to_list stream in
    lwt () = Lwt_io.close file_in in
    Lwt.return list


let gen_boards ?(path="/home/andyman/dev/altera/devkits/") file_out =
    let qsf = 
        [
            "de0_nano", "de0-nano/Demonstration/DE0_Nano_GOLDEN_TOP/DE0_Nano.qsf";
            "c4_15", "cycloneIVGX_4cgx15_start/examples/golden_top/golden_top.qsf";
            "c4_150", "cycloneIVGX_4cgx150_fpga/examples/golden_top/c4gx_f896_host.qsf";
        ]
    in
    lwt () = Lwt_io.write_line file_out "open Altera_qsf" in
    lwt () = Lwt_list.iter_s 
        (fun (board,qsf) -> 
            lwt () = parse_stream_to_file board (path ^ qsf) file_out in
            Lwt.return ()) 
        qsf 
    in
    Lwt.return ()

(********************************************************************)
(* back to tcl command *)
(********************************************************************)

let command_args x = 
    let str x y = 
        match y with
        | None -> ""
        | Some(y) ->
            " -" ^ x ^ " \"" ^ y ^ "\""
    in
    let bool x y = 
        match y with
        | Some(true) -> " -" ^ x
        | _ -> ""
    in
    let a = "" in
    let a = a ^ str "comment" x.comment in
    let a = a ^ bool "disable" x.disable in
    let a = a ^ str "entity" x.entity in
    let a = a ^ str "from" x.from in
    let a = a ^ str "io_standard" x.io_standard in
    let a = a ^ bool "rise" x.rise in
    let a = a ^ bool "fall" x.fall in
    let a = a ^ str "section_id" x.section_id in
    let a = a ^ str "tag" x.tag in
    let a = a ^ str "to" x.to_ in
    let a = a ^ str "name" x.name in
    let a = a ^ str "value" x.value in
    a

(********************************************************************)
(* query qsf data *)
(********************************************************************)

let unwrap = function None -> failwith "unwrap" | Some(x) -> x

let rec filter f = function
    | [] -> []
    | h::t -> 
        (match f h  with
        | None -> filter f t
        | Some h -> h :: filter f t)

let filter_global f = 
    filter (function Set_global_assignment x -> f x | _ -> None)
let filter_io f = 
    filter (function Set_io_assignment x -> f x | _ -> None)
let filter_instance f = 
    filter (function Set_instance_assignment x -> f x | _ -> None)
let filter_location f = 
    filter (function Set_location_assignment x -> f x | _ -> None)

let get_globals name board = 
   filter (function Set_global_assignment x when x.name = Some name -> x.value
                | _ -> None) board

let get_global name board = 
    match get_globals name board with
    | [] -> raise Not_found
    | h::t -> h

let location_assignments = 
    filter (function Set_location_assignment x -> Some(unwrap x.to_, unwrap x.value)
                   | _ -> None)

let instance_assignments name = 
    filter (function Set_instance_assignment x when x.name = Some(name) -> 
                       Some(unwrap x.to_, unwrap x.value)
                   | _ -> None)

let io_standards = instance_assignments "IO_STANDARD"
let max_toggle_rates = instance_assignments "IO_MAXIMUM_TOGGLE_RATE"
let slew_rates = instance_assignments "SLEW_RATE"
let pci_ios = instance_assignments "PCI_IO"
let current_strengths = instance_assignments "CURRENT_STRENGTH_NEW"

type pin_info = 
    {
        pin_name : string;
        pin_loc : string;
        pin_io_std : string;
        pin_current_strength : string;
        pin_pci_io : string;
        pin_slew_rate : string;
        pin_max_toggle_rate : string;
    }
    deriving (Show)

let get_pin_info ?(def_io="") board = 
    let locs = location_assignments board in
    let io = io_standards board in
    let tog = max_toggle_rates board in
    let slew = slew_rates board in
    let pci = pci_ios board in
    let cur = current_strengths board in

    let match_re name (name',_) =
       match name with
       | RE @name' -> true 
       | _ -> false
    in 
    let find name def l = 
        try snd (List.find (match_re name) l)
        with _ -> def
    in

    List.map (fun (pin_name,pin_loc) ->
        {   
            pin_name; 
            pin_loc; 
            pin_io_std = find pin_name def_io io;
            pin_slew_rate = find pin_name "" slew;
            pin_pci_io = find pin_name "" pci;
            pin_current_strength = find pin_name "" cur;
            pin_max_toggle_rate = find pin_name "" tog;
        }
    ) locs

let get_reserve_after_config = 
    filter_global 
        (fun x -> 
            match x.name with
            | None -> None 
            | Some(y) ->
                (match y with
                | RE _* Lazy "RESERVE_" _* Lazy "_AFTER_CONFIGURATION" ->
                    Some( Set_global_assignment x )
                | _ -> None))

type board_settings = 
    {
        (* FPGA part *)
        part : string;
        (* Top level entity name *)
        top_level_entity : string;
        files : ([ `Verilog | `SysVerilog | `Vhdl | `Qsys | `Sdc | `Misc] * 
                 [ `Leave | `Copy | `Generate of string -> unit Lwt.t ] *
                 string) list;
        default_io_std : string;
        reserve_after_config : qsf list;
        pins : pin_info list;
    }

let quote x = "\"" ^ x ^ "\""
let unwrap = function None -> Lwt.fail Not_found | Some(x) -> Lwt.return x

let write_reserve_after_config settings proc = 
    Lwt_list.iter_s (function 
        | Set_global_assignment x ->
            lwt name = unwrap x.name in
            lwt value = unwrap x.value in
            Altera_quartus.Project.set_global_assignment 
                ~name:name (quote value) proc
        | _ -> 
            Lwt.return ()
    ) settings.reserve_after_config

let write_pins settings proc = 
    Lwt_list.iter_s (fun x ->
        lwt () = 
            Altera_quartus.Project.set_location_assignment 
                ~to_:x.pin_name x.pin_loc proc
        in
        let assign name value = 
            if value <> "" then 
                Altera_quartus.Project.set_instance_assignment
                    ~to_:x.pin_name 
                    ~name:name (quote value) proc
            else 
                Lwt.return ()
        in
        lwt () = assign "IO_STANDARD" x.pin_io_std in
        lwt () = assign "PCI_IO" x.pin_pci_io in
        lwt () = assign "CURRENT_STRENGTH_NEW" x.pin_current_strength in
        lwt () = assign "SLEW_RATE" x.pin_slew_rate in
        lwt () = assign "IO_MAXIMUM_TOGGLE_RATE" x.pin_max_toggle_rate in
        Lwt.return ()
    ) settings.pins

let add_files settings proc = 
    Lwt_list.iter_s (fun (typ,oper,name) ->
        (* either leave, copy or generate file *)
        lwt name = 
            match oper with
            (* leave file where it is *)
            | `Leave -> Lwt.return name
            (* copy to project (ie current) dir *)
            | `Copy -> begin
                lwt () = Lwt_io.printf "copying %s to project dir\n" name in
                lwt _ = Altera_quartus.Utils.shell_bool ("cp " ^ name  ^ " .") in
                Lwt.return (Filename.basename name)
            end
            (* generate (presumably in current dir) *)
            | `Generate f -> begin
                lwt () = f name in
                Lwt.return name
            end
        in
        (* add file *)
        let typ = 
            match typ with
            | `Vhdl -> "VHDL_FILE"
            | `Verilog -> "VERILOG_FILE"
            | `SysVerilog -> "SYSTEMVERILOG_FILE"
            | `Qsys -> "QSYS_FILE"
            | `Sdc -> "SDC_FILE"
            | `Misc -> "MISC_FILE"
        in
        Altera_quartus.Project.set_global_assignment ~name:typ (quote name) proc
    ) settings.files

let apply_board_settings settings proc = 
    lwt () = write_reserve_after_config settings proc in
    lwt () = write_pins settings proc in
    lwt () = add_files settings proc in
    Lwt. return ()



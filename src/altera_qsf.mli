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

type qsf = 
      Comment of string
    | Set_global_assignment of t
    | Set_location_assignment of t
    | Set_io_assignment of t
    | Set_instance_assignment of t 

(* file parsing utility functions *)
val parse : string -> qsf
val parse_stream_to_file : string -> string -> Lwt_io.output Lwt_io.channel -> unit Lwt.t
val parse_stream : string -> qsf list Lwt.t
val gen_boards : ?path:string -> Lwt_io.output Lwt_io.channel -> unit Lwt.t

(* utils for dealing with qsf files *)
val command_args : t -> string
val filter : (qsf -> 'a option) -> qsf list -> 'a list
val filter_global : (t -> 'a option) -> qsf list -> 'a list
val filter_io : (t -> 'a option) -> qsf list -> 'a list
val filter_instance : (t -> 'a option) -> qsf list -> 'a list
val filter_location : (t -> 'a option) -> qsf list -> 'a list

val get_globals : string -> qsf list -> string list
val get_global : string -> qsf list -> string
val location_assignments : qsf list -> (string * string) list
val io_standards : qsf list -> (string * string) list

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

val get_pin_info : ?def_io:string -> qsf list -> pin_info list
val get_reserve_after_config : qsf list -> qsf list

(* board settings for the quartus generator *)

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

val quote : string -> string
val write_reserve_after_config : board_settings -> Lwt_process.process_full -> unit Lwt.t
val write_pins : board_settings -> Lwt_process.process_full -> unit Lwt.t
val add_files : board_settings -> Lwt_process.process_full -> unit Lwt.t
val apply_board_settings : board_settings -> Lwt_process.process_full -> unit Lwt.t

    


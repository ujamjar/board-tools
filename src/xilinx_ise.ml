open Lwt
open Utils

module Project = struct

  open Options

  let archive name = 
    Utils.command_bool (buildl "project archive" ~post:name)

  let clean = 
    Utils.command_bool (buildl "project clean")

  let close = 
    Utils.command_bool (buildl "project close")

  let get name ?process ?instance = 
    let args = " " ^ name in
    let args = args ^ arg_string_opt "-process" process in
    let args = args ^ arg_string_opt "-instance" instance in
    Utils.command_string (buildl "project get" ~args)

  let get_processes ?instance = 
    let args = arg_string_opt "-instance" instance in
    Utils.command_string (buildl "project get_processes" ~args)

  let _new name = 
    Utils.command_string (buildl "project new" ~post:name)

  let _open name = 
    Utils.command_string (buildl "project open" ~post:name)

  let properties ?process ?instance =
    let args = arg_string_opt "-process" process in
    let args = args ^ arg_string_opt "-instance" instance in
    Utils.command_strings (buildl "project properties" ~args)

  let set name value ?process ?instance = 
    let args = " " ^ name ^ " " ^ value in
    let args = args ^ arg_string_opt "-process" process in
    let args = args ^ arg_string_opt "-instance" instance in
    Utils.command_string (buildl "project set" ~args)

end

module Xfile = struct

  open Options

  let add file_name ?copy ?lib_vhdl ?view ?include_global = 
    let args = " " ^ file_name in
    let args = args ^ flag_opt "-copy" copy in
    let args = args ^ arg_string_opt "-lib_vhdl" lib_vhdl in
    let args = args ^ arg_of "-view" (function
      | `All -> "All"
      | `Implementation -> "Implementation"
      | `Simulation -> "Simulation"
      | `None -> "None") view
    in
    let args = args ^ flag_opt "-include_global" include_global in
    Utils.command_bool (buildl "xfile add" ~args)

  let get file_name prop = 
    let args = " " ^ file_name in
    let args = args ^ " " ^
      match prop with
      | `Name -> "name"
      | `Timestamp -> "timestamp"
      | `Include_global -> "include_global"
    in
    Utils.command_string (buildl "xfile get" ~args)

  let properties = 
    Utils.command_string (buildl "xfile properties")

  let remove file_name = 
    let args = " " ^ file_name in
    Utils.command_bool (buildl "xfile remove" ~args)

  let set file_name prop value = 
    let args = " " ^ file_name ^ " " ^ prop ^ " " ^ value in
    Utils.command_string (buildl "xfile set" ~args)

end

module Lib_vhdl = struct

  open Options

  let add_file lib_name file_name = 
    let args = " " ^ lib_name ^ " " ^ file_name in
    Utils.command_bool (buildl "lib_vhdl add_file" ~args)

  let delete lib_name = 
    let args = " " ^ lib_name in
    Utils.command_bool (buildl "lib_vhdl delete" ~args)

  let get lib_name prop = 
    let args = " " ^ lib_name ^ " " ^ prop in
    Utils.command_string (buildl "lib_vhdl get" ~args)

  let _new lib_name = 
    let args = " " ^ lib_name in
    Utils.command_bool (buildl "lib_vhdl new" ~args)

  let properties = 
    Utils.command_string (buildl "lib_vhdl properties")

end

module Process = struct

  open Options

  type task = [
    | `Back_annotate_Pin_Locations
    | `Behavioral_Check_Syntax
    | `Check_Syntax
    | `Create_Schematic_Symbol
    | `Generate_IBIS_Model
    | `Generate_Post_Map_Simulation_Model
    | `Generate_Post_Map_Static_Timing
    | `Generate_Post_Place_n_Route_Simulation_Model
    | `Generate_Post_Place_n_Route_Static_Timing
    | `Generate_Post_Synthesis_Simulation_Model
    | `Generate_Post_Translate_Simulation_Model
    | `Generate_Programming_File
    | `Generate_Text_Power_Report
    | `Implement_Design
    | `Map
    | `Place_n_Route
    | `Synthesize_XST
    | `Translate
  ]

  let string_of_task = function
    | `Back_annotate_Pin_Locations                  -> "Back-annotate Pin Locations" 
    | `Behavioral_Check_Syntax                      -> "Behavioral Check Syntax" 
    | `Check_Syntax                                 -> "Check Syntax" 
    | `Create_Schematic_Symbol                      -> "Create Schematic Symbol" 
    | `Generate_IBIS_Model                          -> "Generate IBIS Model" 
    | `Generate_Post_Map_Simulation_Model           -> "Generate Post-Map Simulation Model" 
    | `Generate_Post_Map_Static_Timing              -> "Generate Post-Map Static Timing" 
    | `Generate_Post_Place_n_Route_Simulation_Model -> "Generate Post-Place & Route Simulation Model" 
    | `Generate_Post_Place_n_Route_Static_Timing    -> "Generate Post-Place & Route Static Timing" 
    | `Generate_Post_Synthesis_Simulation_Model     -> "Generate Post-Synthesis Simulation Model" 
    | `Generate_Post_Translate_Simulation_Model     -> "Generate Post-Translate Simulation Model" 
    | `Generate_Programming_File                    -> "Generate Programming File"                     
    | `Generate_Text_Power_Report                   -> "Generate Text Power Report"                    
    | `Implement_Design                             -> "Implement Design"                              
    | `Map                                          -> "Map"                                           
    | `Place_n_Route                                -> "Place & Route"                                 
    | `Synthesize_XST                               -> "Synthesize - XST"                              
    | `Translate                                    -> "Translate"                                     


  let get task prop = 
    let args = " \"" ^ string_of_task task ^ "\"" in
    let args = args ^ " " ^ 
      match prop with
      | `Status -> "status"
      | `Name -> "name"
    in
    command_string (buildl "process get" ~args)

  let properties = 
    Utils.command_string (buildl "process properties")

  let run task ?instance ?force = 
    let args = " " ^ string_of_task task in
    let args = args ^ 
      match force with
      | None -> ""
      | Some `Rerun -> " -force rerun"
      | Some `Rerun_all -> " -force rerun_all"
    in
    Utils.command_bool (buildl "process run" ~args)

  let set task name value = 
    let args = " " ^ string_of_task task ^ " " ^ name ^ " " ^ value in
    Utils.command_string (buildl "process set" ~args)

end

module Globals = struct

  open Options

  let get prop = 
    let args = " " ^ prop in
    Utils.command_string (buildl "globals get" ~args)

  let properties = 
    Utils.command_string (buildl "globals properties")

  let set name value = 
    let args = " " ^ name ^ " " ^ value in
    Utils.command_string (buildl "globals set" ~args)

  let unset prop = 
    let args = " " ^ prop in
    Utils.command_string (buildl "globals unset" ~args)

end

(* module Collection, Object ... *)

let ise f = 
  Lwt_process.with_process_full 
    Lwt_process.(shell "xtclsh run.tcl")
      (fun p -> 
        lwt () = Utils.match_tag (fun _ -> ()) p#stdout in
        lwt r = f p in
        lwt _ = p#close in
        return r)

let run_ise f = 
    Lwt_main.run (
        match_lwt Utils.which "xtclsh" with
        | None -> fail (Failure "Can't find xtclsh.  Check $PATH.")
        | Some(_) -> ise f
    )


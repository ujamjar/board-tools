open Utils

module Project : sig

  val archive : string -> bool command
  
  val clean : bool command

  val close : bool command

  val get : string -> ?process:string -> ?instance:string -> string command

  val get_processes : ?instance:string -> string command

  val _new : string -> string command
  
  val _open : string -> string command

  val properties : ?process:string -> ?instance:string -> string list command

  val set : string -> string -> ?process:string -> ?instance:string -> string command

end

module Xfile : sig

  val add : string -> ?copy:bool -> ?lib_vhdl:string -> 
    ?view:[`All | `Implementation | `Simulation | `None] -> ?include_global:bool ->
    bool command

  val get : string -> [`Name | `Timestamp | `Include_global] -> string command

  val properties : string command

  val remove : string -> bool command

  val set : string -> string -> string -> string command

end

module Lib_vhdl : sig

  val add_file : string -> string -> bool command

  val delete : string -> bool command

  val get : string -> string -> string command

  val _new : string -> bool command

  val properties : string command

end

module Process : sig

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

  val string_of_task : task -> string

  val get : task -> [ `Status | `Name ] -> string command

  val properties : string command

  val run : task -> ?instance:string -> ?force:[`Rerun | `Rerun_all] -> bool command

  val set : task -> string -> string -> string command

end

module Globals : sig

  val get : string -> string command

  val properties : string command

  val set : string -> string -> string command

  val unset : string -> string command

end

val ise : 'a command -> 'a Lwt.t
val run_ise : 'a command -> 'a


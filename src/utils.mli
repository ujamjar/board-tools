type 'a command = Lwt_process.process_full -> 'a Lwt.t

val shell_bool : string -> bool Lwt.t
val shell_string_opt : string -> string option Lwt.t

val to_bool : 'a Lwt.t -> bool Lwt.t

val which : string -> string option Lwt.t
val getcwd : unit -> string Lwt.t
val ls : string -> string list Lwt.t
val is_dir : string -> bool Lwt.t
val is_link : string -> bool Lwt.t
val is_file : string -> bool Lwt.t
val rmdir : string -> unit Lwt.t
val mkdir : string -> unit Lwt.t
val direxists : string -> bool Lwt.t

(*********************************************************************)

val list_of_string : string -> char list
val string_of_list : char list -> string
val bytes_of_rlist : char list -> Lwt_bytes.t
val split_tokens : string -> string list Lwt.t

(*********************************************************************)

type 'a full_command = string -> 'a command
val match_tag' : char list -> (char option -> unit) -> Lwt_io.input_channel -> unit Lwt.t
val match_tag : (char option -> unit) -> Lwt_io.input_channel -> unit Lwt.t
val verbose : bool ref
val write_command : ?dump:bool -> Lwt_io.output_channel -> string -> unit Lwt.t
val command : (string Lwt_stream.t -> 'a Lwt.t) -> 'a full_command

(*********************************************************************)

val (-:) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val (:-) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val (--:) : 'a -> ('a -> 'b) -> 'b
val ($) : ('a -> 'b) -> 'a -> 'b

val string : string Lwt_stream.t -> string Lwt.t
val strings : string Lwt_stream.t -> string list Lwt.t

val int : string Lwt_stream.t -> int Lwt.t
val ints : string Lwt_stream.t -> int list Lwt.t

val bool : string Lwt_stream.t -> bool Lwt.t
val bools : string Lwt_stream.t -> bool list Lwt.t

val option : 'a Lwt.t -> 'a option Lwt.t

val unit : string Lwt_stream.t -> unit Lwt.t

val echo : (string Lwt_stream.t -> 'a Lwt.t) -> string Lwt_stream.t -> 'a Lwt.t
val progress : (string Lwt_stream.t -> 'a Lwt.t) -> string Lwt_stream.t -> 'a Lwt.t

(*********************************************************************)

val command_bool : bool full_command
val command_bool_opt : bool option full_command
val command_int : int full_command
val command_int_opt : int option full_command
val command_strings : string list full_command
val command_string : string full_command
val command_string_opt : string option full_command
val command_unit : unit full_command

val command_catch : (bool * string) full_command

val catch : (string Lwt_stream.t -> 'a Lwt.t) -> 'a option full_command

module Options : sig

    val flag1_of : ('a -> string) -> 'a -> string
    val flag_opt : string -> bool option -> string
    val int_opt : string -> int option -> string
    val string_opt : string -> string option -> string
    val arg1_of : string -> ('a -> string) -> 'a -> string
    val arg_of : string -> ('a -> string) -> 'a option -> string
    val arg_string_opt : string -> string option -> string
    val arg_int_opt : string -> int option -> string
    val strings : string list -> string

    val fold : ('a -> string) -> 'a list -> string

    val build : ?args:string -> ?post:string -> string -> string
    val buildl : ?args:string -> ?post:string -> string -> string

end


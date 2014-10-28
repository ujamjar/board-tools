open Lwt

type 'a command = Lwt_process.process_full -> 'a Lwt.t

let shell_bool c = 
    Lwt_process.(with_process_none (shell c)
        (fun p ->
            match_lwt p#status with
            | Unix.WEXITED 0 -> return true
            | _ -> return false))

let shell_string_opt c =
    try_lwt 
        lwt s = Lwt_process.(pread_line (shell c)) in
        Lwt.return (Some(s))
    with _ -> 
        Lwt.return None

let to_bool t = try_lwt lwt _ = t in return true with _ -> return false

let which c = shell_string_opt ("which " ^ c)

let getcwd () = (Lwt.wrap1 Unix.getcwd) ()

let with_cwd f = 
    lwt cwd = getcwd () in
    try_lwt f >>= return 
    finally Lwt_unix.chdir cwd

let ls dir = Lwt_stream.to_list (Lwt_unix.files_of_directory dir)

let is_dir s = Lwt_unix.stat s >>= (fun s -> return Unix.(s.st_kind = S_DIR))
let is_link s = Lwt_unix.stat s >>= (fun s -> return Unix.(s.st_kind = S_LNK))
let is_file s = Lwt_unix.stat s >>= (fun s -> return Unix.(s.st_kind = S_REG))

let rec rmdir dir = 
    lwt is_dir  = is_dir dir in
    if not is_dir then fail (Failure "not a directory")
    else
        lwt contents = ls dir in
        let contents = Lwt_list.filter_s 
            (fun d -> return (d<>"." && d<>"..")) contents 
        in
        match_lwt contents with
        | [] ->
            Lwt_unix.rmdir dir (* delete empty dir *)
        | _ as l ->
            lwt () = with_cwd (
                lwt () = Lwt_unix.chdir dir in
                (* delete children *)
                Lwt_list.iter_s 
                    (fun c ->
                        lwt s = Lwt_unix.stat c in
                        match Unix.(s.st_kind) with
                        | Unix.S_REG (* | Unix.S_LINK *) -> Lwt_unix.unlink c
                        | Unix.S_DIR -> rmdir c
                        | _ -> fail (Failure "unknown file type")) l
            ) in
            (* try again *)
            rmdir dir

let mkdir dir = 
    let rec f = function
        | [] -> return ()
        | dir::dirs -> begin
            lwt () = 
                try_lwt Lwt_unix.chdir dir 
                with Unix.Unix_error _ -> begin
                    lwt () = Lwt_unix.mkdir dir 0o775 in
                    Lwt_unix.chdir dir 
                end
            in
            f dirs
        end
    in
    lwt cwd = getcwd () in
    try_lwt
        f ((SPLIT "/") dir)
    finally
        Lwt_unix.chdir cwd

let direxists path = 
    try_lwt Lwt_unix.(lwt () = (opendir path >>= closedir) in return true)
    with _ -> return false

(***********************************************************)

let list_of_string s = 
    let len = String.length s in
    let rec f n = 
        if n=len then []
        else s.[n] :: f (n+1)
    in
    f 0

let string_of_list l = 
    let len = List.length l in
    let s = String.create len in
    let rec f n l = 
        match l with
        | [] -> s
        | c::cs -> begin
            s.[n] <- c;
            f (n+1) cs
        end
    in
    f 0 l

let bytes_of_rlist l = 
    let len = List.length l in
    let s = Lwt_bytes.create len in
    let rec f n l = 
        match l with
        | [] -> s
        | c::cs -> begin
            Lwt_bytes.set s (len-n-1) c;
            f (n+1) cs
        end
    in
    f 0 l

let rec split_tokens' s = 
    match s with
    | RE [' ']* "{" (_* Lazy as token) "}" (_* as rest) -> token :: split_tokens' rest
    | RE [' ']* (graph+ as token) (_* as rest) -> token :: split_tokens' rest
    | _ -> []
let split_tokens = wrap1 split_tokens'

(***********************************************************)
type 'a full_command = string -> 'a command

let tag = list_of_string "tcl> " 

let match_tag' tag push_char chan = 
    let rec matcher tag' accumulated = 
        match tag' with
        | [] -> 
            let () = push_char None in
            return ()

        | c::cs->
            lwt c' = Lwt_io.read_char chan in
            if c=c' then 
                matcher cs (c'::accumulated) 
            else 
                let accumulated = List.rev (c'::accumulated) in
                let () = List.iter (fun c -> push_char (Some c)) accumulated in
                matcher tag [] 
    in
    matcher tag [] 

let match_tag = match_tag' tag

let verbose = ref false
let write_command ?(dump=false) chan command = 
    lwt () = Lwt_io.fprintf chan "%s" command in
    lwt () = 
        if !verbose then Lwt_io.(fprintf stdout "%s" command) else return ()
    in
    return ()

let command f c p = 
    (* char stream and match tag *)
    let stream_char, push_char = Lwt_stream.create () in

    (* convert char stream to lines *)
    let stream_line, push_line = Lwt_stream.create () in
    let rec to_lines line = 
        let push_string () = push_line (Some (string_of_list (List.rev line))) in
        match_lwt Lwt_stream.get stream_char with
        | Some(c) ->
            if c = '\n' then
                let () = push_string () in
                to_lines [] 
            else
                to_lines (c :: line) 
        | None ->
              let () = if line = [] then () else push_string () in
              let () = push_line None in
              return ()
    in

    (* run the threads *)
    lwt () = write_command p#stdin c in
    lwt () = match_tag push_char p#stdout 
    and () = to_lines [] 
    and result = f stream_line in
    return result

(* combinators *)

let (-:) f g a = g (f a)
let (:-) f g a = f (g a)
let (--:) a f = f a
let ($) f a = f a

let strings stream = Lwt_stream.to_list stream
let string stream = 
    lwt line = Lwt_stream.next stream in
    lwt () = Lwt_stream.junk_while (fun _ -> true) stream in
    return line

let int' s = wrap1 int_of_string ((REPLACE "," -> "_") s)
let ints stream = Lwt_stream.to_list (Lwt_stream.map_s int' stream)
let int stream = 
    lwt line = string stream in
    int' line

let bool' b = 
    match b with
    | "0" -> return false
    | "1" -> return true
    | _ -> fail (Failure "bad bool")

let bool stream = 
    lwt line = string stream in
    bool' line

let bools stream = Lwt_stream.to_list (Lwt_stream.map_s bool' stream)

let option v = 
    try_lwt lwt v = v in return (Some v)
    with _ -> return None

let unit stream = Lwt_stream.junk_while (fun _ -> true) stream

type 'a echo = (string Lwt_stream.t -> 'a Lwt.t) -> string Lwt_stream.t -> 'a Lwt.t

let echo f stream = 
    let s' = Lwt_stream.clone stream in
    lwt () = Lwt_io.(write_lines stdout s') 
    and r = f stream in
    return r

let progress f stream =
    let s' = Lwt_stream.clone stream in
    lwt () =
        let rec f n = 
            try_lwt 
                lwt _ = (Lwt_stream.next s') in
                lwt () = Lwt_io.printf "\r%i" n in
                f (n+1)
            with _ ->
                Lwt_io.printf "\n"
        in
        f 1
    and r = f stream in
    return r

let command_bool = command (echo $ bool)
let command_bool_opt = command (echo $ bool -: option)
let command_int = command (echo $ int)
let command_int_opt = command (echo $ int -: option)
let command_strings = command (echo $ strings)
let command_string = command (echo $ string)
let command_string_opt = command (echo $ string -: option)
let command_unit = command (echo $ unit)

let command_catch command p =
    let command = 
        ("set return_value [ catch { " ^ command ^ " } result_value ]\n")
    in
    lwt () = command_unit command p in
    lwt ret = command_int "set return_value\n" p in
    if ret=0 then return (true,"")
    else 
        lwt res = command_string "set result_value\n" p in
        return (false,res)

let catch conv cmd p = 
  let cmd = 
      ("set return_value [ catch { " ^ cmd ^ " } result_value ]\n")
  in
  lwt () = command_unit cmd p in
  lwt ret = command_int "set return_value\n" p in
  if ret=0 then return None
  else 
      lwt res = command (echo $ conv) "set result_value\n" p in
      return (Some res)

module Options = struct

    let flag1_of f a = " " ^ f a
    let flag_opt f a = 
        match a with
        | Some(x) when x = true -> " " ^ f
        | _ -> ""
    let arg1_of x f a = " " ^ x ^ " " ^ f a
    let arg_of f g a = 
      match a with
      | None -> ""
      | Some(a) -> " " ^ f ^ " " ^ g a
    let int_opt f a = 
        match a with
        | None -> ""
        | Some(i) -> " " ^ f ^ " " ^ string_of_int i
    let string_opt f a = 
        match a with
        | None -> ""
        | Some(s) -> " " ^ f ^ " " ^ s
    
    let arg_string_opt a o = 
        match o with
        | None -> ""
        | Some(x) -> " " ^ a ^ " " ^ x
    let arg_int_opt a o = 
        match o with
        | None -> ""
        | Some(x) -> " " ^ a ^ " " ^ string_of_int x

    let fold f l = List.fold_left (fun a o -> a ^ " " ^ f o) "" l

    let strings = fold (fun s -> s)

    let build ?(args="") ?(post="") command = 
        command ^ args ^ (if post<>"" then " " ^ post else "")

    let buildl ?(args="") ?(post="") command = build ~args ~post command ^ "\n"
        
end


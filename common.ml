open Core
open Lwt    
open Cohttp    
open Cohttp_lwt_unix

module Environment = struct
  let system =
    match Sys.os_type with
    | "Unix" ->
      let ic = Unix.open_process_in "uname" in
      let uname = In_channel.input_line ic in
      let () = In_channel.close ic in
      (match uname with
        | Some  "Darwin" -> "osx"
        | _  -> "linux")
    | "Win32" -> "windows"
    | _ -> "common"
      
  let rows = match Terminal_size.get_rows () with
    | Some n -> n
    | None -> 25

  let columns = match Terminal_size.get_columns () with
    | Some n -> n
    | None -> 80
end

module Cache = struct
  type t =
    | Miss
    | Timeout
    | Hit of string
  
  let download_location = "https://tldr-pages.github.io/assets/tldr.zip"
  
  let use_cache =
    match Sys.getenv "TLDR_CACHE_ENABLED" with
    | Some "1" -> true
    | _        -> false

  let max_age =
    match Sys.getenv "TLDR_MAX_CACHE_AGE" with
    | Some n -> Float.of_string n
    | None   -> 24.

  let directory =
    let open Filename in
    match Sys.getenv "XDG_CACHE_HOME" with
    | Some path -> concat path "tldr"
    | None ->
      match Sys.getenv "HOME" with
      | Some path -> concat (concat path ".cache") "tldr"
      | None      -> concat (concat "~"  ".cache") "tldr"
  (* I con't know if this actually works *)

  let get_file_path command platform =
    Filename.concat directory (command ^ "_" ^ platform ^ ".md")

  let load_page command platform =
    let file = get_file_path command platform in
    let exists = file |> Fpath.v |> Bos.OS.File.exists in
    
    if use_cache && exists = Result.Ok true then
      let last_modified = (Unix.stat file).st_mtime 
      and cache_epoch = max_age *. 60. *. 60. *. 24. 
      and cur_time = Unix.time () in
      if cur_time -. last_modified > cache_epoch then
        Timeout
      else
        Hit (In_channel.read_all file)
    else 
      Miss

  let store_page page command platform =
    let file_path = (get_file_path command platform) in
    if directory |> Fpath.v |> Bos.OS.File.exists = Result.Ok false then
      ignore (Bos.OS.Dir.create (directory |> Fpath.v));
    Out_channel.write_all file_path ~data:page  
end


module Remote = struct
  type t =
    | Success of string
    | Missing
    | Error
  
  let default_remote = "https://raw.githubusercontent.com/tldr-pages/tldr/master/pages"

  let get_page_url ?(remote = default_remote) ?(platform = Environment.system) command =
      remote ^ "/" ^ platform ^ "/" ^ command ^ ".md"

  let get_page ?(remote = default_remote) ?(platform = Environment.system) command =
    let url = get_page_url ~remote:remote ~platform:platform command in
    let request = Client.get (Uri.of_string url) >>= fun (resp , body) ->
      let code = resp |> Response.status |> Code.code_of_status in
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      match code with
      | 200 -> Success body
      | 404 -> Missing
      | _   -> Error
    in
    Lwt_main.run request
end

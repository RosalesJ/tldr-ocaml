open Core
open Async
open Cohttp_async

type t =
  | Error of string
  | Success of string
  | Missing

module Environment = struct
  let system =
    match Sys.os_type with
    | "Unix" ->
       (Process.run_exn ~prog:"uname" ~args:[] () >>| function
         |"Darwin" -> "osx"
         | _       -> "linux")
    | "Win32"      -> return "windows"
    | _            -> return "common"
end

module Cache = struct
  let download_location = "https://tldr-pages.github.io/assets/tldr.zip"
  
  let use_cache =
    Sys.getenv "TLDR_CACHE_ENABLED"
    |> Option.value_map ~default:true ~f:(function "0" -> false | _ -> true)

  let max_age =
    Sys.getenv "TLDR_MAX_CACHE_AGE"
    |> Option.value_map ~default:24. ~f:Float.of_string
    |> Time.Span.of_day

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
      let open Time in
      Unix.stat file
      >>| (fun x -> x.mtime)
      >>| (fun last_modified ->
                   if now () > Time.add last_modified max_age then
                     Missing
                   else
                     Success (In_channel.read_all file))
    else
      return Missing

  let store_page page command platform =
    let file_path = (get_file_path command platform) in
    if directory |> Fpath.v |> Bos.OS.File.exists = Result.Ok false then
      ignore (Bos.OS.Dir.create (directory |> Fpath.v));
    Out_channel.write_all file_path ~data:page
end


module Remote = struct
  let default_remote = "https://raw.githubusercontent.com/tldr-pages/tldr/master/pages"

  let get_page_url ?(remote = default_remote) ?(platform = Environment.system) command =
    platform >>| fun x -> sprintf "%s/%s/%s.md" remote x command

  let get_page ?(remote = default_remote) ?(platform = Environment.system) command =
    let url = get_page_url ~remote:remote ~platform:platform command in
    (url >>| Uri.of_string) >>=
      Client.get >>=
      fun (resp , body) ->
      let code = resp |> Response.status |> Cohttp.Code.code_of_status in
      body |> Cohttp_async.Body.to_string >>| fun body ->
      match code with
      | 200 -> if   Cache.use_cache
               then Deferred.upon platform (Cache.store_page body command);
               Success body
      | 404 -> Missing
      | _   -> Error "There was an error with connection"
end

let (<|>) first second =
  Deferred.join (
  first >>| function
  | Missing -> second
  | x -> return x)


let get_page command platform =
  Cache.load_page command "common"
  <|> Cache.load_page command platform
  <|> Remote.get_page command ~platform:(return "common")
  <|> Remote.get_page command ~platform:(return platform)


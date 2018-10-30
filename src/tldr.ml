open Core
open Common


let no_documentation =
  Printf.sprintf "`%s` documentation is not available. \n Consider contributing Pull Request to https://github.com/tldr-pages/tldr\n"

let display = Display.display

let display_page command platform =
  let rec retrieve command platform =
    match Cache.load_page command platform with
    | Hit contents -> contents
    | _ -> 
      match Remote.get_page command ~platform:platform with
      | Missing when platform = "common" -> no_documentation command
      | Missing -> retrieve command "common"
      | Error -> "Error: Could not reach server or something"
      | Success contents ->
        Common.Cache.store_page contents command platform;
        contents
  in
  display (retrieve command platform)


let () =
  Command.basic ~summary:"tldr"
    Command.Let_syntax.(
      [%map_open
        let update_cache = flag "--update-cache" no_arg
            ~doc:"Update the cached commands"
        and os = flag "--os" (optional_with_default Environment.system string)
            ~doc:"Override the operating System [linux, osx, sunos, windows]"
        and command = anon (sequence ("command" %: string))
        in

        fun () ->
          if command = [] then
            printf "Please enter a command\n"
          else
            let command = String.concat ~sep:"-" command in
            ignore update_cache;
            display_page command os
      ])
  |> Command.run


open Common

let no_documentation =
  Printf.sprintf "`%s` documentation is not available. \n Consider contributing Pull Request to https://github.com/tldr-pages/tldr\n"

let usage_message = Printf.sprintf "%s: %s\n\n%s"
    "tldr"
    "Simplified man pages"
    "Usage: tldr <command>"

let doc = Printf.sprintf "%s\n"
    usage_message

let display_page command platform =
  match get_page command platform with
  | Missing -> Printf.printf "%s" (no_documentation command)
  | Error e -> Printf.printf "%s" e
  | Success page -> Display.display page

let () =
  let os = ref Environment.system in
  let update = ref false in
  let command = ref "" in

  let append_to_command str =
    if !command = "" then
      command := str
    else
      command := String.concat "-" [!command; str]
  in

  let spec_list = [("--os", Arg.Set_string os, "Override the operating System [linux, osx, sunos, windows]");
                   ("-p", Arg.Set_string os, "Override the operating System [linux, osx, sunos, windows]");
                   ("--update", Arg.Set update, "Update the cached commands");
                   ("--", Arg.Rest append_to_command, "The actual command")] in

  Arg.parse spec_list append_to_command doc;

  if !command = "" then
    Printf.printf "%s\n" usage_message
  else
    display_page !command !os

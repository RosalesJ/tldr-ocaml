open Base
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
  | Missing -> Stdio.printf "%s" (no_documentation command)
  | Error e -> Stdio.printf "%s" e
  | Success page -> Display.display page

let () =
  let os = ref Environment.system in
  let update = ref false in
  let command = ref "" in

  let append_to_command str =
    if String.equal !command "" then
      command := str
    else
      command := String.concat ~sep:"-" [!command; str]
  in

  let spec_list = [("--os", Caml.Arg.Set_string os, "Override the operating System [linux, osx, sunos, windows]");
                   ("-p", Caml.Arg.Set_string os, "Override the operating System [linux, osx, sunos, windows]");
                   ("--update", Caml.Arg.Set update, "Update the cached commands");
                   ("--", Caml.Arg.Rest append_to_command, "The actual command")] in

  Caml.Arg.parse spec_list append_to_command doc;

  if String.equal !command "" then
    Stdio.printf "%s\n" usage_message
  else
    display_page !command !os

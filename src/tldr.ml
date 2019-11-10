open Common
open Async

let no_documentation =
  Printf.sprintf "`%s` documentation is not available. \n Consider contributing Pull Request to https://github.com/tldr-pages/tldr\n"

let display_page command platform =
  get_page command platform
  >>| (function
       | Missing -> no_documentation command
       | Error e -> e
       | Success page -> Display.display page)
  
let () =
  let spec = Command.Spec.(
      empty
      +> anon ("filename" %: string))
  in
  
  Command.async_spec ~summary:"tldr" spec
    (fun x () -> display_page x "Unix" >>| print_string)
  |> Command.run


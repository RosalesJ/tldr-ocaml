open Base
open Common
open Cmdliner

let (<*>) = Term.($)

let no_documentation =
  Printf.sprintf "`%s` documentation is not available. \n Consider contributing Pull Request to https://github.com/tldr-pages/tldr"

let usage_message = Printf.sprintf "%s - %s\n\n%s"
    "tldr"
    "Simplified man pages"
    "Usage: tldr <command>"

let _doc = Printf.sprintf "%s"
    usage_message

let display_page command platform update_cache =
  ignore update_cache;
  match get_page command platform with
  | Missing -> Stdio.printf "%s\n" (no_documentation command)
  | Error e -> Stdio.printf "%s\n" e
  | Success page -> Display.display page

let command =
  let doc = "Display the tldr page for $(docv)" in
  Arg.(value & pos_all string [] & info [] ~docv:"command" ~doc)

let platform =
  let doc = "Display the command for a given $(docv)" in
  Arg.(value & opt string Environment.system & info ["p"; "platform"] ~docv:"PLATFORM" ~doc)

let update_cache =
  let doc = "Update the local cache" in
  Arg.(value & flag & info ["u"; "update"] ~docv: "update" ~doc)

let () =
  let info =
    let doc = "Simplified man pages" in
    let man = [
        `S Manpage.s_bugs;
        `P "Email bug reports to <coby@case.edu>." ]
    in
    Term.info "tldr" ~version:"0.3" ~doc ~exits:Term.default_exits ~man
  in

  let run_t = Term.(const display_page
                    <*> (pure (String.concat ~sep:"-") <*> command)
                    <*> platform
                    <*> update_cache) in

  Term.exit @@ Term.eval (run_t, info)

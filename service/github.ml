let solver = Ocaml_ci.Solver_pool.spawn_local ()

let () =
  Unix.putenv "DOCKER_BUILDKIT" "1";
  Unix.putenv "PROGRESS_NO_TRUNC" "1"

let main config mode fmt ovs winmac exit server repo =
  let repo = Current_git.Local.v (Fpath.v repo) in
  let engine = Current.Engine.create ~config (Pipeline.github ~exit ~solver ~fmt ~ovs ~winmac repo) in
  let site = Current_web.Site.(v ~has_role:allow_all) ~name:"ocaml-ci-github" (Current_web.routes engine) in
  Logging.run (
    Lwt.choose 
      ([Current.Engine.thread engine] @ (if server then [Current_web.run ~mode site] else []))
  )

(* Command-line parsing *)

open Cmdliner

let fmt = 
  Arg.value @@ 
  Arg.flag @@ 
  Arg.info 
    ~doc:"Enable lint and format checks" 
    ~docv:"FMT"
    ["f"; "fmt"]

let winmac = 
  let doc = "Add Windows and MacOS checks using Github Action runners (not Docker containers)" in 
  let docv = "WINMAC" in 
  Arg.(value & flag & info ~doc ~docv ["w"; "winmac"])

let ext = 
  let doc = "Exit after printing workflow to stdout" in 
  let docv = "EXIT" in 
  Arg.(value & flag & info ~doc ~docv ["e"; "exit"])

let server = 
  let doc = "Run the webserver to view the pipeline at localhost:8080" in 
  let docv = "SERVER" in 
  Arg.(value & flag & info ~doc ~docv ["s"; "server"])

let ovs = 
  let doc  = "Specify the number of most recent OCaml versions you want" in 
  let docv = "OVS" in
  Arg.(value & Arg.(opt int 3) & Arg.info ~doc ~docv ["o"; "ovs"])

let repo =
  Arg.required @@
  Arg.pos 0 Arg.(some dir) None @@
  Arg.info
    ~doc:"The directory containing the .git subdirectory."
    ~docv:"DIR"
    []

let cmd =
  let doc = "Produce Github Action workflows for testing your project" in
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $ fmt $ ovs $ winmac $ ext $ server $ repo ),
  Term.info "ocaml-ci-github" ~doc

let () = Term.(exit @@ eval cmd)
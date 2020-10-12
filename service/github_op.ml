open Ocaml_ci

let checkout_pool = Current.Pool.create ~label:"git-clone" 1
module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default
module Raw = Current_docker.Raw

type t = Builder.t


let commit_locks = Hashtbl.create 1000

let job_to_string j = 
  Yaml.to_string_exn (Obuilder_spec.Action.job_to_yaml j)

let job_of_string s = Obuilder_spec.Action.job_of_yaml (Yaml.of_string_exn s)

module Job_outcome = struct 
  type t = { variant: string; action : Obuilder_spec.Action.job }

  let to_yaml {variant; action} = 
    let open Obuilder_spec.Action in 
    `O [("variant", `String variant); ("action", job_to_yaml action)]

  let of_yaml = let open Obuilder_spec.Action in function 
    | `O [("variant", `String variant); ("action", yml)] -> { variant ; action = job_of_yaml yml}
    | _ -> failwith ""

  let marshal t = Yaml.to_string_exn (to_yaml t)
  let unmarshal s = 
    of_yaml (Yaml.of_string_exn s)
end 

let rec with_commit_lock ~job commit variant fn =
  let open Lwt.Infix in 
  let key = (Current_git.Commit.hash commit, variant) in
  match Hashtbl.find_opt commit_locks key with
  | Some lock ->
    Current.Job.log job "Waiting for a similar build to finish...";
    lock >>= fun () ->
    with_commit_lock ~job commit variant fn
  | None ->
    let finished, set_finished = Lwt.wait () in
    Hashtbl.add commit_locks key finished;
    Lwt.finalize fn
      (fun () ->
         Hashtbl.remove commit_locks key;
         Lwt.wakeup set_finished ();
         Lwt.return_unit
      )
let id = "ci-build"

module Key = struct
  type t = {
    commit : Current_git.Commit.t;            (* The source code to build and test *)
    repo : Current_github.Repo_id.t;          (* Used to choose a build cache *)
    label : string;                           (* A unique ID for this build within the commit *)
    github : Ocaml_ci.Github.t                (* Github configuration options *)
  }

  let to_json { commit; label; repo; github } =
    `Assoc [
      "commit", `String (Current_git.Commit.hash commit);
      "repo", `String (Fmt.to_to_string Current_github.Repo_id.pp repo);
      "label", `String label;
      "github", Ocaml_ci.Github.to_yojson github
    ]

  let digest t = Yojson.Safe.to_string (to_json t)
end

module Value = struct

  type t = {
    ty : Spec.ty;
    base : Raw.Image.t;                       (* The image with the OCaml compiler to use. *)
    variant : Variant.t;                      (* Added as a comment in the Dockerfile *)
  }

  let to_json { base; ty; variant } =
    `Assoc [
      "base", `String (Raw.Image.digest base);
      "op", Spec.ty_to_yojson ty;
      "variant", (Variant.to_yojson variant);
    ]

  let digest t = Yojson.Safe.to_string (to_json t)
end

module Outcome = Job_outcome

let or_raise = function
  | Ok () -> ()
  | Error (`Msg m) -> raise (Failure m)

let run { Builder.docker_context = _; pool; build_timeout } job
    { Key.commit; label; repo; github } { Value.base; variant; ty } =
  let open Obuilder_spec in 
  let gh = if label = "winmac" then Some github else Some ({ github with winmac = false}) in 
  let build_spec =
    let base = Raw.Image.hash base in
    match ty with
    | `Opam (`Build, selection, opam_files) -> Opam_build.spec ~github:gh ~base ~opam_files ~selection ()
    | `Opam (`Lint `Doc, selection, opam_files) -> Lint.doc_spec ~base ~opam_files ~selection
    | `Opam (`Lint `Opam, _selection, opam_files) -> Lint.opam_lint_spec ~base ~opam_files
    | `Opam_fmt ocamlformat_source -> Lint.fmt_spec ~base ~ocamlformat_source
    | `Duniverse -> Duniverse_build.spec ~base ~repo ~variant
  in
  let gh_workflow =
    if github.winmac && label = "winmac" 
    then Obuilder_spec.Action.workflow_of_spec ~oses:["windows-latest"; "macos-latest"] ~use_docker:false build_spec 
    else Obuilder_spec.Action.workflow_of_spec ~use_docker:true build_spec in
  Current.Job.write job
    (Fmt.strf "@[<v>Base: %a@,%a@]@."
        Raw.Image.pp base
        Spec.pp_summary ty);
  Current.Job.write job
    (Fmt.strf "@.\
                To reproduce locally:@.@.\
                %a@.\
                cat > .github/workflows/ocaml-ci.yml <<'END-OF-WORKFLOW'@.\
                \o033[34m%a\o033[0m@.\
                END-OF-WORKFLOW@."
        Current_git.Commit_id.pp_user_clone (Current_git.Commit.id commit)
        Action.pp gh_workflow);
  let open Lwt.Infix in 
  let workflow = Action.to_string gh_workflow in
  Current.Job.start ~timeout:build_timeout ~pool job ~level:Current.Level.Average >>= fun () ->
  with_commit_lock ~job commit variant @@ fun () ->
  Current_git.with_checkout ~pool:checkout_pool ~job commit @@ fun dir ->
  Current.Job.write job (Fmt.strf "Writing Workflow:@.%s@." workflow);
  Bos.OS.File.write Fpath.(dir / "ocaml-ci.yml") (workflow ^ "\n") |> or_raise;
  let res : Outcome.t = {variant = label; action =  gh_workflow.jobs} in 
  Lwt.return (Ok res)

let pp f ({ Key.repo; commit; label; github }, _) =
  Fmt.pf f "test %a %a (%s) \nGithub Configuration: %a"
    Current_github.Repo_id.pp repo
    Current_git.Commit.pp commit
    label
    Yojson.Safe.pp (Ocaml_ci.Github.to_yojson github)

let auto_cancel = true
let latched = true
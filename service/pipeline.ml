open Current.Syntax

open Ocaml_ci
let checkout_pool = Current.Pool.create ~label:"git-clone" 1
module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

let platforms =
  let schedule = Current_cache.Schedule.v ~valid_for:(Duration.of_day 30) () in
  let v { Conf.label; builder; pool; distro; ocaml_version; arch } =
    let base = Platform.pull ~arch ~schedule ~builder ~distro ~ocaml_version in
    let host_base =
      match arch with
      | `X86_64 -> base
      | _ -> Platform.pull ~arch:`X86_64 ~schedule ~builder ~distro ~ocaml_version
    in
    Platform.get ~arch ~label ~builder ~pool ~distro ~ocaml_version ~host_base base
  in
  Current.list_seq (List.map v Conf.platforms)

(* Link for GitHub statuses. *)
let url ~owner ~name ~hash = Uri.of_string (Printf.sprintf "https://ci.ocamllabs.io/github/%s/%s/commit/%s" owner name hash)

let opam_repository_commit =
  let repo = { Github.Repo_id.owner = "ocaml"; name = "opam-repository" } in
  Github.Api.Anonymous.head_of repo @@ `Ref "refs/heads/master"

let github_status_of_state ~head result =
  let+ head = head
  and+ result = result in
  let { Github.Repo_id.owner; name } = Github.Api.Commit.repo_id head in
  let hash = Github.Api.Commit.hash head in
  let url = url ~owner ~name ~hash in
  match result with
  | Ok _              -> Github.Api.Status.v ~url `Success ~description:"Passed"
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg m)    -> Github.Api.Status.v ~url `Failure ~description:m

let set_active_installations installations =
  let+ installations = installations in
  installations
  |> List.fold_left (fun acc i -> Index.Owner_set.add (Github.Installation.account i) acc) Index.Owner_set.empty
  |> Index.set_active_owners;
  installations

let set_active_repos ~installation repos =
  let+ installation = installation
  and+ repos = repos in
  let owner = Github.Installation.account installation in
  repos
  |> List.fold_left (fun acc r -> Index.Repo_set.add (Github.Api.Repo.id r).name acc) Index.Repo_set.empty
  |> Index.set_active_repos ~owner;
  repos

let set_active_refs ~repo xs =
  let+ repo = repo
  and+ xs = xs in
  let repo = Github.Api.Repo.id repo in
  Index.set_active_refs ~repo (
    xs |> List.fold_left (fun acc x ->
        let commit = Github.Api.Commit.id x in
        let gref = Git.Commit_id.gref commit in
        let hash = Git.Commit_id.hash commit in
        Index.Ref_map.add gref hash acc
      ) Index.Ref_map.empty
  );
  xs

let get_job_id x =
  let+ md = Current.Analysis.metadata x in
  match md with
  | Some { Current.Metadata.job_id; _ } -> job_id
  | None -> None

module Raw = Current_docker.Raw
let commit_locks = Hashtbl.create 1000

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
module Op = struct
  type t = Builder.t

  let id = "ci-build"

  module Key = struct
    type t = {
      commit : Current_git.Commit.t;            (* The source code to build and test *)
      repo : Current_github.Repo_id.t;          (* Used to choose a build cache *)
      label : string;                           (* A unique ID for this build within the commit *)
    }

    let to_json { commit; label; repo } =
      `Assoc [
        "commit", `String (Current_git.Commit.hash commit);
        "repo", `String (Fmt.to_to_string Current_github.Repo_id.pp repo);
        "label", `String label;
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

  module Outcome = Current.Unit

  let or_raise = function
    | Ok () -> ()
    | Error (`Msg m) -> raise (Failure m)

  let run { Builder.docker_context = _; pool; build_timeout } job
      { Key.commit; label = _; repo } { Value.base; variant; ty } =
    let open Obuilder_spec in 
    let build_spec =
      let base = Raw.Image.hash base in
      match ty with
      | `Opam (`Build, selection, opam_files) -> Opam_build.spec ~github:true ~base ~opam_files ~selection
      | `Opam (`Lint `Doc, selection, opam_files) -> Lint.doc_spec ~base ~opam_files ~selection
      | `Opam (`Lint `Opam, _selection, opam_files) -> Lint.opam_lint_spec ~base ~opam_files
      | `Opam_fmt ocamlformat_source -> Lint.fmt_spec ~base ~ocamlformat_source
      | `Duniverse -> Duniverse_build.spec ~base ~repo ~variant
    in
    let make_workflow () =
      Obuilder_spec.Action.workflow_of_spec build_spec
    in
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
          Action.pp (make_workflow ()));
    let open Lwt.Infix in 
    let workflow = Action.to_string (make_workflow ()) in
    Current.Job.start ~timeout:build_timeout ~pool job ~level:Current.Level.Average >>= fun () ->
    with_commit_lock ~job commit variant @@ fun () ->
    Current_git.with_checkout ~pool:checkout_pool ~job commit @@ fun dir ->
    Current.Job.write job (Fmt.strf "Writing Workflow:@.%s@." workflow);
    Bos.OS.File.write Fpath.(dir / "ocaml-ci.yml") (workflow ^ "\n") |> or_raise;
    Current.Process.exec ~cancellable:true ~job ("echo", [| "done writing files"|])

  let pp f ({ Key.repo; commit; label }, _) =
    Fmt.pf f "test %a %a (%s)"
      Current_github.Repo_id.pp repo
      Current_git.Commit.pp commit
      label

  let auto_cancel = true
  let latched = true
end 

module GC = Current_cache.Generic(Op)

let github_v ~platforms ~repo ~spec source = 
  Current.component "write" |>
  let> { Spec.variant; ty; label } = spec
  and> commit = source
  and> platforms = platforms
  and> repo = repo in
  match List.find_opt (fun p -> Variant.equal p.Platform.variant variant) platforms with 
  | Some { Platform.builder; variant; base; _ } ->
    GC.run builder { Op.Key.commit; repo; label } { Op.Value.base; ty; variant }
  | None ->
  (* We can only get here if there is a bug. If the set of platforms changes, [Analyse] should recalculate. *)
  let msg = Fmt.strf "BUG: variant %a is not a supported platform" Variant.pp variant in
  Current_incr.const (Error (`Msg msg), None)

let build_with_docker ?ocluster ?(github=true) ~repo ~analysis source =
  print_endline "USING GITHUB"; print_endline @@ string_of_bool github;
  Current.with_context analysis @@ fun () ->
  let specs =
    let+ analysis = Current.state ~hidden:true analysis in
    match analysis with
    | Error _ ->
        (* If we don't have the analysis yet, just use the empty list. *)
        []
    | Ok analysis ->
      match Analyse.Analysis.selections analysis with
      | `Duniverse variants ->
        variants
        |> List.rev_map (fun variant ->
            Spec.duniverse ~label:(Variant.to_string variant) ~variant
          )
      | `Opam_build selections ->
        let lint_selection = List.hd selections in
        let builds =
          selections |> List.map (fun selection ->
              let label = Variant.to_string selection.Selection.variant in
              Spec.opam ~label ~selection ~analysis `Build
            )
        and lint =
          [
            Spec.opam ~label:"(lint-fmt)" ~selection:lint_selection ~analysis (`Lint `Fmt);
            Spec.opam ~label:"(lint-doc)" ~selection:lint_selection ~analysis (`Lint `Doc);
            Spec.opam ~label:"(lint-opam)" ~selection:lint_selection ~analysis (`Lint `Opam);
          ]
        in
        lint @ builds
  in
  let builds = specs |> Current.list_map (module Spec) (fun spec ->
      let+ result =
        match ocluster, github with
        | None, false -> Build.v ~platforms ~repo ~spec source
        | None, true -> 
          let build = github_v ~platforms ~repo ~spec source in 
          let+ state = Current.state ~hidden:true build
          and+ job_id = get_job_id build
          and+ spec = spec in
          let result =
            state |> Result.map @@ fun () ->
            match spec.ty with
            | `Duniverse
            | `Opam (`Build, _, _) -> `Built
            | `Opam (`Lint (`Doc|`Opam), _, _) -> `Checked
            | `Opam_fmt _ -> `Checked
          in
          result, job_id
        | Some ocluster, _ ->
          let src = Current.map Git.Commit.id source in
          Cluster_build.v ocluster ~platforms ~repo ~spec src
      and+ spec = spec in
      Spec.label spec, result
    ) in
  let+ builds = builds
  and+ analysis_result = Current.state ~hidden:true (Current.map (fun _ -> `Checked) analysis)
  and+ analysis_id = get_job_id analysis in
  builds @ [
    "(analysis)", (analysis_result, analysis_id);
  ]

let list_errors ~ok errs =
  let groups =  (* Group by error message *)
    List.sort compare errs |> List.fold_left (fun acc (msg, l) ->
        match acc with
        | (m2, ls) :: acc' when m2 = msg -> (m2, l :: ls) :: acc'
        | _ -> (msg, [l]) :: acc
      ) []
  in
  Error (`Msg (
      match groups with
      | [] -> "No builds at all!"
      | [ msg, _ ] when ok = 0 -> msg (* Everything failed with the same error *)
      | [ msg, ls ] -> Fmt.strf "%a failed: %s" Fmt.(list ~sep:(unit ", ") string) ls msg
      | _ ->
        (* Multiple error messages; just list everything that failed. *)
        let pp_label f (_, l) = Fmt.string f l in
        Fmt.strf "%a failed" Fmt.(list ~sep:(unit ", ") pp_label) errs
    ))

let summarise results =
  results |> List.fold_left (fun (ok, pending, err, skip) -> function
      | _, Ok `Checked -> (ok, pending, err, skip)  (* Don't count lint checks *)
      | _, Ok `Built -> (ok + 1, pending, err, skip)
      | l, Error `Msg m when Astring.String.is_prefix ~affix:"[SKIP]" m -> (ok, pending, err, (m, l) :: skip)
      | l, Error `Msg m -> (ok, pending, (m, l) :: err, skip)
      | _, Error `Active _ -> (ok, pending + 1, err, skip)
    ) (0, 0, [], [])
  |> fun (ok, pending, err, skip) ->
  if pending > 0 then Error (`Active `Running)
  else match ok, err, skip with
    | 0, [], skip -> list_errors ~ok:0 skip (* Everything was skipped - treat skips as errors *)
    | _, [], _ -> Ok ()                     (* No errors and at least one success *)
    | ok, err, _ -> list_errors ~ok err     (* Some errors found - report *)


let local_test ~github ~solver repo () =
  let src = Git.Local.head_commit repo in
  let repo = Current.return { Github.Repo_id.owner = "local"; name = "test" }
  and analysis = Analyse.examine ~solver ~platforms ~opam_repository_commit src in
  Current.component "summarise" |>
  let> results = build_with_docker ~github ~repo ~analysis src in
  let result =
    results
    |> List.map (fun (variant, (build, _job)) -> variant, build)
    |> summarise
  in
  Current_incr.const (result, None)

let v ?ocluster ~app ~solver () =
  let ocluster = Option.map (Cluster_build.config ~timeout:(Duration.of_hour 1)) ocluster in
  Current.with_context opam_repository_commit @@ fun () ->
  Current.with_context platforms @@ fun () ->
  let installations = Github.App.installations app |> set_active_installations in
  installations |> Current.list_iter ~collapse_key:"org" (module Github.Installation) @@ fun installation ->
  let repos = Github.Installation.repositories installation |> set_active_repos ~installation in
  repos |> Current.list_iter ~collapse_key:"repo" (module Github.Api.Repo) @@ fun repo ->
  let refs = Github.Api.Repo.ci_refs repo |> set_active_refs ~repo in
  refs |> Current.list_iter (module Github.Api.Commit) @@ fun head ->
  let src = Git.fetch (Current.map Github.Api.Commit.id head) in
  let analysis = Analyse.examine ~solver ~platforms ~opam_repository_commit src in
  let builds =
    let repo = Current.map Github.Api.Repo.id repo in
    build_with_docker ?ocluster ~repo ~analysis src in
  let summary =
    builds
    |> Current.map (List.map (fun (variant, (build, _job)) -> variant, build))
    |> Current.map summarise
  in
  let status =
    let+ summary = summary in
    match summary with
    | Ok () -> `Passed
    | Error (`Active `Running) -> `Pending
    | Error (`Msg _) -> `Failed
  in
  let index =
    let+ commit = head
    and+ builds = builds
    and+ status = status in
    let repo = Current_github.Api.Commit.repo_id commit in
    let hash = Current_github.Api.Commit.hash commit in
    let jobs = builds |> List.map (fun (variant, (_, job_id)) -> (variant, job_id)) in
    Index.record ~repo ~hash ~status jobs
  and set_github_status =
    summary
    |> github_status_of_state ~head
    |> Github.Api.Commit.set_status head "ocaml-ci"
  in
  Current.all [index; set_github_status]

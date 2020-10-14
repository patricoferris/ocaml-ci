open Current.Syntax

open Ocaml_ci
(* let checkout_pool = Current.Pool.create ~label:"git-clone" 1 *)
module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

let make_platforms plats =
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
  Current.list_seq (List.map v plats)

let platforms = make_platforms Conf.platforms

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

module Op = Github_op
module GC = Current_cache.Generic(Op)

let github_v ~github ~platforms ~repo ~spec source = 
  Current.component "write" |>
  let> { Spec.variant; ty; label } = spec
  and> commit = source
  and> platforms = platforms
  and> repo = repo in
  match List.find_opt (fun p -> Variant.equal p.Platform.variant variant) platforms with 
  | Some { Platform.builder; variant; base; _ } ->
    GC.run builder { Op.Key.commit; repo; label; github } { Op.Value.base; ty; variant }
  | None ->
  (* We can only get here if there is a bug. If the set of platforms changes, [Analyse] should recalculate. *)
  let msg = Fmt.strf "BUG: variant %a is not a supported platform" Variant.pp variant in
  Current_incr.const (Error (`Msg msg), None)

let build_with_github ~repo ~platforms ~analysis ~fmt ~winmac ~ovs source = 
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
        (* Building the Windows and MacOS version solves the dependencies via Ubuntu *)
        let winmac_version = 
          if winmac then [ Spec.opam ~label:"winmac" ~selection:lint_selection ~analysis `Build] else []
        in 
        let builds =
          selections |> List.map (fun selection ->
              let label = Variant.to_string selection.Selection.variant in
              Spec.opam ~label ~selection ~analysis `Build
            )
        and lint = if fmt then 
          [
            Spec.opam ~label:"(lint-fmt)" ~selection:lint_selection ~analysis (`Lint `Fmt);
            Spec.opam ~label:"(lint-doc)" ~selection:lint_selection ~analysis (`Lint `Doc);
            Spec.opam ~label:"(lint-opam)" ~selection:lint_selection ~analysis (`Lint `Opam);
          ] else []
        in
        lint @ builds @ winmac_version
  in
  let builds = specs |> Current.list_map (module Spec) (fun spec ->
      let+ result = 
          let github : Ocaml_ci.Github.t = { winmac; ovs } in 
          let build = github_v ~github ~platforms ~repo ~spec source in 
          let+ state = Current.state ~hidden:true build
          and+ job_id = get_job_id build
          and+ spec = spec
          and+ b = build in 
          let result =
            state |> Result.map @@ fun _ ->
            match spec.ty with
            | `Duniverse
            | `Opam (`Build, _, _) -> `Built
            | `Opam (`Lint (`Doc|`Opam), _, _) -> `Checked
            | `Opam_fmt _ -> `Checked
          in
          result, Some b, job_id
      and+ spec = spec in
      Spec.label spec, result
    ) in
  let+ builds = builds
  and+ analysis_result = Current.state ~hidden:true (Current.map (fun _ -> `Checked) analysis)
  and+ analysis_id = get_job_id analysis in
  builds @ [
    "(analysis)", (analysis_result, None, analysis_id);
  ]

let build_with_docker ?ocluster ~repo ~analysis source =
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
        match ocluster with
        | None -> Build.v ~platforms ~repo ~spec source
        | Some ocluster ->
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

let summarise_github repo exit results =
  let open Workflow in 
  let dots_to_underscores s = String.(concat "_" (split_on_char '.' s)) in 
  let make_yaml (jobs : Github_op.Outcome.t list) : Yaml.value = 
    `O (List.map (fun (f : Github_op.Outcome.t) -> (dots_to_underscores f.variant, Types.job_to_yaml f.action.job)) jobs) 
  in  
  let jobs = results |> List.map (fun (_, job, _) -> match job with Some job -> [job] | None -> []) |> List.flatten in 
  let t = t (make_yaml jobs) |> with_name "Github OCaml-CI" |> with_on (simple_event ["push"; "pull_request"]) in 
  if List.length jobs > 0 then 
    (ignore (Bos.OS.Dir.create Fpath.(repo / ".github" / "workflows" ));
    let s = Fmt.(str "%a" (Pp.workflow ~drop_null:true (fun a -> a)) t) in
    (match Bos.OS.File.write Fpath.(repo / ".github" / "workflows" / "ocaml-ci.yml") s with 
    | Ok _ -> ()
    | Error (`Msg m) -> failwith m);
  if exit then Stdlib.exit 0);
  results |> List.fold_left (fun (ok, pending, err, skip) -> function
      | _, _, Ok `Checked -> (ok, pending, err, skip)  (* Don't count lint checks *)
      | _, _, Ok `Built -> (ok + 1, pending, err, skip)
      | l, _, Error `Msg m when Astring.String.is_prefix ~affix:"[SKIP]" m -> (ok, pending, err, (m, l) :: skip)
      | l, _, Error `Msg m -> (ok, pending, (m, l) :: err, skip)
      | _, _, Error `Active _ -> (ok, pending + 1, err, skip)
    ) (0, 0, [], [])
  |> fun (ok, pending, err, skip) ->
  if pending > 0 then Error (`Active `Running)
  else match ok, err, skip with
    | 0, [], skip -> list_errors ~ok:0 skip (* Everything was skipped - treat skips as errors *)
    | _, [], _ -> Ok ()                     (* No errors and at least one success *)
    | ok, err, _ -> list_errors ~ok err     (* Some errors found - report *)

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


let github ~exit ~solver ~fmt ~ovs ~winmac repo () =
  let r = Current_git.Local.v repo in 
  let src = Git.Local.head_commit r in
  let releases = Conf.github_platforms ~ovs in 
  let platforms = make_platforms releases in 
  let repo_c = Current.return { Github.Repo_id.owner = "local"; name = "test" } 
  and analysis = Analyse.examine ~solver ~platforms ~opam_repository_commit src in
    Current.component "Github Summary" |>
    let> results = 
      build_with_github 
        ~repo:repo_c ~platforms ~analysis ~fmt ~winmac 
        ~ovs:(Conf.most_recent ovs |> List.map Ocaml_version.to_string) src in
    let result =
      results
      |> List.map (fun (variant, (build, job, _job)) -> variant, job, build)
      |> summarise_github repo exit
    in
    Current_incr.const (result, None) 

let local_test ~solver repo () =
  let src = Git.Local.head_commit repo in
  let repo = Current.return { Github.Repo_id.owner = "local"; name = "test" }
  and analysis = Analyse.examine ~solver ~platforms ~opam_repository_commit src in
    Current.component "summarise" |>
    let> results = build_with_docker ~repo ~analysis src in
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

val download_cache : string

val install_for_github :
  ?_docker:bool ->
  opam_files:string list ->
  selection:Selection.t ->
  Obuilder_spec.op list

val install_project_deps :
  opam_files:string list ->
  selection:Selection.t ->
  Obuilder_spec.op list

val spec :
  ?github:bool ->
  base:string ->
  opam_files:string list ->
  selection:Selection.t ->
  Obuilder_spec.stage

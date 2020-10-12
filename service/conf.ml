let profile =
  match Sys.getenv_opt "CI_PROFILE" with
  | Some "production" -> `Production
  | Some "dev" | None -> `Dev
  | Some x -> Fmt.failwith "Unknown $PROFILE setting %S" x

module Capnp = struct
  (* Cap'n Proto RPC is enabled by passing --capnp-public-address. These values are hard-coded
     (because they're just internal to the Docker container). *)

  let cap_secrets =
    match profile with
    | `Production -> "/capnp-secrets"
    | `Dev -> "./capnp-secrets"

  let secret_key = cap_secrets ^ "/secret-key.pem"
  let cap_file = cap_secrets ^ "/ocaml-ci-admin.cap"
  let internal_port = 9000
end

let dev_pool = Current.Pool.create ~label:"docker" 1

(** Maximum time for one Docker build. *)
let build_timeout = Duration.of_hour 1

module Builders = struct
  let v docker_context =
    let docker_context, pool =
      Some docker_context, Current.Pool.create ~label:("docker-" ^ docker_context) 20
    in
    { Ocaml_ci.Builder.docker_context; pool; build_timeout }

  let local = { Ocaml_ci.Builder.docker_context = None; pool = dev_pool; build_timeout }
end

module OV = Ocaml_version
module DD = Dockerfile_distro

let default_compiler = OV.(Releases.latest |> without_patch)
let trunk_compiler = OV.(Sources.trunk |> without_patch)

type platform = {
  label : string;
  builder : Ocaml_ci.Builder.t;
  pool : string;
  distro : string;
  ocaml_version : OV.t;
  arch: OV.arch;
}

let pool_of_arch = function
| `X86_64 | `I386 -> "linux-x86_64"
| `Aarch32 | `Aarch64 -> "linux-arm64"
| `Ppc64le -> "linux-ppc64"

let platforms =
  let v ?(arch=`X86_64) label distro ocaml_version =
    { arch; label; builder = Builders.local; pool = pool_of_arch arch; distro; ocaml_version }
  in
  let make_distro distro =
    let label = DD.latest_tag_of_distro distro in
    let tag = DD.tag_of_distro distro in
    let ov = OV.(Releases.latest |> with_just_major_and_minor) in
    if distro = `Debian `V10 then
      v label tag (OV.with_variant ov (Some "flambda")) ::
      List.map (fun arch -> v ~arch label tag ov) (DD.distro_arches ov distro)
    else
      [v label tag ov]
  in
  let make_release ?arch ov =
    let distro = DD.tag_of_distro (`Debian `V10) in
    let ov = OV.with_just_major_and_minor ov in
    v ?arch (OV.to_string ov) distro ov in
  match profile with
  | `Production ->
      let distros = List.map make_distro [
        `Debian `V10; `Alpine `V3_12; `Ubuntu `V20_04;
        `Ubuntu `V18_04; `OpenSUSE `V15_2; `CentOS `V8;
        `Fedora `V32 ] |> List.flatten
      in
      (* The first one in this list is used for lint actions *)
      let ovs = List.rev OV.Releases.recent @ OV.Releases.unreleased_betas in
      List.map make_release ovs @ distros
  | `Dev ->
      let ovs = List.map OV.of_string_exn ["4.10"; "4.11"; "4.03"] in
      List.map make_release ovs @ [make_release ~arch:`I386 (List.hd ovs)]

let most_recent ovs = 
  let rec take_n n acc vs = match n, vs with 
    | 0, _ -> List.rev acc 
    | _, [] -> List.rev acc 
    | n, x::xs -> take_n (n - 1) (x::acc) xs 
  in
    take_n ovs [] (List.rev OV.Releases.recent)
    |> List.map OV.with_just_major_and_minor

let github_platforms ~ovs = 
  let v ?(arch=`X86_64) label distro ocaml_version =
    { arch; label; builder = Builders.local; pool = pool_of_arch arch; distro; ocaml_version }
  in
  let releases = most_recent ovs in 
  let make_distro distro =
    let label = DD.latest_tag_of_distro distro in
    let tag = DD.tag_of_distro distro in
      List.map (fun ov -> v label tag ov) releases
  in
  let distros = 
    List.map make_distro [
      `Debian `V10; `Alpine `V3_12; `Ubuntu `V18_04;
      `OpenSUSE `V15_2; `CentOS `V8
    ] |> List.flatten
  in
    distros
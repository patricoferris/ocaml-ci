let debian_10_vars ocaml_package ocaml_version =
  { Ocaml_ci_api.Worker.Vars.
    os = "debian";
    arch = "x86_64";
    os_family = "debian";
    os_distribution = "debian";
    os_version = "10";
    ocaml_package;
    ocaml_version
  }

let v = [
  "debian-10-ocaml-4.11", debian_10_vars "ocaml-variants" "4.11.0"; (* NOTE: This will change from ocaml-variants to ocaml-base-compiler once OCaml 4.11 is released *)
  "debian-10-ocaml-4.10", debian_10_vars "ocaml-base-compiler" "4.10.0";
  "debian-10-ocaml-4.09", debian_10_vars "ocaml-base-compiler" "4.09.0";
  "debian-10-ocaml-4.08", debian_10_vars "ocaml-base-compiler" "4.08.0";
  "debian-10-ocaml-4.07", debian_10_vars "ocaml-base-compiler" "4.07.0";
]

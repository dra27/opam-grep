(* SPDX-License-Identifier: MIT *)

module Cmd = Bos.Cmd
module Exec = Bos.OS.Cmd
module Dir = Bos.OS.Dir
module Path = Bos.OS.Path

let ( // ) = Fpath.( / )
let ( % ) = Cmd.( % )

exception OpamGrepError of string

let result = function
  | Ok x -> x
  | Error (`Msg msg) -> raise (OpamGrepError msg)

let dst () =
  let cachedir =
    match Sys.getenv_opt "XDG_CACHE_HOME" with
    | Some cachedir -> Fpath.v cachedir
    | None ->
        match Sys.getenv_opt "HOME" with
        | Some homedir -> Fpath.v homedir // ".cache"
        | None -> raise (OpamGrepError "Cannot find your home directory")
  in
  cachedir // "opam-grep"

let sync ~dst =
  let dst = dst // ".opam-repository" in
  let () =
    if result (Dir.exists dst) then
      (Cmd.v "git" % "-C" % Fpath.to_string dst % "pull" % "origin") |>
      Exec.run_out |>
      Exec.out_null |>
      Exec.success |>
      result
    else
      (Cmd.v "git" % "clone" % "https://github.com/ocaml/opam-repository.git" % Fpath.to_string dst) |>
      Exec.run_out |>
      Exec.out_null |>
      Exec.success |>
      result
  in
  let packages =
    (* XXX If there isn't an OpamRepository function to do this, there should be! *)
    let f package latest =
      let open OpamPackage in
      try
        if OpamPackage.Version.compare package.version (OpamPackage.Name.Map.find package.name latest).version = 1 then
          OpamPackage.Name.Map.add package.name package latest
        else
          latest
      with Not_found -> OpamPackage.Name.Map.add package.name package latest
    in
    OpamPackage.Set.fold f (OpamRepository.packages (OpamFilename.Dir.of_string (Fpath.to_string dst))) OpamPackage.Name.Map.empty
  in
  OpamPackage.Name.Map.values (OpamPackage.Name.Map.map OpamPackage.to_string packages)

let check ~dst pkg =
  let tmpdir = dst // "tmp" in
  let pkgdir = dst // pkg in
  if not (result (Dir.exists pkgdir)) then begin
    result (Dir.delete ~recurse:true tmpdir);
    let _ : (unit, _) result =
      (Cmd.v "opam" % "source" % "--dir" % Fpath.to_string tmpdir % pkg) |>
      Exec.run_out ~err:Exec.err_null |>
      Exec.out_null |>
      Exec.success
    in
    result (Path.move tmpdir pkgdir)
  end;
  pkgdir

let greps = [
  Cmd.v "rg"; (* ripgrep (fast, rust) *)
  Cmd.v "ugrep"; (* ugrep (fast, C++) *)
  Cmd.v "grep" (* grep (posix-ish) *)
]

let get_grep_cmd () =
  match List.find_opt (fun grep -> result (Exec.exists grep)) greps with
  | Some grep -> grep
  | None -> raise (OpamGrepError "Could not find any grep command")

let bar ~total =
  let module Line = Progress.Line in
  Line.list [ Line.spinner (); Line.bar total; Line.count_to total ]

let search ~regexp =
  let dst = dst () in
  prerr_endline "[Info] Getting the list of all known opam packages..";
  let pkgs = sync ~dst in
  let grep = get_grep_cmd () in
  prerr_endline ("[Info] Fetching and grepping using "^Cmd.get_line_tool grep^"..");
  Progress.with_reporter (bar ~total:(List.length pkgs)) begin fun progress ->
    List.iter begin fun pkg ->
      progress 1;
      let pkgdir = check ~dst pkg in
      match Exec.run (grep % "--binary" % "-qsr" % "-e" % regexp % Fpath.to_string pkgdir) with
      | Ok () ->
          let pkg = List.hd (String.split_on_char '.' pkg) in
          Progress.interject_with begin fun () ->
            print_endline (pkg^" matches your regexp.")
          end
      | Error _ -> () (* Ignore errors here *)
    end pkgs;
  end

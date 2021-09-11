module Cmd = Bos.Cmd
module Exec = Bos.OS.Cmd
module Dir = Bos.OS.Dir
module Path = Bos.OS.Path

let ( // ) = Fpath.( / )
let result = Rresult.R.failwith_error_msg

let list_split_bunch n l =
  let rec aux i acc = function
    | [] -> (acc, [])
    | x::xs when i < n -> aux (succ i) (x :: acc) xs
    | l -> (acc, l)
  in
  let rec accu acc l =
    match aux 0 [] l with
    | (x, []) -> x :: acc
    | (x, (_::_ as rest)) -> accu (x :: acc) rest
  in
  accu [] l

let successful_cmd ~msg = function
  | (x, (_, `Exited 0)) -> x
  | (_, (_, (`Exited _ | `Signaled _))) -> failwith msg

module Commands = struct
  let opam_list = Cmd.(v "opam" % "list" % "-A" % "-s" % "--color=never")
  let opam_show pkgs = Cmd.(v "opam" % "show" % "--color=never" % "-f" % "package" %% of_list pkgs)
  let opam_source ~path pkg = Cmd.(v "opam" % "source" % "--dir" % Fpath.to_string path % pkg)
  let ripgrep ~regexp ~dir = Cmd.(v "rg" % "--binary" % "-qsr" % "-e" % regexp % Fpath.to_string dir)
  let ugrep ~regexp ~dir = Cmd.(v "ugrep" % "--binary" % "-qsr" % "-e" % regexp % Fpath.to_string dir)
  let grep ~regexp ~dir = Cmd.(v "grep" % "--binary" % "-qsr" % "-e" % regexp % Fpath.to_string dir)
end

let dst () =
  let cachedir =
    match Sys.getenv_opt "XDG_CACHE_HOME" with
    | Some cachedir -> Fpath.v cachedir
    | None ->
        match Sys.getenv_opt "HOME" with
        | Some homedir -> Fpath.v homedir // ".cache"
        | None -> failwith "Cannot find your home directory"
  in
  cachedir // "opam-grep"

let sync ~dst =
  let _exists : bool = result (Dir.create ~path:true dst) in
  let pkgs_bunch =
    result (Exec.out_lines (Exec.run_out Commands.opam_list)) |>
    successful_cmd ~msg:"opam list failed" |>
    list_split_bunch 100
  in
  let opam_show pkgs =
    result (Exec.out_lines (Exec.run_out (Commands.opam_show pkgs))) |>
    successful_cmd ~msg:"opam grep failed"
  in
  List.map opam_show pkgs_bunch |> List.concat |> List.sort_uniq String.compare

let check ~dst pkg =
  let tmpdir = dst // "tmp" in
  let pkgdir = dst // pkg in
  if not (result (Dir.exists pkgdir)) then begin
    result (Dir.delete ~recurse:true tmpdir);
    let _ : (unit, _) result = Exec.success (Exec.out_null (Exec.run_out ~err:Exec.err_null (Commands.opam_source ~path:tmpdir pkg))) in
    result (Path.move tmpdir pkgdir)
  end

let get_grep_cmd () =
  (* TODO: Avoid using dummy arguments *)
  let dir = result (Dir.current ()) in
  let ripgrep = Commands.ripgrep ~regexp:"" ~dir in
  let ugrep = Commands.ugrep ~regexp:"" ~dir in
  let grep = Commands.grep ~regexp:"" ~dir in
  if result (Exec.exists ripgrep) then
    Commands.ripgrep
  else if result (Exec.exists ugrep) then
    Commands.ugrep
  else if result (Exec.exists grep) then
    Commands.grep
  else
    failwith "Could not find any grep command"

let bar ~total =
  let module Line = Progress.Line in
  Line.list [ Line.spinner (); Line.bar total; Line.count_to total ]

let search ~regexp =
  let dst = dst () in
  let pkgs = sync ~dst in
  let grep = get_grep_cmd () in
  Progress.with_reporter (bar ~total:(List.length pkgs)) begin fun progress ->
    List.iter begin fun pkg ->
      progress 1;
      check ~dst pkg;
      match Exec.run (grep ~regexp ~dir:(dst // pkg)) with
      | Ok () ->
          let pkg = List.hd (String.split_on_char '.' pkg) in
          Progress.interject_with begin fun () ->
            print_endline (pkg^" matches your regexp.")
          end
      | Error _ -> () (* Ignore errors here *)
    end pkgs;
  end

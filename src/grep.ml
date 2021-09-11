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
  let opam_list = Bos.Cmd.(v "opam" % "list" % "-A" % "-s" % "--color=never")
  let opam_show pkgs = Bos.Cmd.(v "opam" % "show" % "--color=never" % "-f" % "package" %% of_list pkgs)
  let opam_source ~path pkg = Bos.Cmd.(v "opam" % "source" % "--dir" % Fpath.to_string path % pkg)
  let ripgrep ~regexp ~dir = Bos.Cmd.(v "rg" % "--binary" % "-qsr" % "-e" % regexp % Fpath.to_string dir)
  let ugrep ~regexp ~dir = Bos.Cmd.(v "ugrep" % "--binary" % "-qsr" % "-e" % regexp % Fpath.to_string dir)
  let grep ~regexp ~dir = Bos.Cmd.(v "grep" % "--binary" % "-qsr" % "-e" % regexp % Fpath.to_string dir)
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
  let _exists : bool = result (Bos.OS.Dir.create ~path:true dst) in
  let pkgs_bunch =
    result (Bos.OS.Cmd.out_lines (Bos.OS.Cmd.run_out Commands.opam_list)) |>
    successful_cmd ~msg:"opam list failed" |>
    list_split_bunch 100
  in
  let opam_show pkgs =
    result (Bos.OS.Cmd.out_lines (Bos.OS.Cmd.run_out (Commands.opam_show pkgs))) |>
    successful_cmd ~msg:"opam grep failed"
  in
  List.map opam_show pkgs_bunch |> List.concat

let check ~dst pkg =
  let tmpdir = dst // "tmp" in
  let pkgdir = dst // pkg in
  if not (result (Bos.OS.Dir.exists pkgdir)) then begin
    let _ : (unit, _) result = Bos.OS.Cmd.run (Commands.opam_source ~path:tmpdir pkg) in
    result (Bos.OS.Path.move tmpdir pkgdir)
  end

let spin =
  let spin = ref "/" in
  fun () ->
    print_string ("\033[2K\rSearching: " ^ !spin);
    flush stdout;
    match !spin with
    | "|" -> spin := "/"
    | "/" -> spin := "-"
    | "-" -> spin := "\\"
    | "\\" -> spin := "|"
    | _ -> assert false

let get_grep_cmd () =
  (* TODO: Avoid using dummy arguments *)
  let dir = result (Bos.OS.Dir.current ()) in
  let ripgrep = Commands.ripgrep ~regexp:"" ~dir in
  let ugrep = Commands.ugrep ~regexp:"" ~dir in
  let grep = Commands.grep ~regexp:"" ~dir in
  if result (Bos.OS.Cmd.exists ripgrep) then
    Commands.ripgrep
  else if result (Bos.OS.Cmd.exists ugrep) then
    Commands.ugrep
  else if result (Bos.OS.Cmd.exists grep) then
    Commands.grep
  else
    failwith "Could not find any grep command"

let search ~regexp =
  let dst = dst () in
  let pkgs = sync ~dst in
  let grep = get_grep_cmd () in
  List.iter begin fun pkg ->
    spin ();
    check ~dst pkg;
    match Bos.OS.Cmd.run (grep ~regexp ~dir:(dst // pkg)) with
    | Ok () ->
        let pkg = List.hd (String.split_on_char '.' pkg) in
        print_endline ("\033[2K\r"^pkg^" matches your regexp.")
    | Error _ -> () (* Ignore errors here *)
  end pkgs;
  print_endline "\033[2K\rUpdate complete."

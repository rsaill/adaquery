let fullname (fn:string) : string =
  if Filename.is_relative fn then
    (Sys.getcwd ()) ^ "/" ^ fn
  else fn

let get_cache_dir (project:string) : string =
  let dir =
    try (Unix.getenv "HOME") ^ "/.adaquery"
    with Not_found -> "."
  in
  (if not (Sys.file_exists dir) then
     if Sys.command ("mkdir " ^ dir) = 0 then
       Print.debug "Directory '%s' was created." dir
     else
       ( Print.fail "Fail to create directory '%s'." dir )
  );
  dir

let get_cache_file (project:string) : string =
  let dir = get_cache_dir project in
  Printf.sprintf "%s/%s.cache" dir project

let get_alias_file (project:string) : string =
  let dir = get_cache_dir project in
  Printf.sprintf "%s/%s.alias" dir project

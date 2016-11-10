open Datatypes

(* Types *)

module M = Map.Make(String)

type t_leaf =
  | L_Incomplete_Alias of loc
  | L_Subprog of loc
  | L_Type of loc
  | L_Enum of loc
  | L_Number of loc
  | L_Object of loc
  | L_Exception of loc

let leaf_to_string = function
  | L_Incomplete_Alias _ -> "Incomplete Alias"
  | L_Subprog _ -> "Subprog"
  | L_Type _ -> "Type"
  | L_Enum _ -> "Enum"
  | L_Number _ -> "Number"
  | L_Object _ -> "Object"
  | L_Exception _ -> "Exception"

type t_tree = t_branch M.t
and t_branch =
  | Leaf of t_leaf list
  | Alias of loc*path
  | Node of (loc*bool)*bool*t_tree

type t = (string,t_branch) Hashtbl.t

(* Print *)

let its indent = String.make indent ' '

let rec print_branch (ident:int) out : t_branch -> unit = function
  | Leaf [l] -> Printf.fprintf out "%s\n" (leaf_to_string l)
  | Leaf [] -> assert false
  | Leaf (l::_) -> Printf.fprintf out "%s and other definitions\n" (leaf_to_string l)
  | Node (_,_,tree) -> print_tree ident out tree
  | Alias (_,path) -> Printf.fprintf out "Alias of %s\n" (String.concat "." path)

and print_tree (ident:int) out (tree:t_tree) : unit =
  Printf.fprintf out "Package\n";
  M.iter ( fun key br ->
      Printf.fprintf out "%s%s -> %a" (its ident) key (print_branch (ident+2)) br
    ) tree

(* Utils *)

let designator_to_string : t_designator -> ident option = function
  | Compound_Name [id] -> Some id
  | String_Name id -> Some id
  | Compound_Name lst ->
   begin
     Print.debug "Complex subprogram name '%s', skipping."
       (String.concat "." (List.map snd lst));
     None;
   end

let tname_to_path (tname:t_name) : path option =
  let rec aux : t_name -> path = function
  | Simple_name id -> [snd id]
  | No_name -> raise (Failure "No_name")
  | Selected_comp (tname,id) -> (snd id)::(aux tname)
  in
  try Some (List.rev (aux tname))
  with Failure _ -> None

(* Read/Write/Create table *)

let write (tbl:t) (fn:string) : unit =
  let out = open_out_bin fn in
  Marshal.to_channel out tbl []

let read (fn:string) : t =
  let dpin = open_in fn in
  Marshal.from_channel dpin

let create () : t = Hashtbl.create 147

(* Building the table *)

let rec merge_trees (tr1:t_tree) (tr2:t_tree) : t_tree =
  let aux_opt _ a_opt b_opt =
    match a_opt, b_opt with
    | None, _ -> b_opt
    | _, None -> a_opt
    | Some a, Some b -> Some (merge_branches a b)
  in
  M.merge aux_opt tr1 tr2

and merge_branches a b =
  let merge_opt_loc lc1 lc2 = if (snd lc1) then lc1 else lc2 in
  match a, b with
  | Leaf lst1, Leaf lst2 -> Leaf (lst1@lst2)
  | Node (lc1,isg1,tr1), Node (lc2,isg2,tr2) ->
    Node (merge_opt_loc lc1 lc2, isg1||isg2, merge_trees tr1 tr2)
  | (Leaf _|Alias _), (Node _ as n) | (Node _ as n), (Leaf _|Alias _) ->
    ( Print.debug "A package has the same name than an object or an alias. Keeping the package."; n)
  | (Alias _ as n), Leaf _ | _, (Alias _ as n)->
    ( Print.debug "An alias has the same name than a package. Keeping the alias."; n)

let tree_of_leaves (tree:t_tree) (f:loc -> t_leaf) (lst:ident list) : t_tree =
  List.fold_left (
    fun tree (lc,name) -> merge_trees tree (M.singleton name (Leaf [f lc]))
  ) tree lst 

let rec decl_to_tree : t_decl -> t_tree = function
  | Package (name,lc,is_g,Decl lst) ->
    let aux (tr:t_tree) (d:t_decl) : t_tree = merge_trees tr (decl_to_tree d) in
    let tree:t_tree = List.fold_left aux M.empty lst in
    M.singleton name (Node (lc,is_g,tree))
  | Package (name,lc,_,New tn) ->
    begin match tname_to_path tn with
      | None -> M.singleton name (Leaf [L_Incomplete_Alias (fst lc)])
      | Some path -> M.singleton name (Alias (fst lc,path))
    end
  | Package (name,lc,_,Renamed tn) ->
    begin match tname_to_path tn with
      | None -> M.singleton name (Leaf [L_Incomplete_Alias (fst lc)])
      | Some path -> M.singleton name (Alias (fst lc,path))
    end
  | Subprog name -> 
    begin match designator_to_string name with
      | Some (lc,name) -> M.singleton name (Leaf [L_Subprog lc])
      | None -> M.empty
    end
  | Type ((lc,name),opt) ->
    let map = M.singleton name (Leaf [L_Type lc]) in
    begin match opt with
      | None -> map
      | Some lst ->  tree_of_leaves map (fun lc -> L_Enum lc) lst
    end
  | Number lst -> tree_of_leaves M.empty (fun lc -> L_Number lc) lst
  | Object lst -> tree_of_leaves M.empty (fun lc -> L_Object lc) lst
  | Exception lst -> tree_of_leaves M.empty (fun lc -> L_Exception lc) lst
  | Other _ -> M.empty

let add (tbl:t) (name:string) (branch:t_branch) : unit =
  try 
    let branch2 = Hashtbl.find tbl name in
    Hashtbl.replace tbl name (merge_branches branch branch2)
  with
    Not_found -> Hashtbl.add tbl name branch

let add_decl (tbl:t) (d:t_decl) : unit =
  let tree = decl_to_tree d in
  M.iter (add tbl) tree

(* Querying the table *)

let map_find x map =
  try Some (M.find x map)
  with Not_found -> None

let get_loc : t_leaf -> loc = function
  | L_Subprog l | L_Type l | L_Enum l | L_Incomplete_Alias l
  | L_Number l | L_Object l | L_Exception l -> l

let get_loc_list = function
  | Leaf lst -> List.map get_loc lst
  | Alias (l,_) -> [l]
  | Node ((lc,_),_,_) -> [lc]

let rec get_all_suffixes : path -> path list = function
  | [] -> []
  | (_::tl) as lst -> lst :: (get_all_suffixes tl)

type t_branch_with_ancestors = { bname:string; branch:t_branch; ancestors:(string*t_tree) list }
type t_tree_with_ancestors = { pname:string; tree:t_tree; ancestors:(string*t_tree) list }

let rec mfind (tbl:t) (f_alias:bool) (pkg:t_tree_with_ancestors) (path:path) : t_branch_with_ancestors option =
  Print.debug "mfind '%s' in package '%s'." (String.concat "." path) pkg.pname;
  match path with
  | [] -> assert false
  | [x] ->
    begin match map_find x pkg.tree with
      | Some (Alias (_,path)) when f_alias ->
        resolve_alias tbl path ((pkg.pname,pkg.tree)::pkg.ancestors)
      | Some br -> Some { bname=(pkg.pname^"."^x); branch=br; ancestors=((pkg.pname,pkg.tree)::pkg.ancestors) }
      | None -> None
    end
  | x::lst ->
    begin match map_find x pkg.tree with
      | None -> (Print.debug "'%s' not found in '%s'." x pkg.pname; None)
      | Some (Leaf _) -> (Print.debug "'%s' found in '%s' but not a package." x pkg.pname; None)
      | Some (Alias (_,path)) ->
        begin match resolve_pkg_alias tbl path ((pkg.pname,pkg.tree)::pkg.ancestors) with
          | None -> None
          | Some pkg -> mfind tbl f_alias pkg lst
        end
      | Some (Node (_,_,tree)) ->
        mfind tbl f_alias { pname=(pkg.pname^"."^x); tree;
                            ancestors=(pkg.pname,pkg.tree)::pkg.ancestors; } lst
    end

and resolve_alias (tbl:t) (path:path) : (string*t_tree) list -> t_branch_with_ancestors option = function
  | [] -> hfind tbl true path
  | (pname,tree)::ancestors ->
    begin match mfind tbl true { pname; tree; ancestors } path with
      | None -> resolve_alias tbl path ancestors
      | (Some br) as opt ->
        begin match br.branch with
          | Alias (_,path) -> resolve_alias tbl path br.ancestors
          | _ -> opt
        end
    end

and resolve_pkg_alias (tbl:t) (path:path) (parents:(string*t_tree) list) : t_tree_with_ancestors option =
  match resolve_alias tbl path parents with
  | None -> None
  | Some br ->
    begin match br.branch with
      | Node (_,_,tree) -> Some { pname=br.bname; tree; ancestors=br.ancestors }
      | _ -> None
    end

and hfind (tbl:t) (f_alias:bool) (lst:path) : t_branch_with_ancestors option =
  Print.debug "hfind '%s'." (String.concat "." lst);
  match lst with
  | [] -> None
  | [n] ->
    begin
      try
        begin match Hashtbl.find tbl n with
          | Alias (_,path) when f_alias -> resolve_alias tbl path []
          | branch -> Some { bname=n; branch; ancestors=[] }
        end
      with Not_found -> None
    end
  | pname::lst ->
    begin
      try ( match Hashtbl.find tbl pname with
          | Leaf _ -> None
          | Alias (_,path) -> hfind tbl f_alias (path@lst)
          | Node (_,_,tree) -> mfind tbl f_alias { pname; tree; ancestors=[] } lst )
      with Not_found -> None
    end

and locate (tbl:t) (lst:path) : loc list =
  match hfind tbl false lst with
  | None -> []
  | Some br -> get_loc_list br.branch

let remove_last (lst:string list) : (string list*string) option =
  let rec aux : string list -> string list*string = function
    | [] -> assert false
    | [x] -> ([],x)
    | hd::tl ->
      let (ll,x) = aux tl in (hd::ll,x)
  in
  match lst with
  | [] -> None
  | _ -> Some (aux lst)

let search_in_branch pkg_name (prefix:string) : t_branch -> string list = function
  | Leaf lst -> []
  | Node (_,_,pkg) ->
    let regexp = Str.regexp ("^" ^ prefix) in
    let aux str _ accu =
      if Str.string_match regexp str 0 then (pkg_name ^ "." ^ str)::accu
      else accu
    in
    M.fold aux pkg []
  | Alias _ -> []

let search (tbl:t) (lst:path) : string list =
  match remove_last lst with
  | None -> []
  | Some ([],pre) ->
    begin
      let regexp = Str.regexp ("^" ^ pre) in
      let aux str _ accu =
        if Str.string_match regexp str 0 then str::accu
        else accu
      in
      Hashtbl.fold aux tbl []
    end
  | Some (pkg,pre) ->
    begin match hfind tbl true pkg with
      | None -> ( Print.debug "[Search] Package '%s' not found." (String.concat "." pkg); [] )
      | Some br -> search_in_branch (String.concat "." pkg) pre br.branch 
    end

let print (tbl:t) (lst:path) : unit =
  match hfind tbl false lst with
  | None -> Printf.fprintf stdout "No package or object found.\n"
  | Some br -> print_branch 0 stdout br.branch

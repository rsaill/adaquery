open Common

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

type t_tree =
  | Leaf of t_leaf list (* several objects may have the same name. Ex: to function working on different types *)
  | Alias of loc*path
  | Node of (loc*bool)*bool*t_tree M.t

type toplevel_tree = (string,t_tree) Hashtbl.t (* t is the top level tree *)

type named_tree = path * t_tree

(* Print *)

let its indent = String.make indent ' '

let rec print_tree (ident:int) out : t_tree -> unit = function
  | Leaf [l] -> Printf.fprintf out "%s\n" (leaf_to_string l)
  | Leaf [] -> assert false
  | Leaf (l::_) -> Printf.fprintf out "%s and other definitions\n" (leaf_to_string l)
  | Node (_,_,tree) -> print_tree_map ident out tree
  | Alias (_,path) -> Printf.fprintf out "Alias of %s\n" (String.concat "." path)

and print_tree_map (ident:int) out (tree:t_tree M.t) : unit =
  Printf.fprintf out "Package\n";
  M.iter ( fun key br ->
      Printf.fprintf out "%s%s -> %a" (its ident) key (print_tree (ident+2)) br
    ) tree

let print_named_tree (out:out_channel) (_,tree:named_tree) : unit =
  print_tree 0 out tree

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

(* Read/Write/Create table *)

let write (tbl:toplevel_tree) (fn:string) : unit =
  let out = open_out_bin fn in
  Marshal.to_channel out tbl []

let read (fn:string) : toplevel_tree =
  let dpin = open_in fn in
  Marshal.from_channel dpin

(* Building the table *)

let create () : toplevel_tree = Hashtbl.create 147

(* Merge two trees *)
let rec merge_tree_maps (tr1:t_tree M.t) (tr2:t_tree M.t) : t_tree M.t =
  let aux_opt _ a_opt b_opt =
    match a_opt, b_opt with
    | None, _ -> b_opt
    | _, None -> a_opt
    | Some a, Some b -> Some (merge_trees a b)
  in
  M.merge aux_opt tr1 tr2

(* Merge two branches *)
and merge_trees (a:t_tree) (b:t_tree) : t_tree =
  let merge_loc (lc1:loc*bool) (lc2:loc*bool) =
    (if (snd lc1) && (snd lc2) then
       Print.debug "Two packages have the same name. Merging.");
    if (snd lc1) then lc1 else lc2 in
  match a, b with
  | Leaf lst1, Leaf lst2 -> Leaf (lst1@lst2)
  | Node (lc1,isg1,tr1), Node (lc2,isg2,tr2) ->
    Node (merge_loc lc1 lc2, isg1||isg2, merge_tree_maps tr1 tr2)
  | (Leaf _|Alias _), (Node _ as n) | (Node _ as n), (Leaf _|Alias _) ->
    ( Print.debug "A package has the same name than an object or an alias. Keeping the package."; n)
  | (Alias _ as n), Leaf _ | _, (Alias _ as n)->
    ( Print.debug "An alias has the same name than a package. Keeping the alias."; n)

(* Add a list of leaves to a tree *)
let tree_of_leaves (tree:t_tree M.t) (f:loc -> t_leaf) (lst:ident list) : t_tree M.t =
  List.fold_left (
    fun tree (lc,name) -> merge_tree_maps tree (M.singleton name (Leaf [f lc]))
  ) tree lst 

(* Build a tree from a declaration *)
let rec decl_to_tree : t_decl -> t_tree M.t = function
  | Package (name,lc,is_g,Decl lst) ->
    let aux (tr:t_tree M.t) (d:t_decl) : t_tree M.t =
      merge_tree_maps tr (decl_to_tree d)
    in
    let tree:t_tree M.t = List.fold_left aux M.empty lst in
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

(* Add a branch to a toplevel tree *)
let add (tbl:toplevel_tree) (name:string) (branch:t_tree) : unit =
  try 
    let branch2 = Hashtbl.find tbl name in
    Hashtbl.replace tbl name (merge_trees branch branch2)
  with
    Not_found -> Hashtbl.add tbl name branch

let add_decl (tbl:toplevel_tree) (d:t_decl) : unit =
  let tree = decl_to_tree d in
  M.iter (add tbl) tree

(* Querying the table *)

let map_find (x:string) (map:'a M.t) : 'a option =
  try Some (M.find x map)
  with Not_found -> None

let hash_find (x:'a) (h:('a,'b) Hashtbl.t) : 'b option =
  try Some (Hashtbl.find h x)
  with Not_found -> None

let get_loc : t_leaf -> loc = function
  | L_Subprog l | L_Type l | L_Enum l | L_Incomplete_Alias l
  | L_Number l | L_Object l | L_Exception l -> l

let get_loc_list = function
  | Leaf lst -> List.map get_loc lst
  | Alias (l,_) -> [l]
  | Node ((lc,_),_,_) -> [lc]

let rec get_all_prefixes : path -> path list = function
  | [] -> []
  | x::tl ->
    let lst = get_all_prefixes tl in
    []::(List.map (fun y -> x::y) lst)

let rec try_lst (f:'a -> 'b option) (lst:'a list) : 'b option =
  match lst with
  | [] -> None
  | hd::tl ->
    begin match f hd with
      | Some _ as r -> r
      | None -> try_lst f tl
    end

let rec mfind (tbl:toplevel_tree) (tree_path,tree:named_tree) (path:path) : named_tree option =
  match path with
  | [] -> Some (tree_path,tree)
  | x::lst ->
    begin match tree with
      | Leaf _ -> None
      | Alias (_,alias_path) -> 
        begin match resolve_alias tbl (tree_path@[x]) alias_path with
          | Some tree2 ->
           begin
(*
             Print.debug "mfind( %a | %a ) = mfind( %a | %a )"
               Print.pp_path tree_path Print.pp_path path
               Print.pp_path (fst tree2) Print.pp_path lst;
*)
             mfind tbl tree2 (x::lst)
           end
          | None -> None
        end
      | Node (_,_,m) ->
        begin match map_find x m with
          | Some child_tree ->
            begin
(*
             Print.debug "mfind( %a | %a ) = mfind( %a | %a )"
               Print.pp_path tree_path Print.pp_path path
               Print.pp_path (tree_path@[x]) Print.pp_path lst;
*)
              mfind tbl (tree_path@[x],child_tree) lst
            end
          | None -> None
        end
    end

and resolve_alias (tbl:toplevel_tree) (current_path:path) (alias_path:path) : named_tree option =
  let possible_paths = List.map (fun lst -> lst@alias_path) (get_all_prefixes current_path) in
  try_lst (hfind tbl) possible_paths

and hfind (tbl:toplevel_tree) (path:path) : named_tree option =
  match path with
  | [] -> None
  | pname::lst ->
    begin match hash_find pname tbl with
      | Some tree -> mfind tbl ([pname],tree) lst
      | None -> None
    end

let locate (tbl:toplevel_tree) (path:path) : loc list =
  match hfind tbl path with
  | None -> []
  | Some (_,tree) -> get_loc_list tree

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

let rec complete_from_tree (tbl:toplevel_tree) (pkg_name:string) (prefix:string) : named_tree -> string list = function
  | _, Leaf lst -> []
  | _, Node (_,_,pkg) ->
    let regexp = Str.regexp ("^" ^ prefix) in
    let aux str _ accu =
      if Str.string_match regexp str 0 then (pkg_name ^ "." ^ str)::accu
      else accu
    in
    M.fold aux pkg []
  | current_path, Alias (_,alias_path) ->
    begin match resolve_alias tbl current_path alias_path with
      | None -> []
      | Some ntree -> complete_from_tree tbl pkg_name prefix ntree
    end

let complete (tbl:toplevel_tree) (lst:path) : string list =
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
  | Some (pkg,prefix) ->
    begin match hfind tbl pkg with
      | None -> ( Print.debug "[Search] Package '%s' not found." (String.concat "." pkg); [] )
      | Some tree -> complete_from_tree tbl (String.concat "." pkg) prefix tree 
    end

let print (tbl:toplevel_tree) (path:path) : unit =
  match hfind tbl path with
  | None -> Printf.fprintf stdout "No package or object found.\n"
  | Some ntree -> print_named_tree stdout ntree

open Common

(* Types *)

module CString :
  sig
    type t
    val compare: t -> t -> int
    val from_string: string -> t
(*     val to_string: t -> string *)
    val hash: t -> int
    val equal: t -> t -> bool
  end = struct
  type t = string
  let from_string = String.lowercase_ascii
  let to_string x = x
  let compare = String.compare
  let hash = Hashtbl.hash
  let equal = String.equal
end

module M = Map.Make(CString)
module H = Hashtbl.Make(CString)

type t_leaf =
  | L_Incomplete_Alias
  | L_Subprog
  | L_Type
  | L_Enum
  | L_Number
  | L_Object
  | L_Exception

let leaf_to_string = function
  | L_Incomplete_Alias -> "Incomplete Alias"
  | L_Subprog -> "Subprog"
  | L_Type -> "Type"
  | L_Enum -> "Enum"
  | L_Number -> "Number"
  | L_Object -> "Object"
  | L_Exception -> "Exception"

type t_tree =
  | Leaf of (loc*string*t_leaf) list (* several objects may have the same name. Ex: functions working on different types *)
  | Alias of loc*string*path
  | Node of (loc*bool)*string*bool*t_tree M.t

type toplevel_tree = t_tree H.t (* t is the top level tree *)

type named_tree = path * t_tree

(* Print *)

let its indent = String.make indent ' '

let rec print_tree (ident:int) out : t_tree -> unit = function
  | Leaf [] -> assert false
  | Leaf [(_,name,l)] -> Printf.fprintf out "%s%s -> %s\n" (its ident) name (leaf_to_string l)
  | Leaf ((_,name,l)::_) -> Printf.fprintf out "%s%s -> %s and other definitions\n" (its ident) name (leaf_to_string l)
  | Alias (_,name,path) -> Printf.fprintf out "%s%s -> Alias of %s\n" (its ident) name (String.concat "." path)
  | Node (_,name,_,tree_map) ->
    begin
      Printf.fprintf out "%s%s -> Package\n" (its ident) name;
      M.iter (fun _ br -> print_tree (ident+2) out br) tree_map
    end
  
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

let write (tbl:toplevel_tree) (file_to_index:string list) (opt_check_ext:bool) (fn:string) : unit =
  let out = open_out_bin fn in
  Marshal.to_channel out (tbl,file_to_index,opt_check_ext) []

let read (fn:string) : toplevel_tree * string list * bool =
  let dpin = open_in fn in
  Marshal.from_channel dpin

(* Building the table *)

let create () : toplevel_tree = H.create 147

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
  let merge_loc (lc1,n1:(loc*bool)*string) (lc2,n2:(loc*bool)*string) =
    (if (snd lc1) && (snd lc2) then
       Print.debug "Two packages have the same name. Merging.");
    if (snd lc1) then (lc1,n1) else (lc2,n2) in
  match a, b with
  | Leaf lst1, Leaf lst2 -> Leaf (lst1@lst2)
  | Node (lc1,n1,isg1,tr1), Node (lc2,n2,isg2,tr2) ->
    let (l,n) = merge_loc (lc1,n1) (lc2,n2) in
    Node (l,n,isg1||isg2, merge_tree_maps tr1 tr2)
  | (Leaf _|Alias _), (Node _ as n) | (Node _ as n), (Leaf _|Alias _) ->
    ( Print.debug "A package has the same name as an object or an alias. Keeping the package."; n)
  | (Alias _ as n), Leaf _ | _, (Alias _ as n)->
    ( Print.debug "An alias has the same name as a package. Keeping the alias."; n)

(* Add a list of leaves to a tree *)
let tree_of_leaves (tree:t_tree M.t) (t:t_leaf) (lst:ident list) : t_tree M.t =
  List.fold_left (
    fun tree (lc,name) ->
      merge_tree_maps tree (M.singleton (CString.from_string name) (Leaf [(lc,name,t)]))
  ) tree lst 

(* Build a tree from a declaration *)
let rec decl_to_tree : t_decl -> t_tree M.t = function
  | Package (name,lc,is_g,Decl lst) ->
    let aux (tr:t_tree M.t) (d:t_decl) : t_tree M.t =
      merge_tree_maps tr (decl_to_tree d)
    in
    let tree:t_tree M.t = List.fold_left aux M.empty lst in
    M.singleton (CString.from_string name) (Node (lc,name,is_g,tree))
  | Package (name,(lc,_),_,New tn) ->
    begin match tname_to_path tn with
      | None -> M.singleton (CString.from_string name)
                  (Leaf [(lc,name,L_Incomplete_Alias)])
      | Some path -> M.singleton (CString.from_string name) (Alias (lc,name,path))
    end
  | Package (name,(lc,_),_,Renamed tn) ->
    begin match tname_to_path tn with
      | None -> M.singleton (CString.from_string name)
                  (Leaf [(lc,name,L_Incomplete_Alias)])
      | Some path -> M.singleton (CString.from_string name) (Alias (lc,name,path))
    end
  | Subprog name -> 
    begin match designator_to_string name with
      | Some (lc,name) -> M.singleton (CString.from_string name)
                            (Leaf [(lc,name,L_Subprog)])
      | None -> M.empty
    end
  | Type ((lc,name),opt) ->
    let map = M.singleton (CString.from_string name) (Leaf [(lc,name,L_Type)]) in
    begin match opt with
      | None -> map
      | Some lst ->  tree_of_leaves map L_Enum lst
    end
  | Number lst -> tree_of_leaves M.empty L_Number lst
  | Object lst -> tree_of_leaves M.empty L_Object lst
  | Exception lst -> tree_of_leaves M.empty L_Exception lst
  | Other _ -> M.empty

(* Add a branch to a toplevel tree *)
let add (tbl:toplevel_tree) (name:CString.t) (branch:t_tree) : unit =
  try 
    let branch2 = H.find tbl name in
    H.replace tbl name (merge_trees branch branch2)
  with
    Not_found -> H.add tbl name branch

let add_decl (tbl:toplevel_tree) (d:t_decl) : unit =
  let tree = decl_to_tree d in
  M.iter (add tbl) tree

(* Querying the table *)

let map_find (x:CString.t) (map:'a M.t) : 'a option =
  try Some (M.find x map)
  with Not_found -> None

let hash_find (x:CString.t) (h:'a H.t) : 'b option =
  try Some (H.find h x)
  with Not_found -> None

let get_loc_list = function
  | Leaf lst -> List.map (fun (l,_,_) -> l) lst
  | Alias (l,_,_) -> [l]
  | Node ((lc,_),_,_,_) -> [lc]

let rec get_all_prefixes : path -> path list = function
  | [] -> [[]]
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
      | Alias (_,_,alias_path) -> 
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
      | Node (_,_,_,m) ->
        begin match map_find (CString.from_string x) m with
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
    begin match hash_find (CString.from_string pname) tbl with
      | Some tree -> mfind tbl ([pname],tree) lst
      | None -> None
    end

let hfind_with_scope (tbl:toplevel_tree) (scope:path) (path:path) : named_tree option =
  let possible_paths = List.map (fun lst -> lst@path) (get_all_prefixes scope) in
  try_lst (hfind tbl) possible_paths

let locate (tbl:toplevel_tree) (scope:path) (path:path) : loc list =
  match hfind_with_scope tbl scope path with
  | None -> Print.fail "[Locate] Object '%s' not found." (String.concat "." path)
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

let get_name : t_tree -> string = function
  | Leaf [] -> assert false
  | Leaf ((_,name,_)::_) | Alias (_,name,_) | Node (_,name,_,_) -> name

let rec complete_from_tree (tbl:toplevel_tree) (pkg_name:string) (prefix:string) : named_tree -> (string list) option = function
  | _, Leaf lst -> None
  | _, Node (_,_,_,pkg) ->
    let regexp = Str.regexp_case_fold ("^" ^ prefix) in
    let aux _ tr (accu:string list) : string list =
      let name = get_name tr in
      if Str.string_match regexp name 0 then (pkg_name ^ "." ^ name)::accu
      else accu
    in
    Some (M.fold aux pkg [])
  | current_path, Alias (_,_,alias_path) ->
    begin match resolve_alias tbl current_path alias_path with
      | None -> None
      | Some ntree -> complete_from_tree tbl pkg_name prefix ntree
    end

let complete (tbl:toplevel_tree) (scope:path) (lst:path) : string list =
  match remove_last lst with
  | None -> []
  | Some ([],prefix) ->
    begin
      let regexp = Str.regexp_case_fold ("^" ^ prefix) in
      let aux _ tr accu =
        let name = get_name tr in
        if Str.string_match regexp name 0 then name::accu
        else accu
      in
      H.fold aux tbl [] (*TODO use scope*)
    end
  | Some (pkg,prefix) ->
    begin match hfind_with_scope tbl scope pkg with
      | None -> Print.fail "[Completion] Object '%s' not found." (String.concat "." pkg)
      | Some tree ->
       begin match complete_from_tree tbl (String.concat "." pkg) prefix tree with
         | None -> Print.fail "[Completion] Object '%s' not found." (String.concat "." pkg)
         | Some lst -> lst
       end
    end

let print (tbl:toplevel_tree) (scope:path) (path:path) : unit =
  match hfind_with_scope tbl scope path with
  | None -> Print.fail "[Print] Object '%s' not found." (String.concat "." path)
  | Some ntree -> print_named_tree stdout ntree

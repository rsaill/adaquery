%{
open Common

let mk_pkg (cname:compound_name) (is_generic:bool) (content:t_package) : t_decl =
  let rec aux cname content lc_bool =
  match cname with
  | [] -> assert false
  | [(lc,name)] -> Package (name,(lc,lc_bool),is_generic,content)
  | (lc,name)::cname -> aux cname (Decl [Package (name,(lc,lc_bool),false,content)]) false
  in
  aux cname content true

let set_generic : t_decl -> t_decl = function
  | Package (name,lc,_,content) -> Package (name,lc,true,content)
  | d -> d
%}
/******* A YACC grammar for Ada 9X *********************************/
/* Copyright (C) Intermetrics, Inc. 1994 Cambridge, MA  USA        */
/* Copying permitted if accompanied by this statement.             */
/* Derivative works are permitted if accompanied by this statement.*/
/* This grammar is thought to be correct as of May 1, 1994         */
/* but as usual there is *no warranty* to that effect.             */
/*******************************************************************/

%token <string> IDENT
%token <string> CHAR_STRING
%token CHAR_LIT
%token NUMERIC_LIT

%token LPAR
%token BAR
%token RPAR
%token COLON
%token SEMICOLON
%token COMMA
%token DOT_DOT
%token LT_LT
%token BOX
%token LT
%token LT_EQ
%token EXPON
%token NE
%token GT_GT
%token GE
%token GE_EQ
%token IS_ASSIGNED
%token RIGHT_SHAFT
%token ABORT
%token ABS
%token ABSTRACT
%token ACCEPT
%token ACCESS
%token ALIASED
%token ALL
%token AND
%token ARRAY
%token AT
%token BEGIN
%token BODY
%token CASE
%token CONSTANT
%token DECLARE
%token DELAY
%token DELTA
%token DIGITS
%token DO
%token ELSE
%token ELSIF
%token END
%token ENTRY
%token EXCEPTION
%token EXIT
%token FOR
%token FUNCTION
%token GENERIC
%token GOTO
%token IF
%token IN
%token IS
%token LIMITED
%token LOOP
%token MOD
%token NEW
%token NOT
%token NULL
%token OF
%token OR
%token OTHERS
%token OUT
%token PACKAGE
%token PRAGMA
%token PRIVATE
%token PROCEDURE
%token PROTECTED
%token RAISE
%token RANGE
%token RECORD
%token REM
%token RENAMES
%token REQUEUE
%token RETURN
%token REVERSE
%token SELECT
%token SEPARATE
%token SUBTYPE
%token TAGGED
%token TASK
%token TERMINATE
%token THEN
%token TYPE
%token UNTIL
%token USE
%token WHEN
%token WHILE
%token WITH
%token XOR
%token PLUS
%token STAR
%token MINUS
%token SLASH
%token EQUAL
%token AMPERSAND
%token DOT
%token TIC
%token EOF

%start goal_symbol
%type <Common.t_decl list> goal_symbol
%type <Common.t_decl> unit
%type <Common.t_decl> decl

%%

goal_symbol : pragma_s lst=compilation EOF { lst }

pragma  : PRAGMA IDENT SEMICOLON { () }
	| PRAGMA simple_name LPAR pragma_arg_s RPAR SEMICOLON { () }

pragma_arg_s : pragma_arg { () }
	| pragma_arg_s COMMA pragma_arg { () }

pragma_arg : expression  { () }
	| simple_name RIGHT_SHAFT expression { () }

pragma_s : { () }
	| pragma_s pragma { () }

decl    : d=object_decl { d }
	| lst=number_decl { Number lst }
	| d=type_decl { d }
	| d=subtype_decl { d }
	| d=subprog_decl { Subprog d }
	| d=pkg_decl { d }
	| task_decl { Other Task }
	| prot_decl { Other Prot }
	| lst=exception_decl { Exception lst }
	| r=rename_decl { r }
	| g=generic_decl { g }
	| body_stub { Other Body_stub }

object_decl : lst=def_id_s COLON object_qualifier_opt object_subtype_def init_opt SEMICOLON { Object lst }

def_id_s : id=def_id { [id] }
	| lst=def_id_s COMMA id=def_id { id::lst }

def_id  : id=IDENT { ($startpos,id) }

object_qualifier_opt : { () }
	| ALIASED { () }
	| CONSTANT { () }
	| ACCESS { () }
	| ALIASED CONSTANT { () }

object_subtype_def : subtype_ind { () }
	| array_type { () }

init_opt : { () }
	| IS_ASSIGNED expression { () }

number_decl : lst=def_id_s COLON CONSTANT IS_ASSIGNED expression SEMICOLON { lst }

type_decl : TYPE id=IDENT discrim_part_opt def=type_completion SEMICOLON { Type (($startpos(id),id),def) }

discrim_part_opt : { () }
	| discrim_part { () }
	| LPAR BOX RPAR { () }

type_completion : { None }
	| IS def=type_def { def }

type_def : e=enumeration_type  { Some e }
	| integer_type { None }
	| real_type { None }
	| array_type { None }
	| record_type { None }
	| access_type { None }
	| derived_type { None }
	| private_type { None }

subtype_decl : SUBTYPE id=IDENT IS subtype_ind SEMICOLON { Type (($startpos(id),id),None) }

subtype_ind : name adaconstraint { () }
	| name { () }

adaconstraint : range_constraint { () }
	| decimal_digits_constraint { () }

decimal_digits_constraint : DIGITS expression range_constr_opt { () }

derived_type : NEW subtype_ind { () }
	| NEW subtype_ind WITH PRIVATE { () }
	| NEW subtype_ind WITH record_def  { () }
	| ABSTRACT NEW subtype_ind WITH PRIVATE { () }
	| ABSTRACT NEW subtype_ind WITH record_def { () }
	;

range_constraint : RANGE range { () }

range : simple_expression DOT_DOT simple_expression { () }
	| name TIC RANGE { () }
	| name TIC RANGE LPAR expression RPAR { () }

enumeration_type : LPAR lst=enum_id_s RPAR { lst }

enum_id_s : id=enum_id { [id] }
	| lst=enum_id_s COMMA id=enum_id { id::lst }

enum_id : id=IDENT { ($startpos(id),id) }
	| CHAR_LIT { ($startpos,"!") }

integer_type : range_spec { () }
	| MOD expression { () }

range_spec : range_constraint { () }

range_spec_opt : { () }
	| range_spec { () }

real_type : float_type { () }
	| fixed_type { () }

float_type : DIGITS expression range_spec_opt { () }

fixed_type : DELTA expression range_spec { () }
	| DELTA expression DIGITS expression range_spec_opt { () }

array_type : unconstr_array_type { () }
	| constr_array_type { () }

unconstr_array_type : ARRAY LPAR index_s RPAR OF component_subtype_def { () }

constr_array_type : ARRAY iter_index_constraint OF component_subtype_def { () }

component_subtype_def : aliased_opt subtype_ind { () }

aliased_opt :  { () }
	| ALIASED { () }

index_s : index { () }
	| index_s COMMA index { () }

index : name RANGE BOX { () }

iter_index_constraint : LPAR iter_discrete_range_s RPAR { () }

iter_discrete_range_s : discrete_range { () }
	| iter_discrete_range_s COMMA discrete_range { () }

discrete_range : name range_constr_opt { () }
	| range { () }

range_constr_opt : { () }
	| range_constraint { () }

record_type : tagged_opt limited_opt record_def { () }

record_def : RECORD pragma_s comp_list END RECORD { () }
	| NULL RECORD { () }

tagged_opt : { () }
	| TAGGED { () }
	| ABSTRACT TAGGED { () }

comp_list : comp_decl_s variant_part_opt { () }
	| variant_part pragma_s { () }
	| NULL SEMICOLON pragma_s { () }

comp_decl_s : comp_decl { () }
	| comp_decl_s pragma_s comp_decl { () }

variant_part_opt : pragma_s { () }
	| pragma_s variant_part pragma_s { () }

comp_decl : def_id_s COLON component_subtype_def init_opt SEMICOLON { () }

discrim_part : LPAR discrim_spec_s RPAR { () }

discrim_spec_s : discrim_spec { () }
	| discrim_spec_s SEMICOLON discrim_spec { () }

discrim_spec : def_id_s COLON access_opt mark init_opt { () }

access_opt : { () }
	| ACCESS { () }

variant_part : CASE simple_name IS pragma_s variant_s END CASE SEMICOLON { () }

variant_s : variant { () }
	| variant_s variant { () }

variant : WHEN choice_s RIGHT_SHAFT pragma_s comp_list { () }

choice_s : choice { () }
	| choice_s BAR choice { () }

choice : expression { () }
	| discrete_with_range { () }
	| OTHERS { () }

discrete_with_range : name range_constraint { () }
	| range { () }

access_type : ACCESS subtype_ind { () }
	| ACCESS CONSTANT subtype_ind { () }
	| ACCESS ALL subtype_ind { () }
	| ACCESS prot_opt PROCEDURE formal_part_opt { () }
	| ACCESS prot_opt FUNCTION formal_part_opt RETURN mark { () }

prot_opt : { () }
	| PROTECTED { () }

decl_part : { [] }
	| lst=decl_item_or_body_s1 { lst }

decl_item_s :  { [] }
	| lst=decl_item_s1 { lst }

decl_item_s1 : d=decl_item { match d with Some d -> [d] | None -> []}
	| lst=decl_item_s1 d=decl_item { match d with Some d -> d::lst | None -> lst }

decl_item : d=decl { Some d }
	| use_clause { None }
	| rep_spec { None }
	| pragma { None }

decl_item_or_body_s1 : d=decl_item_or_body { match d with None -> [] | Some d -> [d] }
	| lst=decl_item_or_body_s1 d=decl_item_or_body { match d with None -> lst | Some d -> d::lst }

decl_item_or_body : b=body { Some b }
        | d=decl_item { d }

body : s=subprog_body { s }
	| s=pkg_body { s }
	| task_body { Other Task }
	| prot_body { Other Prot }

name : id=simple_name { Simple_name id }
	| id=indexed_comp { id }
	| id=selected_comp { id }
	| attribute { No_name }
	| operator_symbol { No_name }

mark : simple_name { () }
	| mark TIC attribute_id { () }
	| mark DOT simple_name { () }

simple_name : id=IDENT { ($startpos(id),id) }

compound_name : id=simple_name { [id] }
	| lst=compound_name DOT id=simple_name { id::lst }

c_name_list : compound_name { () }
	 | c_name_list COMMA compound_name { () }

used_char : CHAR_LIT { () }

operator_symbol : CHAR_STRING { () }

indexed_comp : n=name LPAR value_s RPAR { n }

value_s : value { () }
	| value_s COMMA value { () }

value : expression { () }
	| comp_assoc { () }
	| discrete_with_range { () }

selected_comp : n=name DOT s=simple_name { Selected_comp (n,s) }
	| name DOT used_char { No_name }
	| name DOT operator_symbol { No_name }
	| name DOT ALL { No_name }

attribute : name TIC attribute_id { () }

attribute_id : IDENT { () }
	| DIGITS { () }
	| DELTA { () }
	| ACCESS { () }

literal : NUMERIC_LIT { () }
	| used_char { () }
	| NULL { () }

aggregate : LPAR comp_assoc RPAR { () }
	| LPAR value_s_2 RPAR { () }
	| LPAR expression WITH value_s RPAR { () }
	| LPAR expression WITH NULL RECORD RPAR { () }
	| LPAR NULL RECORD RPAR { () }
	;

value_s_2 : value COMMA value { () }
	| value_s_2 COMMA value { () }

comp_assoc : choice_s RIGHT_SHAFT expression { () }
| choice_s RIGHT_SHAFT BOX { () }

expression : relation { () }
	| expression logical relation { () }
	| expression short_circuit relation { () }

logical : AND { () }
	| OR { () }
	| XOR { () }

short_circuit : AND THEN { () }
	| OR ELSE { () }

relation : simple_expression { () }
	| simple_expression relational simple_expression { () }
	| simple_expression membership range { () }
	| simple_expression membership name { () }

relational : EQUAL { () }
	| NE { () }
	| LT { () }
	| LT_EQ { () }
	| GE { () }
	| GE_EQ { () }

membership : IN { () }
	| NOT IN { () }

simple_expression : unary term { () }
	| term { () }
	| simple_expression adding term { () }

unary   : PLUS { () }
	| MINUS { () }

adding  : PLUS { () }
	| MINUS { () }
	| AMPERSAND { () }

term    : factor { () }
	| term multiplying factor { () }

multiplying : STAR { () }
	| SLASH { () }
	| MOD { () }
	| REM { () }

factor : primary { () }
	| NOT primary { () }
	| ABS primary { () }
	| primary EXPON primary { () }

primary : literal { () }
	| name { () }
	| allocator { () }
	| qualified { () }
	| parenthesized_primary { () }

parenthesized_primary : aggregate { () }
	| LPAR expression RPAR { () }

qualified : name TIC parenthesized_primary { () }

allocator : NEW name { () }
	| NEW qualified { () }

statement_s : statement { () }
	| statement_s statement { () }

statement : unlabeled { () }
	| label statement { () }

unlabeled : simple_stmt { () }
	| compound_stmt { () }
	| pragma { () }

simple_stmt : null_stmt { () }
	| assign_stmt { () }
	| exit_stmt { () }
	| return_stmt { () }
	| goto_stmt { () }
	| procedure_call { () }
	| delay_stmt { () }
	| abort_stmt { () }
	| raise_stmt { () }
	| code_stmt { () }
	| requeue_stmt { () }

compound_stmt : if_stmt { () }
	| case_stmt { () }
	| loop_stmt { () }
	| block { () }
	| accept_stmt { () }
	| select_stmt { () }

label : LT_LT IDENT GT_GT { () }

null_stmt : NULL SEMICOLON { () }

assign_stmt : name IS_ASSIGNED expression SEMICOLON { () }

if_stmt : IF cond_clause_s else_opt END IF SEMICOLON { () }

cond_clause_s : cond_clause { () }
	| cond_clause_s ELSIF cond_clause { () }

cond_clause : cond_part statement_s { () }

cond_part : condition THEN { () }

condition : expression { () }

else_opt : { () }
	| ELSE statement_s { () }

case_stmt : case_hdr pragma_s alternative_s END CASE SEMICOLON { () }

case_hdr : CASE expression IS { () }

alternative_s : { () }
	| alternative_s alternative { () }

alternative : WHEN choice_s RIGHT_SHAFT statement_s { () }

loop_stmt : label_opt iteration basic_loop id_opt SEMICOLON { () }

label_opt : { () }
	| IDENT COLON { () }

iteration : { () }
	| WHILE condition { () }
	| iter_part reverse_opt discrete_range { () }

iter_part : FOR IDENT IN { () }

reverse_opt : { () }
	| REVERSE { () }

basic_loop : LOOP statement_s END LOOP { () }

id_opt : { () }
	| designator { () }

block : label_opt block_decl block_body END id_opt SEMICOLON { () }

block_decl : { () }
	| DECLARE decl_part { () }

block_body : BEGIN handled_stmt_s { () }

handled_stmt_s : statement_s except_handler_part_opt  { () }

except_handler_part_opt : { () }
	| except_handler_part { () }

exit_stmt : EXIT name_opt when_opt SEMICOLON { () }

name_opt : { () }
	| name { () }

when_opt : { () }
	| WHEN condition { () }

return_stmt : RETURN SEMICOLON { () }
	| RETURN expression SEMICOLON { () }

goto_stmt : GOTO name SEMICOLON { () }

subprog_decl : name=subprog_spec SEMICOLON { name }
	| name=generic_subp_inst SEMICOLON { name }
	| name=subprog_spec_is_push ABSTRACT SEMICOLON { name }

subprog_spec : PROCEDURE name=compound_name formal_part_opt { Compound_Name name }
	| FUNCTION name=designator formal_part_opt RETURN name { name }
	| FUNCTION name=designator  /* for generic inst and generic rename */ { name }

designator : name=compound_name { Compound_Name name }
	| str=CHAR_STRING { String_Name ($startpos,str) }

formal_part_opt :  { () }
	| formal_part { () }

formal_part : LPAR param_s RPAR { () }

param_s : param { () }
	| param_s SEMICOLON param { () }

param : def_id_s COLON mode mark init_opt { () }

mode : { () }
	| IN { () }
	| OUT { () }
	| IN OUT { () }
	| ACCESS { () }

subprog_spec_is_push : name=subprog_spec IS { name }

subprog_body : s=subprog_spec_is_push
	       decl_part block_body END id_opt SEMICOLON { Subprog s }

procedure_call : name SEMICOLON { () }

pkg_decl : p=pkg_spec SEMICOLON { p }
	| p=generic_pkg_inst SEMICOLON { p }

pkg_spec : PACKAGE name=compound_name IS
	     decls=decl_item_s private_part END c_id_opt { mk_pkg name false (Decl decls) }

private_part : { () }
	| PRIVATE decl_item_s { () }

c_id_opt :  { () }
	| compound_name { () }

pkg_body : PACKAGE BODY name=compound_name IS
	       lst=decl_part body_opt END c_id_opt SEMICOLON { mk_pkg name false (Decl lst) }

body_opt : { () }
	| block_body { () }

private_type : tagged_opt limited_opt PRIVATE { () }

limited_opt : { () }
	| LIMITED { () }

use_clause : USE name_s SEMICOLON { () }
	| USE TYPE name_s SEMICOLON { () }

name_s : name { () }
	| name_s COMMA name { () }

rename_decl : lst=def_id_s COLON object_qualifier_opt subtype_ind renames SEMICOLON { Object lst }
	| lst=def_id_s COLON EXCEPTION renames SEMICOLON { Exception lst }
	| r=rename_unit { r }

rename_unit : PACKAGE n=compound_name r=renames SEMICOLON { mk_pkg n false (Renamed r) }
	| s=subprog_spec renames SEMICOLON { Subprog s }
	| generic_formal_part PACKAGE n=compound_name r=renames SEMICOLON { mk_pkg n true (Renamed r) }
	| generic_formal_part s=subprog_spec renames SEMICOLON { Subprog s }

renames : RENAMES n=name { n }

task_decl : task_spec SEMICOLON { () }

task_spec : TASK simple_name task_def { () }
	| TASK TYPE simple_name discrim_part_opt task_def { () }

task_def : { () }
	| IS entry_decl_s rep_spec_s task_private_opt END id_opt { () }

task_private_opt : { () }
	| PRIVATE entry_decl_s rep_spec_s { () }

task_body : TASK BODY simple_name IS
	       decl_part block_body END id_opt SEMICOLON { () }

prot_decl : prot_spec SEMICOLON { () }

prot_spec : PROTECTED IDENT prot_def { () }
	| PROTECTED TYPE simple_name discrim_part_opt prot_def { () }

prot_def : IS prot_op_decl_s prot_private_opt END id_opt { () }

prot_private_opt : { () }
	| PRIVATE prot_elem_decl_s  { () }


prot_op_decl_s :  { () }
	| prot_op_decl_s prot_op_decl { () }

prot_op_decl : entry_decl { () }
	| subprog_spec SEMICOLON { () }
	| rep_spec { () }
	| pragma { () }

prot_elem_decl_s :  { () }
	| prot_elem_decl_s prot_elem_decl { () }

prot_elem_decl : prot_op_decl | comp_decl { () }

prot_body : PROTECTED BODY simple_name IS
	       prot_op_body_s END id_opt SEMICOLON { () }

prot_op_body_s : pragma_s { () }
	| prot_op_body_s prot_op_body pragma_s { () }

prot_op_body : entry_body { () }
	| subprog_body { () }
	| subprog_spec SEMICOLON { () }

entry_decl_s : pragma_s { () }
	| entry_decl_s entry_decl pragma_s { () }

entry_decl : ENTRY IDENT formal_part_opt SEMICOLON { () }
	| ENTRY IDENT LPAR discrete_range RPAR formal_part_opt SEMICOLON { () }

entry_body : ENTRY IDENT formal_part_opt WHEN condition entry_body_part { () }
	| ENTRY IDENT LPAR iter_part discrete_range RPAR
		formal_part_opt WHEN condition entry_body_part { () }

entry_body_part : SEMICOLON { () }
	| IS decl_part block_body END id_opt SEMICOLON { () }

rep_spec_s : { () }
	| rep_spec_s rep_spec pragma_s { () }

entry_call : procedure_call { () }

accept_stmt : accept_hdr SEMICOLON { () }
	| accept_hdr DO handled_stmt_s END id_opt SEMICOLON { () }

accept_hdr : ACCEPT entry_name formal_part_opt { () }

entry_name : simple_name { () }
	| entry_name LPAR expression RPAR { () }

delay_stmt : DELAY expression SEMICOLON { () }
	| DELAY UNTIL expression SEMICOLON { () }

select_stmt : select_wait { () }
	| async_select { () }
	| timed_entry_call { () }
	| cond_entry_call { () }

select_wait : SELECT guarded_select_alt or_select else_opt
	      END SELECT SEMICOLON { () }

guarded_select_alt : select_alt { () }
	| WHEN condition RIGHT_SHAFT select_alt { () }

or_select : { () }
	| or_select OR guarded_select_alt { () }

select_alt : accept_stmt stmts_opt { () }
	| delay_stmt stmts_opt { () }
	| TERMINATE SEMICOLON { () }

delay_or_entry_alt : delay_stmt stmts_opt { () }
	| entry_call stmts_opt { () }

async_select : SELECT delay_or_entry_alt
	       THEN ABORT statement_s
	       END SELECT SEMICOLON { () }

timed_entry_call : SELECT entry_call stmts_opt
		   OR delay_stmt stmts_opt
	           END SELECT SEMICOLON { () }

cond_entry_call : SELECT entry_call stmts_opt
		  ELSE statement_s
	          END SELECT SEMICOLON { () }

stmts_opt : { () }
	| statement_s { () }

abort_stmt : ABORT name_s SEMICOLON { () }

compilation : { [] }
	| lst=compilation c=comp_unit { c::lst }
        (*| pragma pragma_s { [] }*)

comp_unit : context_spec private_opt u=unit pragma_s { u }
	| private_opt u=unit pragma_s { u }

private_opt : { () }
	| PRIVATE { () }

context_spec : with_clause { () }
        | use_clause { () }
	| context_spec with_clause { () }
	| context_spec use_clause { () }
	| context_spec pragma { () }

with_clause : WITH c_name_list SEMICOLON { () }

unit : a=pkg_decl { a }
	| a=pkg_body { a }
	| a=subprog_decl { Subprog a }
	| a=subprog_body { a }
	| a=subunit { a }
	| a=generic_decl { a }
	| a=rename_unit { a }

subunit : SEPARATE LPAR compound_name RPAR
	      subunit_body { Other Subunit }

subunit_body : subprog_body { () }
	| pkg_body { () }
	| task_body { () }
	| prot_body { () }

body_stub : TASK BODY simple_name IS SEPARATE SEMICOLON { () }
	| PACKAGE BODY compound_name IS SEPARATE SEMICOLON { () }
	| subprog_spec IS SEPARATE SEMICOLON { () }
	| PROTECTED BODY simple_name IS SEPARATE SEMICOLON { () }

exception_decl : lst=def_id_s COLON EXCEPTION SEMICOLON { lst }

except_handler_part : EXCEPTION exception_handler { () }
	| except_handler_part exception_handler { () }

exception_handler : WHEN except_choice_s RIGHT_SHAFT statement_s { () }
	| WHEN IDENT COLON except_choice_s RIGHT_SHAFT statement_s { () }

except_choice_s : except_choice { () }
	| except_choice_s BAR except_choice { () }

except_choice : name { () }
	| OTHERS { () }

raise_stmt : RAISE name_opt SEMICOLON { () }

requeue_stmt : REQUEUE name SEMICOLON { () }
	| REQUEUE name WITH ABORT SEMICOLON { () }

generic_decl : generic_formal_part s=subprog_spec SEMICOLON { Subprog s }
	| generic_formal_part p=pkg_spec SEMICOLON { set_generic p }

generic_formal_part : GENERIC { () }
	| generic_formal_part generic_formal { () }

generic_formal : param SEMICOLON { () }
	| TYPE simple_name generic_discrim_part_opt IS generic_type_def SEMICOLON { () }
	| WITH PROCEDURE simple_name
	    formal_part_opt subp_default SEMICOLON { () }
	| WITH FUNCTION designator
	    formal_part_opt RETURN name subp_default SEMICOLON { () }
	| WITH PACKAGE simple_name IS NEW name LPAR BOX RPAR SEMICOLON { () }
	| WITH PACKAGE simple_name IS NEW name SEMICOLON { () }
	| use_clause { () }

generic_discrim_part_opt : { () }
	| discrim_part { () }
	| LPAR BOX RPAR { () }

subp_default : { () }
	| IS name { () }
	| IS BOX { () }

generic_type_def : LPAR BOX RPAR { () }
	| RANGE BOX { () }
	| MOD BOX { () }
	| DELTA BOX { () }
	| DELTA BOX DIGITS BOX { () }
	| DIGITS BOX { () }
	| array_type { () }
	| access_type { () }
	| private_type { () }
	| generic_derived_type { () }

generic_derived_type : NEW subtype_ind { () }
	| NEW subtype_ind WITH PRIVATE { () }
	| ABSTRACT NEW subtype_ind WITH PRIVATE { () }

generic_subp_inst : name=subprog_spec IS generic_inst { name }

generic_pkg_inst : PACKAGE name=compound_name IS g=generic_inst { mk_pkg name false (New g) }

generic_inst : NEW n=name { n }

rep_spec : attrib_def { () }
	| record_type_spec { () }
	| address_spec { () }

attrib_def : FOR mark USE expression SEMICOLON { () }

record_type_spec : FOR mark USE RECORD align_opt comp_loc_s END RECORD SEMICOLON { () }

align_opt : { () }
	| AT MOD expression SEMICOLON { () }

comp_loc_s : { () }
	| comp_loc_s mark AT expression RANGE range SEMICOLON { () }

address_spec : FOR mark USE AT expression SEMICOLON { () }

code_stmt : qualified SEMICOLON { () }

%%



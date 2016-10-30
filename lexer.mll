{
open Lexing
open Parser

exception Error of position * string

let keywords = Hashtbl.create 57

let _ = List.iter (fun (name, keyword) ->
    Hashtbl.add keywords name keyword) [
    "ABORT",        ABORT;
    "ABS",          ABS;
    "ABSTRACT",     ABSTRACT;
    "ACCEPT",       ACCEPT;
    "ACCESS",       ACCESS;
    "ALIASED",      ALIASED;
    "ALL",          ALL;
    "AND",          AND;
    "ARRAY",        ARRAY;
    "AT",           AT;
    "BEGIN",        BEGIN;
    "BODY",         BODY;
    "CASE",         CASE;
    "CONSTANT",     CONSTANT;
    "DECLARE",      DECLARE;
    "DELAY",        DELAY;
    "DELTA",        DELTA;
    "DIGITS",       DIGITS;
    "DO",           DO;
    "ELSE",         ELSE;
    "ELSIF",        ELSIF;
    "END",          END;
    "ENTRY",        ENTRY;
    "EXCEPTION",    EXCEPTION;
    "EXIT",         EXIT;
    "FOR",          FOR;
    "FUNCTION",     FUNCTION;
    "GENERIC",      GENERIC;
    "GOTO",         GOTO;
    "IF",           IF;
    "IN",           IN;
    "IS",           IS;
    "LIMITED",      LIMITED;
    "LOOP",         LOOP;
    "MOD",          MOD;
    "NEW",          NEW;
    "NOT",          NOT;
    "NULL",         NULL;
    "OF",           OF;
    "OR",           OR;
    "OTHERS",       OTHERS;
    "OUT",          OUT;
    "PACKAGE",      PACKAGE;
    "PRAGMA",       PRAGMA;
    "PRIVATE",      PRIVATE;
    "PROCEDURE",    PROCEDURE;
    "PROTECTED",    PROTECTED;
    "RAISE",        RAISE;
    "RANGE",        RANGE;
    "RECORD",       RECORD;
    "REM",          REM;
    "RENAMES",      RENAMES;
    "REQUEUE",      REQUEUE;
    "RETURN",       RETURN;
    "REVERSE",      REVERSE;
    "SELECT",       SELECT;
    "SEPARATE",     SEPARATE;
    "SUBTYPE",      SUBTYPE;
    "TAGGED",       TAGGED;
    "TASK",         TASK;
    "TERMINATE",    TERMINATE;
    "THEN",         THEN;
    "TYPE",         TYPE;
    "UNTIL",        UNTIL;
    "USE",          USE;
    "WHEN",         WHEN;
    "WHILE",        WHILE;
    "WITH",         WITH;
    "XOR",          XOR;
  ]

let ident_to_token id =
  try Hashtbl.find keywords (String.uppercase_ascii id)
  with Not_found -> IDENT id
}

let space   = [' ' '\t' '\r']
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let identifier = letter ( ( letter | digit | '_') )*
let character = '\'' _ '\''

let numeral = digit ('_'? digit)*
let exponent = 'E' '+'? numeral | 'E' '-' numeral
let decimal_literal = numeral ('.' numeral)? exponent?

let base = numeral
let extended_digit = digit | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
let based_numeral = extended_digit ('_'? extended_digit)*
let based_literal = base '#' based_numeral ('.' based_numeral)? '#' exponent?

let number = decimal_literal | based_literal

rule token = parse
  | space       { token lexbuf  }
  | '\n'        { new_line lexbuf ; token lexbuf }
  | "--"        { comment lexbuf}
  | '|'         { BAR   }
  | '.'         { DOT   }
  | ','         { COMMA }
  | ':'         { COLON }
  | '('         { LPAR  }
  | ')'         { RPAR  }
  | '&'         { AMPERSAND }
  | '''         { TIC }
  | '*'         { STAR }
  | '+'         { PLUS }
  | '-'         { MINUS }
  | '/'         { SLASH }
  | ';'         { SEMICOLON }
  | '<'         { LT }
  | '='         { EQUAL }
  | '>'         { GE }
  | "=>"        { RIGHT_SHAFT }
  | ".."        { DOT_DOT }
  | "**"        { EXPON }
  | ":="        { IS_ASSIGNED }
  | "/="        { NE }
  | ">="        { GE_EQ }
  | "<="        { LT_EQ }
  | "<<"        { LT_LT }
  | ">>"        { GT_GT }
  | "<>"        { BOX }
  | identifier as id { ident_to_token id  }
  | number { NUMERIC_LIT  }
  | character { CHAR_LIT  }
  | '"' { string (Buffer.create 42) lexbuf }
  | '#' { preproc lexbuf }
  | _   as c    { raise (Error (lexbuf.Lexing.lex_start_p, "unexpected character '" ^ String.make 1 c ^ "'.")) }
  | eof { EOF }

 and comment = parse
  | '\n' { new_line lexbuf ; token lexbuf }
  | _    { comment lexbuf        }
  | eof	 { raise (Error (lexbuf.Lexing.lex_start_p, "unexpected end of file." )) }

and string buf = parse
  | "\"\"" { Buffer.add_char buf '"'; string buf lexbuf }
  | '\n' { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; string buf lexbuf }
  | '"' { CHAR_STRING (Buffer.contents buf) }
  | _ as c
  { Buffer.add_char buf c; string buf lexbuf }
  | eof { raise (Error (lexbuf.Lexing.lex_start_p, "unexpected end of file." )) }

and preproc = parse
  | '\n' { new_line lexbuf ; token lexbuf }
  | _    { preproc lexbuf        }
  | eof { raise (Error (lexbuf.Lexing.lex_start_p, "unexpected end of file." )) }

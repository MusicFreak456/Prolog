{ 
  open Menhir_parser

  let symbols = [  
    '(' , L_PARENTHESIS ;
    ')' , R_PARENTHESIS ;
    '.' , PERIOD        ;
    ',' , COMMA         ;
    '[' , L_BRACKET     ;
    ']' , R_BRACKET     ;
    '|' , PIPE
  ]

  let symbol_table = Hashtbl.create (List.length symbols)
  let _ = List.iter 
    (fun (symbol, token) -> Hashtbl.add symbol_table symbol token)
    symbols

  let symbol_lookup lexbuf =
    let matched_string = Lexing.lexeme lexbuf in
    let symbol = String.get matched_string 0 in
    Hashtbl.find symbol_table symbol
}

let upper_case_letter = ['A' - 'Z']
let lower_case_letter = ['a' - 'z']
let letter = upper_case_letter | lower_case_letter
let digit = ['0' - '9']
let alphanumeric = upper_case_letter | lower_case_letter | digit

let filepath = (alphanumeric | '_' | '-' | '/' | '.')+ '.' letter+

let whitespace = [' ' '\t']+
let newline = '\n' | "\r\n"
let symbol = ['(' ')' '.' ',' '[' ']' '|']

let atom     = lower_case_letter (alphanumeric | ['_'])*
let variable = upper_case_letter (alphanumeric | ['_'])*

rule read =
  parse
  | whitespace { read lexbuf }
  | newline    { Lexing.new_line lexbuf; read lexbuf }
  | symbol     { symbol_lookup lexbuf }
  | ":-"       { RULE }
  | "?-"       { DB_QUERY }
  | '%'        { read_comment_line lexbuf }
  | "/*"       { read_comment_block lexbuf }
  | variable   { VARIABLE (Lexing.lexeme lexbuf) }
  | atom       { ATOM (Lexing.lexeme lexbuf) }
  | filepath   { FILEPATH (Lexing.lexeme lexbuf) }
  | eof        { EOF }
  | _ { 
    let curr_char = Lexing.lexeme lexbuf in
    let position = Lexing.lexeme_start_p lexbuf in
    Error.throw 
      (Error.ParsingError(position, "Unexpected character: " ^ curr_char)) }

and read_comment_line =
  parse
  | '\n'       { Lexing.new_line lexbuf; read lexbuf }
  | eof        { EOF }
  | _          { read_comment_line lexbuf }

and read_comment_block =
  parse
  | '\n'       { Lexing.new_line lexbuf; read_comment_block lexbuf }
  | "*/"       { read lexbuf }
  | eof        { 
    let position = Lexing.lexeme_start_p lexbuf in
    Error.throw (Error.ParsingError(position, "Unclosed comment block")) }
  | _          { read_comment_block lexbuf }

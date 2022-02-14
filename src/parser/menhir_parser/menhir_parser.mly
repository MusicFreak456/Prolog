%{
  open Syntax

  let desugar_atom funct = Term.Symbol(funct, [])

  let desugar_fact predicate = (predicate, [])

  let desugar_list term_list =
    List.fold_right (fun term acc ->
      Term.Symbol("cons", [term; acc])
    ) term_list (desugar_atom "nil")

  let desugar_cons car cdr = Term.Symbol("cons", [car; cdr])
%}

%token <string> ATOM
%token <string> VARIABLE
%token <string> FILEPATH

%token RULE     /* :- */
%token DB_QUERY /* ?- */

%token L_PARENTHESIS
%token R_PARENTHESIS
%token L_BRACKET
%token R_BRACKET
%token PIPE
%token PERIOD
%token COMMA
%token EOF

%start <Syntax.program> program
%start <Syntax.query>   query
%start <Syntax.clause>  clause

%%

program:
 | list = clause*; EOF { list }
 ;

query:
  | DB_QUERY; preds = predicates; PERIOD { DbQuery preds }
  | L_BRACKET; path = FILEPATH; R_BRACKET; PERIOD { DbOpen path }
  ;

clause:
  | head = predicate; PERIOD                          { desugar_fact head }
  | head = predicate; RULE; body = predicates; PERIOD { (head, body) }
  ;

predicates:
  list = separated_nonempty_list(COMMA, predicate) { list };

predicate:
  | funct = ATOM   { desugar_atom funct }
  | sym   = symbol { sym }
  ;

symbol:
  f = ATOM; L_PARENTHESIS; args = terms; R_PARENTHESIS { Term.Symbol(f, args) };

terms:
  list = separated_list(COMMA, term) { list };

term:
  | L_BRACKET; list = separated_list(COMMA, term); R_BRACKET 
    { desugar_list list }
  | L_BRACKET; car = term; PIPE; cdr = term; R_BRACKET { desugar_cons car cdr }
  | atom = atom    { atom }
  | var = VARIABLE { Term.Variable var }
  | sym = symbol   { sym }
  ;

atom: funct = ATOM { desugar_atom funct };

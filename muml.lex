%name MumlLexer;

%let digit = [0-9];
%let int = {digit}+;
%let letter = [a-zA-Z];
%let id = {letter}({letter} | {digit} | "'" | "_")*;
%let op = ("<" | ">" | "+" | "-" | "^" | "*" | "=")+;

%states CON_STRING;

%defs (
  structure T = MumlTokens
  type lex_result = T.token
  fun eof() = T.EOF
  val stringbuf = ref "";
);

<INITIAL> let => ( T.KW_let );
<INITIAL> in => ( T.KW_in );
<INITIAL> end => ( T.KW_end );
<INITIAL> fn => ( T.KW_fn );
<INITIAL> fun => ( T.KW_fun );
<INITIAL> val => ( T.KW_val );
<INITIAL> if => ( T.KW_if );
<INITIAL> then => ( T.KW_then );
<INITIAL> else => ( T.KW_else );
<INITIAL> andalso => ( T.KW_andalso );
<INITIAL> orelse => ( T.KW_orelse );
<INITIAL> "=>" => ( T.ARROW );
<INITIAL> "->" => ( T.TARROW );
<INITIAL> {id} => ( T.ID yytext );
<INITIAL> {op} => ( T.OP yytext );
<INITIAL> {int} => ( T.CON_int (valOf (Int.fromString yytext)) );
<INITIAL> "\"" => ( YYBEGIN(CON_STRING); stringbuf := ""; continue() );
<INITIAL> "(" => ( T.LP );
<INITIAL> ")" => ( T.RP );
<INITIAL> "[" => ( T.LB );
<INITIAL> "]" => ( T.RB );
<INITIAL> "," => ( T.COMMA );
<INITIAL> ";" => ( T.SEMI );
<INITIAL> ":" => ( T.COLON );
<INITIAL> " " | \n | \t => ( continue() );

<CON_STRING> "\"" => ( YYBEGIN(INITIAL); T.CON_string(!stringbuf) );
<CON_STRING> [^"]* => ( stringbuf := (!stringbuf ^ yytext); continue() );

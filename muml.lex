%name MumlLexer;

%let digit = [0-9];
%let int = {digit}+;
%let alpha = [a-zA-Z];
%let id = {alpha}({alpha} | {digit})*;

%states CON_STRING;

%defs (
  structure T = MumlTokens
  type lex_result = T.token
  fun eof() = T.EOF
);

let => ( T.KW_let );
in => ( T.KW_in );
end => ( T.KW_end );
fn => ( T.KW_fn );
fun => ( T.KW_fun );
{id} => ( T.ID yytext );
{int} => ( T.CON_int (valOf (Int.fromString yytext)) );
"\"" => ( YYBEGIN(CON_STRING); continue() );
"(" => ( T.LP );
")" => ( T.RP );
"," => ( T.COMMA );
";" => ( T.SEMI );
" " | \n | \t => ( continue() );

<CON_STRING> "\"" => ( YYBEGIN(INITIAL); T.CON_string("") );

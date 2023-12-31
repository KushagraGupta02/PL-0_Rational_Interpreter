signature My_TOKENS =
sig
type ('a,'b) token
type svalue
val PROCEDURE:  'a * 'a -> (svalue,'a) token
val IDENTIFIER: (string) *  'a * 'a -> (svalue,'a) token
val RATIONAL: (string) *  'a * 'a -> (svalue,'a) token
val POSNUMERAL: (string) *  'a * 'a -> (svalue,'a) token
val FROMDECIMAL:  'a * 'a -> (svalue,'a) token
val MAKERAT:  'a * 'a -> (svalue,'a) token
val PRINT:  'a * 'a -> (svalue,'a) token
val CALL:  'a * 'a -> (svalue,'a) token
val WRITE:  'a * 'a -> (svalue,'a) token
val READ:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val NEG:  'a * 'a -> (svalue,'a) token
val FF:  'a * 'a -> (svalue,'a) token
val TT:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val RDIV:  'a * 'a -> (svalue,'a) token
val RTIMES:  'a * 'a -> (svalue,'a) token
val RMINUS:  'a * 'a -> (svalue,'a) token
val RPLUS:  'a * 'a -> (svalue,'a) token
val MOD:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val TIMES:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val GEQ:  'a * 'a -> (svalue,'a) token
val GT:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val NEQ:  'a * 'a -> (svalue,'a) token
val LEQ:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val RATFUN:  'a * 'a -> (svalue,'a) token
val INVERSE:  'a * 'a -> (svalue,'a) token
val RAT:  'a * 'a -> (svalue,'a) token
val BOOL:  'a * 'a -> (svalue,'a) token
val INT:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val OD:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val FI:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
end
signature My_LRVALS=
sig
structure Tokens : My_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end

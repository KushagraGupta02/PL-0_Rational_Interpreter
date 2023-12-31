functor MyLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : My_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open DataTypes ; 
exception VariableRedeclarationException of string ; 
exception TypeMisMatchException ; 
exception UnDeclaredVariableException ;  


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\014\000\000\000\
\\001\000\002\000\037\000\000\000\
\\001\000\003\000\039\000\000\000\
\\001\000\003\000\041\000\000\000\
\\001\000\003\000\054\000\019\000\053\000\020\000\052\000\036\000\051\000\
\\039\000\050\000\040\000\049\000\041\000\048\000\047\000\047\000\
\\048\000\046\000\049\000\045\000\050\000\044\000\051\000\043\000\000\000\
\\001\000\003\000\080\000\000\000\
\\001\000\003\000\081\000\000\000\
\\001\000\003\000\084\000\000\000\
\\001\000\003\000\085\000\000\000\
\\001\000\004\000\088\000\021\000\078\000\022\000\077\000\023\000\076\000\
\\024\000\075\000\025\000\074\000\026\000\073\000\027\000\072\000\
\\028\000\071\000\029\000\070\000\030\000\069\000\031\000\068\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\035\000\064\000\
\\037\000\063\000\038\000\062\000\000\000\
\\001\000\004\000\089\000\000\000\
\\001\000\004\000\112\000\021\000\078\000\022\000\077\000\023\000\076\000\
\\024\000\075\000\025\000\074\000\026\000\073\000\027\000\072\000\
\\028\000\071\000\029\000\070\000\030\000\069\000\031\000\068\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\035\000\064\000\
\\037\000\063\000\038\000\062\000\000\000\
\\001\000\004\000\115\000\021\000\078\000\022\000\077\000\023\000\076\000\
\\024\000\075\000\025\000\074\000\026\000\073\000\027\000\072\000\
\\028\000\071\000\029\000\070\000\030\000\069\000\031\000\068\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\035\000\064\000\
\\037\000\063\000\038\000\062\000\000\000\
\\001\000\004\000\117\000\021\000\078\000\022\000\077\000\023\000\076\000\
\\024\000\075\000\025\000\074\000\026\000\073\000\027\000\072\000\
\\028\000\071\000\029\000\070\000\030\000\069\000\031\000\068\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\035\000\064\000\
\\037\000\063\000\038\000\062\000\000\000\
\\001\000\004\000\118\000\021\000\078\000\022\000\077\000\023\000\076\000\
\\024\000\075\000\025\000\074\000\026\000\073\000\027\000\072\000\
\\028\000\071\000\029\000\070\000\030\000\069\000\031\000\068\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\035\000\064\000\
\\037\000\063\000\038\000\062\000\000\000\
\\001\000\004\000\122\000\021\000\078\000\022\000\077\000\023\000\076\000\
\\024\000\075\000\025\000\074\000\026\000\073\000\027\000\072\000\
\\028\000\071\000\029\000\070\000\030\000\069\000\031\000\068\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\035\000\064\000\
\\037\000\063\000\038\000\062\000\000\000\
\\001\000\005\000\000\000\000\000\
\\001\000\007\000\087\000\021\000\078\000\022\000\077\000\023\000\076\000\
\\024\000\075\000\025\000\074\000\026\000\073\000\027\000\072\000\
\\028\000\071\000\029\000\070\000\030\000\069\000\031\000\068\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\035\000\064\000\
\\037\000\063\000\038\000\062\000\000\000\
\\001\000\008\000\119\000\000\000\
\\001\000\009\000\123\000\000\000\
\\001\000\011\000\079\000\021\000\078\000\022\000\077\000\023\000\076\000\
\\024\000\075\000\025\000\074\000\026\000\073\000\027\000\072\000\
\\028\000\071\000\029\000\070\000\030\000\069\000\031\000\068\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\035\000\064\000\
\\037\000\063\000\038\000\062\000\000\000\
\\001\000\012\000\114\000\000\000\
\\001\000\014\000\020\000\000\000\
\\001\000\014\000\030\000\000\000\
\\001\000\014\000\033\000\000\000\
\\001\000\014\000\036\000\000\000\
\\001\000\014\000\057\000\000\000\
\\001\000\015\000\116\000\021\000\078\000\022\000\077\000\023\000\076\000\
\\024\000\075\000\025\000\074\000\026\000\073\000\027\000\072\000\
\\028\000\071\000\029\000\070\000\030\000\069\000\031\000\068\000\
\\032\000\067\000\033\000\066\000\034\000\065\000\035\000\064\000\
\\037\000\063\000\038\000\062\000\000\000\
\\001\000\042\000\038\000\000\000\
\\001\000\051\000\016\000\000\000\
\\001\000\051\000\021\000\000\000\
\\001\000\051\000\040\000\000\000\
\\001\000\051\000\061\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\006\000\029\000\010\000\028\000\043\000\027\000\045\000\026\000\
\\046\000\025\000\051\000\024\000\000\000\
\\131\000\000\000\
\\132\000\018\000\007\000\000\000\
\\133\000\000\000\
\\134\000\016\000\009\000\000\000\
\\135\000\000\000\
\\136\000\017\000\018\000\000\000\
\\137\000\000\000\
\\138\000\052\000\012\000\000\000\
\\139\000\000\000\
\\140\000\000\000\
\\141\000\015\000\031\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\021\000\078\000\022\000\077\000\023\000\076\000\024\000\075\000\
\\025\000\074\000\026\000\073\000\027\000\072\000\028\000\071\000\
\\029\000\070\000\030\000\069\000\031\000\068\000\032\000\067\000\
\\033\000\066\000\034\000\065\000\035\000\064\000\037\000\063\000\
\\038\000\062\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\027\000\072\000\028\000\071\000\029\000\070\000\030\000\069\000\
\\031\000\068\000\032\000\067\000\033\000\066\000\034\000\065\000\
\\035\000\064\000\000\000\
\\150\000\027\000\072\000\028\000\071\000\029\000\070\000\030\000\069\000\
\\031\000\068\000\032\000\067\000\033\000\066\000\034\000\065\000\
\\035\000\064\000\000\000\
\\151\000\021\000\078\000\022\000\077\000\025\000\074\000\026\000\073\000\
\\027\000\072\000\028\000\071\000\029\000\070\000\030\000\069\000\
\\031\000\068\000\032\000\067\000\033\000\066\000\034\000\065\000\
\\035\000\064\000\000\000\
\\152\000\021\000\078\000\022\000\077\000\025\000\074\000\026\000\073\000\
\\027\000\072\000\028\000\071\000\029\000\070\000\030\000\069\000\
\\031\000\068\000\032\000\067\000\033\000\066\000\034\000\065\000\
\\035\000\064\000\000\000\
\\153\000\027\000\072\000\028\000\071\000\029\000\070\000\030\000\069\000\
\\031\000\068\000\032\000\067\000\033\000\066\000\034\000\065\000\
\\035\000\064\000\000\000\
\\154\000\027\000\072\000\028\000\071\000\029\000\070\000\030\000\069\000\
\\031\000\068\000\032\000\067\000\033\000\066\000\034\000\065\000\
\\035\000\064\000\000\000\
\\155\000\029\000\070\000\030\000\069\000\031\000\068\000\032\000\067\000\
\\033\000\066\000\034\000\065\000\035\000\064\000\000\000\
\\156\000\029\000\070\000\030\000\069\000\031\000\068\000\032\000\067\000\
\\033\000\066\000\034\000\065\000\035\000\064\000\000\000\
\\157\000\032\000\067\000\033\000\066\000\034\000\065\000\035\000\064\000\000\000\
\\158\000\032\000\067\000\033\000\066\000\034\000\065\000\035\000\064\000\000\000\
\\159\000\032\000\067\000\033\000\066\000\034\000\065\000\035\000\064\000\000\000\
\\160\000\034\000\065\000\035\000\064\000\000\000\
\\161\000\034\000\065\000\035\000\064\000\000\000\
\\162\000\000\000\
\\163\000\000\000\
\\164\000\021\000\078\000\022\000\077\000\023\000\076\000\024\000\075\000\
\\025\000\074\000\026\000\073\000\027\000\072\000\028\000\071\000\
\\029\000\070\000\030\000\069\000\031\000\068\000\032\000\067\000\
\\033\000\066\000\034\000\065\000\035\000\064\000\000\000\
\\165\000\021\000\078\000\022\000\077\000\023\000\076\000\024\000\075\000\
\\025\000\074\000\026\000\073\000\027\000\072\000\028\000\071\000\
\\029\000\070\000\030\000\069\000\031\000\068\000\032\000\067\000\
\\033\000\066\000\034\000\065\000\035\000\064\000\037\000\063\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\"
val actionRowNumbers =
"\040\000\042\000\046\000\000\000\
\\033\000\029\000\044\000\029\000\
\\022\000\035\000\030\000\034\000\
\\038\000\023\000\049\000\036\000\
\\029\000\024\000\046\000\040\000\
\\025\000\001\000\028\000\002\000\
\\031\000\003\000\004\000\004\000\
\\041\000\029\000\026\000\043\000\
\\047\000\048\000\038\000\037\000\
\\004\000\004\000\056\000\032\000\
\\020\000\084\000\083\000\082\000\
\\005\000\006\000\004\000\081\000\
\\080\000\004\000\007\000\008\000\
\\004\000\017\000\050\000\045\000\
\\039\000\053\000\009\000\010\000\
\\004\000\004\000\004\000\004\000\
\\004\000\004\000\004\000\004\000\
\\004\000\004\000\004\000\004\000\
\\004\000\004\000\004\000\004\000\
\\004\000\000\000\004\000\004\000\
\\075\000\085\000\004\000\004\000\
\\011\000\000\000\052\000\051\000\
\\073\000\072\000\071\000\070\000\
\\069\000\068\000\067\000\066\000\
\\065\000\064\000\063\000\062\000\
\\061\000\059\000\060\000\057\000\
\\058\000\021\000\012\000\027\000\
\\013\000\014\000\074\000\018\000\
\\055\000\079\000\004\000\077\000\
\\076\000\000\000\015\000\019\000\
\\078\000\054\000\016\000"
val gotoT =
"\
\\001\000\122\000\002\000\004\000\003\000\003\000\005\000\002\000\
\\006\000\001\000\000\000\
\\007\000\006\000\000\000\
\\010\000\009\000\011\000\008\000\000\000\
\\004\000\011\000\000\000\
\\000\000\
\\013\000\013\000\000\000\
\\008\000\015\000\000\000\
\\013\000\017\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\021\000\012\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\030\000\000\000\
\\000\000\
\\010\000\032\000\011\000\008\000\000\000\
\\002\000\033\000\003\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\040\000\000\000\
\\014\000\053\000\000\000\
\\000\000\
\\013\000\054\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\056\000\012\000\020\000\000\000\
\\000\000\
\\014\000\057\000\000\000\
\\014\000\058\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\080\000\000\000\
\\000\000\
\\000\000\
\\014\000\081\000\000\000\
\\000\000\
\\000\000\
\\014\000\084\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\088\000\000\000\
\\014\000\089\000\000\000\
\\014\000\090\000\000\000\
\\014\000\091\000\000\000\
\\014\000\092\000\000\000\
\\014\000\093\000\000\000\
\\014\000\094\000\000\000\
\\014\000\095\000\000\000\
\\014\000\096\000\000\000\
\\014\000\097\000\000\000\
\\014\000\098\000\000\000\
\\014\000\099\000\000\000\
\\014\000\100\000\000\000\
\\014\000\101\000\000\000\
\\014\000\102\000\000\000\
\\014\000\103\000\000\000\
\\014\000\104\000\000\000\
\\004\000\105\000\000\000\
\\014\000\106\000\000\000\
\\014\000\107\000\000\000\
\\000\000\
\\000\000\
\\014\000\108\000\000\000\
\\014\000\109\000\000\000\
\\000\000\
\\004\000\111\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\118\000\000\000\
\\000\000\
\\000\000\
\\004\000\119\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 123
val numrules = 53
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | IDENTIFIER of unit ->  (string) | RATIONAL of unit ->  (string)
 | POSNUMERAL of unit ->  (string) | expression of unit ->  (Exp)
 | varlist of unit ->  (string list) | command of unit ->  (CMD)
 | procdef of unit ->  (PROCDEF) | procdec of unit ->  (PROCDEF list)
 | commands of unit ->  (CMD list) | booldec of unit ->  (BOOLDEC)
 | intdec of unit ->  (INTDEC) | ratdec of unit ->  (RATDEC)
 | vardec of unit ->  (VARDEC) | comseq of unit ->  (CMD list)
 | decseq of unit ->  (DECSEQ) | blk of unit ->  (BLK)
 | start of unit ->  (AST)
end
type svalue = MlyValue.svalue
type result = AST
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 4) => true | _ => false
val showTerminal =
fn (T 0) => "LBRACE"
  | (T 1) => "RBRACE"
  | (T 2) => "LPAREN"
  | (T 3) => "RPAREN"
  | (T 4) => "EOF"
  | (T 5) => "IF"
  | (T 6) => "THEN"
  | (T 7) => "ELSE"
  | (T 8) => "FI"
  | (T 9) => "WHILE"
  | (T 10) => "DO"
  | (T 11) => "OD"
  | (T 12) => "COLON"
  | (T 13) => "SEMICOLON"
  | (T 14) => "COMMA"
  | (T 15) => "INT"
  | (T 16) => "BOOL"
  | (T 17) => "RAT"
  | (T 18) => "INVERSE"
  | (T 19) => "RATFUN"
  | (T 20) => "LT"
  | (T 21) => "LEQ"
  | (T 22) => "NEQ"
  | (T 23) => "EQ"
  | (T 24) => "GT"
  | (T 25) => "GEQ"
  | (T 26) => "PLUS"
  | (T 27) => "MINUS"
  | (T 28) => "TIMES"
  | (T 29) => "DIV"
  | (T 30) => "MOD"
  | (T 31) => "RPLUS"
  | (T 32) => "RMINUS"
  | (T 33) => "RTIMES"
  | (T 34) => "RDIV"
  | (T 35) => "NOT"
  | (T 36) => "AND"
  | (T 37) => "OR"
  | (T 38) => "TT"
  | (T 39) => "FF"
  | (T 40) => "NEG"
  | (T 41) => "ASSIGN"
  | (T 42) => "READ"
  | (T 43) => "WRITE"
  | (T 44) => "CALL"
  | (T 45) => "PRINT"
  | (T 46) => "MAKERAT"
  | (T 47) => "FROMDECIMAL"
  | (T 48) => "POSNUMERAL"
  | (T 49) => "RATIONAL"
  | (T 50) => "IDENTIFIER"
  | (T 51) => "PROCEDURE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 51) $$ (T 47) $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42)
 $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35)
 $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28)
 $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21)
 $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14)
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.blk blk1, blk1left, blk1right)) :: rest671)
) => let val  result = MlyValue.start (fn _ => let val  (blk as blk1)
 = blk1 ()
 in (PROG(blk))
end)
 in ( LrTable.NT 0, ( result, blk1left, blk1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.comseq comseq1, _, comseq1right)) :: ( _, ( 
MlyValue.decseq decseq1, decseq1left, _)) :: rest671)) => let val  
result = MlyValue.blk (fn _ => let val  (decseq as decseq1) = decseq1
 ()
 val  (comseq as comseq1) = comseq1 ()
 in (BLK(decseq , comseq ))
end)
 in ( LrTable.NT 1, ( result, decseq1left, comseq1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.procdec procdec1, _, procdec1right)) :: ( _,
 ( MlyValue.vardec vardec1, vardec1left, _)) :: rest671)) => let val  
result = MlyValue.decseq (fn _ => let val  (vardec as vardec1) = 
vardec1 ()
 val  (procdec as procdec1) = procdec1 ()
 in (DECSEQ(vardec,procdec))
end)
 in ( LrTable.NT 2, ( result, vardec1left, procdec1right), rest671)

end
|  ( 3, ( ( _, ( MlyValue.booldec booldec1, _, booldec1right)) :: ( _,
 ( MlyValue.intdec intdec1, _, _)) :: ( _, ( MlyValue.ratdec ratdec1, 
ratdec1left, _)) :: rest671)) => let val  result = MlyValue.vardec (fn
 _ => let val  (ratdec as ratdec1) = ratdec1 ()
 val  (intdec as intdec1) = intdec1 ()
 val  (booldec as booldec1) = booldec1 ()
 in (VARDEC(ratdec, intdec, booldec))
end)
 in ( LrTable.NT 4, ( result, ratdec1left, booldec1right), rest671)

end
|  ( 4, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.commands 
commands1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.comseq (fn _ => let val  (commands as 
commands1) = commands1 ()
 in ((commands))
end)
 in ( LrTable.NT 3, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 5, ( rest671)) => let val  result = MlyValue.commands (fn _ => (
([])))
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 6, ( ( _, ( MlyValue.commands commands1, _, commands1right)) :: _
 :: ( _, ( MlyValue.command command1, command1left, _)) :: rest671))
 => let val  result = MlyValue.commands (fn _ => let val  (command as 
command1) = command1 ()
 val  (commands as commands1) = commands1 ()
 in ((command::commands))
end)
 in ( LrTable.NT 8, ( result, command1left, commands1right), rest671)

end
|  ( 7, ( rest671)) => let val  result = MlyValue.ratdec (fn _ => (
RATDEC(Rational,[])))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 8, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.varlist 
varlist1, _, _)) :: ( _, ( _, RAT1left, _)) :: rest671)) => let val  
result = MlyValue.ratdec (fn _ => let val  (varlist as varlist1) = 
varlist1 ()
 in (( RATDEC(Rational, varlist)))
end)
 in ( LrTable.NT 5, ( result, RAT1left, SEMICOLON1right), rest671)
end
|  ( 9, ( rest671)) => let val  result = MlyValue.intdec (fn _ => (
INTDEC(Int, [])))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 10, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.varlist 
varlist1, _, _)) :: ( _, ( _, INT1left, _)) :: rest671)) => let val  
result = MlyValue.intdec (fn _ => let val  (varlist as varlist1) = 
varlist1 ()
 in (( INTDEC(Int, varlist)))
end)
 in ( LrTable.NT 6, ( result, INT1left, SEMICOLON1right), rest671)
end
|  ( 11, ( rest671)) => let val  result = MlyValue.booldec (fn _ => (
BOOLDEC(Bool, [])))
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 12, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.varlist 
varlist1, _, _)) :: ( _, ( _, BOOL1left, _)) :: rest671)) => let val  
result = MlyValue.booldec (fn _ => let val  (varlist as varlist1) = 
varlist1 ()
 in ((BOOLDEC(Bool, varlist)))
end)
 in ( LrTable.NT 7, ( result, BOOL1left, SEMICOLON1right), rest671)

end
|  ( 13, ( rest671)) => let val  result = MlyValue.procdec (fn _ => (
([])))
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 14, ( ( _, ( MlyValue.procdec procdec1, _, procdec1right)) :: _
 :: ( _, ( MlyValue.procdef procdef1, procdef1left, _)) :: rest671))
 => let val  result = MlyValue.procdec (fn _ => let val  (procdef as 
procdef1) = procdef1 ()
 val  (procdec as procdec1) = procdec1 ()
 in ((procdef::procdec))
end)
 in ( LrTable.NT 9, ( result, procdef1left, procdec1right), rest671)

end
|  ( 15, ( ( _, ( MlyValue.blk blk1, _, blk1right)) :: ( _, ( 
MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, PROCEDURE1left, _
)) :: rest671)) => let val  result = MlyValue.procdef (fn _ => let
 val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (blk as blk1) = blk1 ()
 in (PROCDEF(IDENTIFIER,blk))
end)
 in ( LrTable.NT 10, ( result, PROCEDURE1left, blk1right), rest671)

end
|  ( 16, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.varlist
 (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 in (([IDENTIFIER]))
end)
 in ( LrTable.NT 12, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
|  ( 17, ( ( _, ( MlyValue.varlist varlist1, _, varlist1right)) :: _
 :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, _)) :: 
rest671)) => let val  result = MlyValue.varlist (fn _ => let val  (
IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (varlist as varlist1) = varlist1 ()
 in ((IDENTIFIER::varlist))
end)
 in ( LrTable.NT 12, ( result, IDENTIFIER1left, varlist1right), 
rest671)
end
|  ( 18, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.IDENTIFIER 
IDENTIFIER1, _, _)) :: _ :: ( _, ( _, READ1left, _)) :: rest671)) =>
 let val  result = MlyValue.command (fn _ => let val  (IDENTIFIER as 
IDENTIFIER1) = IDENTIFIER1 ()
 in ((Read(IDENTIFIER)))
end)
 in ( LrTable.NT 11, ( result, READ1left, RPAREN1right), rest671)
end
|  ( 19, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: _ :: ( _, ( _, PRINT1left, _)) :: rest671)) =>
 let val  result = MlyValue.command (fn _ => let val  (expression as 
expression1) = expression1 ()
 in ((Print(expression)))
end)
 in ( LrTable.NT 11, ( result, PRINT1left, RPAREN1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, _)
) :: rest671)) => let val  result = MlyValue.command (fn _ => let val 
 (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (expression as expression1) = expression1 ()
 in ((SET(IDENTIFIER , expression )))
end)
 in ( LrTable.NT 11, ( result, IDENTIFIER1left, expression1right), 
rest671)
end
|  ( 21, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.comseq comseq2,
 _, _)) :: _ :: ( _, ( MlyValue.comseq comseq1, _, _)) :: _ :: ( _, ( 
MlyValue.expression expression1, _, _)) :: ( _, ( _, IF1left, _)) :: 
rest671)) => let val  result = MlyValue.command (fn _ => let val  (
expression as expression1) = expression1 ()
 val  comseq1 = comseq1 ()
 val  comseq2 = comseq2 ()
 in (ITE(  expression , comseq1  , comseq2))
end)
 in ( LrTable.NT 11, ( result, IF1left, FI1right), rest671)
end
|  ( 22, ( ( _, ( _, _, OD1right)) :: ( _, ( MlyValue.comseq comseq1,
 _, _)) :: _ :: ( _, ( MlyValue.expression expression1, _, _)) :: ( _,
 ( _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.command (fn _ => let val  (expression as expression1) = 
expression1 ()
 val  (comseq as comseq1) = comseq1 ()
 in ( (WH(  expression , comseq )))
end)
 in ( LrTable.NT 11, ( result, WHILE1left, OD1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, _, IDENTIFIER1right
)) :: ( _, ( _, CALL1left, _)) :: rest671)) => let val  result = 
MlyValue.command (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = 
IDENTIFIER1 ()
 in ((Call(IDENTIFIER)))
end)
 in ( LrTable.NT 11, ( result, CALL1left, IDENTIFIER1right), rest671)

end
|  ( 24, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ( ( LEQ( expression1 ,  expression2)))
end)
 in ( LrTable.NT 13, ( result, expression1left, expression2right), 
rest671)
end
|  ( 25, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ( ( LT( expression1 ,  expression2)))
end)
 in ( LrTable.NT 13, ( result, expression1left, expression2right), 
rest671)
end
|  ( 26, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( EQ( expression1 ,  expression2)))
end)
 in ( LrTable.NT 13, ( result, expression1left, expression2right), 
rest671)
end
|  ( 27, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( NEQ( expression1 ,  expression2)) )
end)
 in ( LrTable.NT 13, ( result, expression1left, expression2right), 
rest671)
end
|  ( 28, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( GT( expression1 ,  expression2)))
end)
 in ( LrTable.NT 13, ( result, expression1left, expression2right), 
rest671)
end
|  ( 29, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( GEQ( expression1 ,  expression2)))
end)
 in ( LrTable.NT 13, ( result, expression1left, expression2right), 
rest671)
end
|  ( 30, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ( (PLUS( expression1 ,  expression2)))
end)
 in ( LrTable.NT 13, ( result, expression1left, expression2right), 
rest671)
end
|  ( 31, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( MINUS( expression1 ,  expression2)))
end)
 in ( LrTable.NT 13, ( result, expression1left, expression2right), 
rest671)
end
|  ( 32, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( TIMES( expression1 ,  expression2)))
end)
 in ( LrTable.NT 13, ( result, expression1left, expression2right), 
rest671)
end
|  ( 33, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( DIV( expression1 ,  expression2)))
end)
 in ( LrTable.NT 13, ( result, expression1left, expression2right), 
rest671)
end
|  ( 34, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( MOD( expression1 ,  expression2)))
end)
 in ( LrTable.NT 13, ( result, expression1left, expression2right), 
rest671)
end
|  ( 35, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( RPLUS( expression1 ,  expression2)))
end)
 in ( LrTable.NT 13, ( result, expression1left, expression2right), 
rest671)
end
|  ( 36, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( RMINUS( expression1 ,  expression2)))
end)
 in ( LrTable.NT 13, ( result, expression1left, expression2right), 
rest671)
end
|  ( 37, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( RTIMES( expression1 ,  expression2)))
end)
 in ( LrTable.NT 13, ( result, expression1left, expression2right), 
rest671)
end
|  ( 38, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (( RDIV( expression1 ,  expression2)))
end)
 in ( LrTable.NT 13, ( result, expression1left, expression2right), 
rest671)
end
|  ( 39, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((AND( expression1 ,  expression2))  )
end)
 in ( LrTable.NT 13, ( result, expression1left, expression2right), 
rest671)
end
|  ( 40, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in ((OR( expression1 ,  expression2)) )
end)
 in ( LrTable.NT 13, ( result, expression1left, expression2right), 
rest671)
end
|  ( 41, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.expression (fn _ => let val  (expression as 
expression1) = expression1 ()
 in (( expression))
end)
 in ( LrTable.NT 13, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 42, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, NEG1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (expression as expression1) = 
expression1 ()
 in ((NEG(  expression)))
end)
 in ( LrTable.NT 13, ( result, NEG1left, expression1right), rest671)

end
|  ( 43, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: _ :: ( _, ( _, INVERSE1left, _)) :: rest671))
 => let val  result = MlyValue.expression (fn _ => let val  (
expression as expression1) = expression1 ()
 in ((INVERSE( expression)))
end)
 in ( LrTable.NT 13, ( result, INVERSE1left, RPAREN1right), rest671)

end
|  ( 44, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: _ :: ( _, ( _, RATFUN1left, _)) :: rest671)) =>
 let val  result = MlyValue.expression (fn _ => let val  (expression
 as expression1) = expression1 ()
 in ((RATFUN( expression)))
end)
 in ( LrTable.NT 13, ( result, RATFUN1left, RPAREN1right), rest671)

end
|  ( 45, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression2, _, _)) :: _ :: ( _, ( MlyValue.expression expression1, _,
 _)) :: _ :: ( _, ( _, MAKERAT1left, _)) :: rest671)) => let val  
result = MlyValue.expression (fn _ => let val  expression1 = 
expression1 ()
 val  expression2 = expression2 ()
 in ((make_rat(expression1,expression2)))
end)
 in ( LrTable.NT 13, ( result, MAKERAT1left, RPAREN1right), rest671)

end
|  ( 46, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: _ :: ( _, ( _, FROMDECIMAL1left, _)) :: rest671
)) => let val  result = MlyValue.expression (fn _ => let val  (
expression as expression1) = expression1 ()
 in ((fromDecimal(expression)))
end)
 in ( LrTable.NT 13, ( result, FROMDECIMAL1left, RPAREN1right), 
rest671)
end
|  ( 47, ( ( _, ( _, TT1left, TT1right)) :: rest671)) => let val  
result = MlyValue.expression (fn _ => ((TT )))
 in ( LrTable.NT 13, ( result, TT1left, TT1right), rest671)
end
|  ( 48, ( ( _, ( _, FF1left, FF1right)) :: rest671)) => let val  
result = MlyValue.expression (fn _ => ((FF )))
 in ( LrTable.NT 13, ( result, FF1left, FF1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.POSNUMERAL POSNUMERAL1, POSNUMERAL1left, 
POSNUMERAL1right)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (POSNUMERAL as POSNUMERAL1) = 
POSNUMERAL1 ()
 in ((Posnumeral(POSNUMERAL) ))
end)
 in ( LrTable.NT 13, ( result, POSNUMERAL1left, POSNUMERAL1right), 
rest671)
end
|  ( 50, ( ( _, ( MlyValue.RATIONAL RATIONAL1, RATIONAL1left, 
RATIONAL1right)) :: rest671)) => let val  result = MlyValue.expression
 (fn _ => let val  (RATIONAL as RATIONAL1) = RATIONAL1 ()
 in (( Rational_numeral(RATIONAL) ))
end)
 in ( LrTable.NT 13, ( result, RATIONAL1left, RATIONAL1right), rest671
)
end
|  ( 51, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = 
IDENTIFIER1 ()
 in (( Identifier(IDENTIFIER) ))
end)
 in ( LrTable.NT 13, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
|  ( 52, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (expression as expression1) = 
expression1 ()
 in ( (NOT( expression)) )
end)
 in ( LrTable.NT 13, ( result, NOT1left, expression1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : My_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun OD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun RAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun INVERSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun RATFUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun RPLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun RMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun RTIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun RDIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun CALL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun MAKERAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
fun FROMDECIMAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.VOID,p1,p2))
fun POSNUMERAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(
ParserData.MlyValue.POSNUMERAL (fn () => i),p1,p2))
fun RATIONAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(
ParserData.MlyValue.RATIONAL (fn () => i),p1,p2))
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 50,(
ParserData.MlyValue.IDENTIFIER (fn () => i),p1,p2))
fun PROCEDURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 51,(
ParserData.MlyValue.VOID,p1,p2))
end
end

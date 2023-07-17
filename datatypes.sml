
exception VariableRedeclarationException of string ; 
exception TypeMisMatchException ; 
exception UnDeclaredVariableException ; 
fun clearSymbol(SymbolTable)=  SymbolTable := [];
fun findSymbol(SymbolTable, id, l)=
case (!SymbolTable) of [] => l |
((a,b,c,d)::xs) => if a = id then findSymbol(ref (xs),id, ((a,b,c,d))::l) else findSymbol(ref (xs),id,l)

fun InputCommandLine( display : string ) =
   ( print ( display ^ "\n") ; 
    let
	val input= valOf(TextIO.inputLine TextIO.stdIn)
    in
	String.substring ( input , 0 ,  (String.size input) -1) 
    end)

fun findHelper(SymbolTable, id, scopeno)= 
let val L=findSymbol(SymbolTable,id,[])
fun Helper(L, id, scopeno)=
                case L of [] => NONE |
                (a,b,c,d)::xs => if a= id andalso d= scopeno then SOME(c,b) else Helper(xs, id, scopeno)
in (Helper(L, id, scopeno))
end

fun find(SymbolTable, id, scopeno, Link)= 
case (Link) of [] => NONE |
(x::xs) => if (findHelper(SymbolTable,id, x) = NONE) then find(SymbolTable,id, scopeno,xs) else findHelper(SymbolTable,id, x)


fun insertSymbol(SymbolTable, idList, idType, scopeno)= 
if ( null idList ) then () 
else (if findHelper(SymbolTable,hd idList, scopeno) <> NONE then ()
else 
        SymbolTable := (hd idList , idType, "", scopeno )::(!SymbolTable) ; insertSymbol( SymbolTable, tl idList , idType, scopeno) ); 
fun update(SymbolTable, id, value, idType, scopeno,Link)=
let 
fun updateHelper(SymbolTable, id, value, idType, scopeno)= 
        let 
                val L = findSymbol(SymbolTable, id,[])
                fun findList(L,id, scopeno)=
                case L of [] => false |
                (a,b,c,d)::xs => if d = scopeno then true else findList(xs, id, scopeno)

                fun Replace(SymbolTable, id, value, idType, scopeno, arr)=
                let
                fun ReplaceHelper(SymbolTable, id, value, idType, scopeno, arr)=
                case !SymbolTable of [] => NONE |
                (a,b,c,d)::xs => if a= id andalso d= scopeno then if b= idType then SOME((arr)@[(id,idType,value, scopeno)]@xs)  else raise TypeMisMatchException
                        else ReplaceHelper(ref xs, id, value, idType, scopeno,(a,b,c,d)::arr);
                in if ReplaceHelper((SymbolTable, id, value, idType, scopeno, arr)) = NONE then ()
                else SymbolTable := valOf(ReplaceHelper((SymbolTable, id, value, idType, scopeno, arr)))
                end
        val t = findList (L, id, scopeno)
        in
        if t then let val _ =Replace(SymbolTable, id, value, idType, scopeno, []) in true end
        else false
        end
in
case (Link) of [] => raise UnDeclaredVariableException |
(x::xs) => if updateHelper(SymbolTable, id, value, idType, x) then () else update(SymbolTable, id, value, idType, x, xs)
end

fun clearProc(ProcTable)=  ProcTable := [];
fun findProc(ProcTable, id)=
case (!ProcTable) of [] => NONE|
((a,b,c)::xs) => if a = id then (SOME (a,b,c)) else findProc(ref (xs),id)

fun insertProc(ProcTable, id, blk, scopelist)= 
(if findProc(ProcTable,id)<> NONE then raise VariableRedeclarationException(id) 
else 
        ProcTable := (id, blk, scopelist )::(!ProcTable) );

structure DataTypes = 
struct
datatype AST  = PROG of BLK 
and BLK = BLK of DECSEQ * ( CMD list ) 
and DECSEQ = DECSEQ of VARDEC * PROCDEF list
and VARDEC = VARDEC of RATDEC * INTDEC * BOOLDEC
and PROCDEF = PROCDEF of (string * BLK )
and Type = Int | Bool | Rational
and RATDEC = RATDEC of (Type *(string list))
and INTDEC = INTDEC of (Type * (string list))
and BOOLDEC = BOOLDEC of (Type * (string list) )
and CMD = SET of string*Exp | WH of Exp* (CMD list )| ITE of Exp * ( CMD list ) * (CMD list ) | Call of string | Read of string | Print of Exp
and Exp = NOT of Exp
          |  NEG of Exp
          | INVERSE of Exp
          | RATFUN of Exp
          | AND of Exp*Exp
          | OR of Exp*Exp
          | PLUS of Exp*Exp
          | MINUS of Exp*Exp
          | TIMES of Exp*Exp
          | MOD of Exp * Exp 
          | DIV of Exp * Exp 
          | RPLUS of Exp*Exp
          | RMINUS of Exp*Exp
          | RTIMES of Exp*Exp
          | RDIV of Exp * Exp 
          | LT of Exp*Exp
          | LEQ of Exp*Exp
          | NEQ of Exp*Exp
          | EQ of Exp*Exp
          | GT of Exp*Exp
          | GEQ of Exp * Exp 
          | TT
          | FF 
          | Posnumeral of string 
          | Identifier of string 
          | Rational_numeral of string
          | make_rat of Exp * Exp
          | fromDecimal of Exp

val Symbols = ref ([] : (string * Type * string * int) list);
val Procedures = ref ([] : (string * CMD list * int list) list);
val Link = ref ([0]);
val Out = ref([] :string list);
val scope = ref (0);
fun getType ( id : string ) = 
         if( findSymbol (Symbols, id, [])<>[]) 
         then 
                #2 (hd (findSymbol (Symbols, id, [])) )
         else 
                raise UnDeclaredVariableException ; 

fun checkBool ( typeToBeChecked : Type ) = 
        if ( typeToBeChecked = Bool ) then true else false ; 

fun checkInt ( typeToBeChecked : Type ) = 
        if ( typeToBeChecked = Int ) then true else false ; 

fun checkRat ( typeToBeChecked : Type ) = 
        if ( typeToBeChecked = Rational ) then true else false ;

fun printSymbol (lst : (string * Type * string * int) list) =
  let
    fun printElement ((a,b,c,d): string * Type * string * int) =
            print (a^c^Int.toString(d)^ "\n")
                in
                    List.app printElement lst
                        end;

fun printProc (lst : (string * CMD list* int list) list) =
  let
    fun printElement ((a,b,d): string * CMD list* int list) =
            print (a^ "\n")
                in
                    List.app printElement lst
                        end;

fun OutParse(Outpu : string list, outputFile)=
let
val out = TextIO.openOut outputFile
val Output = Outpu
fun make([]) = TextIO.closeOut out
  | make(x::xs) = 
    let val _ = TextIO.output(out,x)
    val hj = TextIO.output(out,"") 
    in make(xs) 
    end
    val _ = make(Output)
    val l_ = TextIO.closeOut out
in
()
end



fun evaluateExpression(e,scopeno)=
case e of
NOT(a) => if ((#1 (evaluateExpression(a,scopeno)))="ff")  then ("tt",Bool) else ("ff",Bool)
| NEG(a) => if #2(evaluateExpression(a,scopeno))=Int then ((BigInt.toString(BigInt.fromString("~"^(#1 (evaluateExpression(a,scopeno)))))),Int) else if #2(evaluateExpression(a,scopeno))=Rational then ((Rational.toDecimal(Rational.fromDecimal("~"^ (#1 (evaluateExpression(a,scopeno)))))),Rational) else raise TypeMisMatchException
| AND(a,b) => if #1 (evaluateExpression(a,scopeno)) ="ff" then ("ff",Bool) else if #1 (evaluateExpression(b,scopeno))="ff" then ("ff",Bool) else ("tt",Bool)
| OR(a,b) => if #1 (evaluateExpression(a,scopeno)) ="tt" then ("tt",Bool) else if #1 (evaluateExpression(b,scopeno))="tt" then ("tt",Bool) else ("ff",Bool)
|PLUS(a,b) => ((BigInt.toString(BigInt.add(BigInt.fromString(#1 (evaluateExpression(a,scopeno))), BigInt.fromString(#1 (evaluateExpression(b,scopeno)))))),Int)
| MINUS(a,b) => ((BigInt.toString(BigInt.sub(BigInt.fromString(#1 (evaluateExpression(a,scopeno))), BigInt.fromString(#1 (evaluateExpression(b,scopeno)))))),Int)
| TIMES(a,b) => ((BigInt.toString(BigInt.mult(BigInt.fromString(#1 (evaluateExpression(a,scopeno))), BigInt.fromString(#1 (evaluateExpression(b,scopeno)))))),Int)
| DIV(a,b) => ((BigInt.toString((             ((#1(BigInt.fromString(#1 (evaluateExpression(a,scopeno)))) andalso (not (#1(BigInt.fromString(#1 (evaluateExpression(b,scopeno))))))) orelse (#1(BigInt.fromString(#1 (evaluateExpression(b,scopeno)))) andalso (not (#1(BigInt.fromString(#1 (evaluateExpression(a,scopeno))))))))         ,BigInt.divide(#2(BigInt.fromString(#1 (evaluateExpression(a,scopeno)))), #2(BigInt.fromString(#1 (evaluateExpression(b,scopeno)))),[],[]))),Int))
| MOD(a,b) => ((BigInt.toString((             ((#1(BigInt.fromString(#1 (evaluateExpression(a,scopeno)))) andalso (not (#1(BigInt.fromString(#1 (evaluateExpression(b,scopeno))))))) orelse (#1(BigInt.fromString(#1 (evaluateExpression(b,scopeno)))) andalso (not (#1(BigInt.fromString(#1 (evaluateExpression(a,scopeno))))))))         ,BigInt.remainder(#2(BigInt.fromString(#1 (evaluateExpression(a,scopeno)))), #2(BigInt.fromString(#1 (evaluateExpression(b,scopeno)))),[],[]))),Int))
| RTIMES(a,b) => ((Rational.toDecimal(Rational.multiply(Rational.fromDecimal(#1 (evaluateExpression(a,scopeno))), Rational.fromDecimal(#1 (evaluateExpression(b,scopeno)))))),Rational)
| RPLUS(a,b) => ((Rational.toDecimal(Rational.add(Rational.fromDecimal(#1 (evaluateExpression(a,scopeno))), Rational.fromDecimal(#1 (evaluateExpression(b,scopeno)))))),Rational)
| RMINUS(a,b) => ((Rational.toDecimal(Rational.subtract(Rational.fromDecimal(#1 (evaluateExpression(a,scopeno))), Rational.fromDecimal(#1 (evaluateExpression(b,scopeno)))))),Rational)
| RDIV(a,b) => ((Rational.toDecimal(valOf(Rational.divide(Rational.fromDecimal(#1 (evaluateExpression(a,scopeno))), Rational.fromDecimal(#1 (evaluateExpression(b,scopeno))))))),Rational)
| LT(a,b) => if #2(evaluateExpression(a,scopeno))=Int then if (BigInt.less(BigInt.fromString(#1 (evaluateExpression(a,scopeno))), BigInt.fromString(#1 (evaluateExpression(b,scopeno))))) then ("tt",Bool) else ("ff",Bool) 
        else if #2(evaluateExpression(a,scopeno))=Rational then if (Rational.less(Rational.fromDecimal(#1 (evaluateExpression(a,scopeno))), Rational.fromDecimal(#1 (evaluateExpression(b,scopeno))))) then ("tt",Bool) else ("ff",Bool) else if(#1 (evaluateExpression(a,scopeno))="ff" andalso #1 (evaluateExpression(b,scopeno))="tt") then ("tt",Bool) else ("ff",Bool)
| EQ(a,b) => if #2(evaluateExpression(a,scopeno))=Int then if (BigInt.equal(BigInt.fromString(#1 (evaluateExpression(a,scopeno))), BigInt.fromString(#1 (evaluateExpression(b,scopeno))))) then ("tt",Bool) else ("ff",Bool) 
        else if #2(evaluateExpression(a,scopeno))=Rational then if (Rational.equal(Rational.fromDecimal(#1 (evaluateExpression(a,scopeno))), Rational.fromDecimal(#1 (evaluateExpression(b,scopeno))))) then ("tt",Bool) else ("ff",Bool) else if(#1 (evaluateExpression(a,scopeno))= #1 (evaluateExpression(b,scopeno))) then ("tt",Bool) else ("ff",Bool)
| LEQ(a,b) => let val g= evaluateExpression(LT(a,b),scopeno) val h = evaluateExpression(EQ(a,b),scopeno) in if (#1 (g)= "tt") then ("tt",Bool) else if (#1 (h))= "tt" then ("tt",Bool) else ("ff",Bool)end
| GEQ(a,b) => let val g= evaluateExpression(LT(a,b),scopeno) in if (#1 (g)= "ff" )then ("tt",Bool) else ("ff",Bool)end
| GT(a,b) => let val g= evaluateExpression(LEQ(a,b),scopeno) in if (#1 (g)= "ff" ) then ("tt",Bool) else ("ff",Bool) end
| NEQ(a,b) => let val g= evaluateExpression(EQ(a,b),scopeno) in if (#1 (g)= "ff" ) then ("tt",Bool) else ("ff",Bool) end
| Posnumeral(a) => (a,Int)
| Identifier(a) => if find(Symbols,a,scopeno, !Link) = NONE then raise UnDeclaredVariableException else valOf(find(Symbols,a,scopeno, !Link))
| make_rat(a,b) => ((Rational.toDecimal(valOf(Rational.make_rat(BigInt.fromString(#1(evaluateExpression(a,scopeno))),BigInt.fromString(#1(evaluateExpression(b,scopeno)))))),Rational))
| fromDecimal(a) => (#1(evaluateExpression(a,scopeno)),Rational)
| INVERSE(a) => ((Rational.toDecimal(valOf(Rational.inverse(Rational.fromDecimal(#1(evaluateExpression(a,scopeno))))))),Rational)
| RATFUN(a) => ((Rational.toDecimal((Rational.rat(BigInt.fromString(#1(evaluateExpression(a,scopeno))))))),Rational)
| TT => ("tt",Bool)
| FF => ("ff",Bool)
| Rational_numeral(a) => (a,Rational)




fun evaluateProcDef(PROCDEF(c,d),scopeno, scopelist)= 
let
fun evaluateProcDeflist([],scopeno, scopelist)= 0
| evaluateProcDeflist(x::xs,scopeno, scopelist) = 
let 
val _ =scope  := !scope +1;
val j = evaluateProcDef(x,!scope, !scope:: (scopelist))  
in evaluateProcDeflist(xs, !scope, (scopelist))  end

fun evaluateRatDec(RATDEC(m,n),scopeno)= insertSymbol(Symbols,n,m,scopeno)
fun evaluateBoolDec(BOOLDEC(m,n),scopeno)= insertSymbol(Symbols,n,m,scopeno)
fun evaluateIntDec(INTDEC(m,n),scopeno)= insertSymbol(Symbols,n,m,scopeno)

fun evaluateVarDec(VARDEC(r,i,b),scopeno)= 
let val rat = evaluateRatDec(r,scopeno)
val int = evaluateIntDec(i,scopeno)
val bool = evaluateBoolDec(b,scopeno)
(* val n= print("\n")
val _ =printSymbol(!Symbols)
val a= print("LOOOOOOK") *)
in Symbols
end

fun evaluateDec(DECSEQ(c,d),scopeno, scopelist)=
let val h = evaluateVarDec(c,scopeno)
val k = evaluateProcDeflist(d,scopeno, scopelist)
in h
end

val (BLK(v,w))=d
val _ =insertProc(Procedures,c,w,scopelist);
val x = evaluateDec(v,scopeno,scopelist)
in
1
end


fun evaluateProcDeflist([],scopeno, scopelist)= 0
| evaluateProcDeflist(x::xs,scopeno, scopelist) = 
let 
val _ =scope  := !scope +1;
val j = evaluateProcDef(x,!scope, !scope::scopelist)  
in evaluateProcDeflist(xs, !scope, scopelist)  end

fun evaluateRatDec(RATDEC(m,n),scopeno)= insertSymbol(Symbols,n,m,scopeno)
fun evaluateBoolDec(BOOLDEC(m,n),scopeno)= insertSymbol(Symbols,n,m,scopeno)
fun evaluateIntDec(INTDEC(m,n),scopeno)= insertSymbol(Symbols,n,m,scopeno)

fun evaluateVarDec(VARDEC(r,i,b),scopeno)= 
let val rat = evaluateRatDec(r,scopeno)
val int = evaluateIntDec(i,scopeno)
val bool = evaluateBoolDec(b,scopeno)
(* val n= print("\n")
val _ =printSymbol(!Symbols)
val a= print("LOOOOOOK") *)
in Symbols
end

fun evaluateDec(DECSEQ(c,d),scopeno,scopelist)=
let val h = evaluateVarDec(c,scopeno)
val k = evaluateProcDeflist(d,scopeno,scopelist)
in h
end

fun evaluateCmd(a,scopeno)=
case a of WH(c,d) => 
        let fun evaluateCmdlist(x,scopeno)=
        let
        fun evaluateCmdlistrev([],scopeno)= ()
        | evaluateCmdlistrev(x::xs,scopeno)= let val k=evaluateCmdlistrev(xs,scopeno) val j =evaluateCmd(x, scopeno) in j end
        in evaluateCmdlistrev(List.rev(x),scopeno)
        end 
        in 
        if (#1(evaluateExpression(c,scopeno)))="tt" then let val _ =evaluateCmdlist(d,scopeno) in evaluateCmd(a,scopeno) end
        else ()
        end
|SET(c,d) => update(Symbols,c,#1 (evaluateExpression(d,scopeno)),#2 (evaluateExpression(d,scopeno)),scopeno,!Link)
|ITE(c,d,e) => let fun evaluateCmdlist(x,scopeno)=
        let
        fun evaluateCmdlistrev([],scopeno)= ()
        | evaluateCmdlistrev(x::xs,scopeno)= let val k=evaluateCmdlistrev(xs,scopeno) val j =evaluateCmd(x, scopeno) in j end
        in evaluateCmdlistrev(List.rev(x),scopeno)
        end in if (#1(evaluateExpression(c,scopeno)))="tt" then evaluateCmdlist(d,scopeno) else evaluateCmdlist(e,scopeno) end
|Call(d) => let fun evaluateCmdlist(x,scopeno)=
        let
        fun evaluateCmdlistrev([],scopeno)= ()
        | evaluateCmdlistrev(x::xs,scopeno)= let val k=evaluateCmdlistrev(xs,scopeno) val j =evaluateCmd(x, scopeno) in j end
        in evaluateCmdlistrev(List.rev(x),scopeno)
        end
        (* val k = printProc(!Procedures)
        val _ = printSymbol(!Symbols) *)
        val SOME (a,b,c) = findProc(Procedures,d)
        val old = !Link;
        val j= Link := (c);
        val _ = evaluateCmdlist(b,hd c)
        val _= Link := (old);
        in ()
        end
|Print(d) => let val _ = print(#1(evaluateExpression(d,scopeno))^ "\n") val _ = Out := !Out @ [#1(evaluateExpression(d,scopeno))^ "\n"] in () end
|Read(d) => let val k = InputCommandLine("Enter the value of " ^d) in 
update(Symbols,d,k,(#2 (valOf(find(Symbols, d, scopeno, !Link)))),scopeno,!Link) end

fun evaluateCmdlist(x,scopeno)=
let
fun evaluateCmdlistrev([],scopeno)= ()
| evaluateCmdlistrev(x::xs,scopeno)= let val k=evaluateCmdlistrev(xs,scopeno) val j =evaluateCmd(x, scopeno) in j end
in evaluateCmdlistrev(List.rev(x),scopeno)
end

fun evaluateBlk(BLK(d,c),scopeno)=
let val u = evaluateDec (d, scopeno, [0])
val h = evaluateCmdlist (c,scopeno)
    
in h
end


fun evaluateProg(PROG(h),scopeno,outputFile) = 
let
    val i = clearSymbol(Symbols)
    val j = clearProc(Procedures)
    val _ = scope := !scope - !scope;
    val _ = Link := [0];
    val _ = Out := [];
    val k=evaluateBlk(h,scopeno)
    val j = OutParse(!Out, outputFile)
    (* val _ =printSymbol(!Symbols) *)
in (Symbols, Procedures,Link)
end
end ; 


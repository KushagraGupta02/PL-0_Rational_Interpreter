(* while_ast.sml *)
structure My =
    struct
    exception MyError;
    fun make_AST (fileName) =
    let
        val inStream = TextIO.openIn fileName;
        val grab : int -> string = fn
        n => if TextIO.endOfStream inStream
            then ""
            else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn
        (msg,line,col) =>
        print (fileName^"["^Int.toString line^":"
        ^Int.toString col^"] "^msg^"\n");
        val (tree,rem) = MyParser.parse
        (15,
        (MyParser.makeLexer grab ),
        printError,
        ()) 
        handle MyParser.ParseError => raise MyError ; 
        (* Close the source program file *)
        val _ = TextIO.closeIn inStream;

    in 
        (tree,rem)
    end

    fun interpret(inputFile,outputFile) =
    let 
    open DataTypes;
    val x = make_AST inputFile;
    in
    evaluateProg(#1 x, 0, outputFile)
    end

end;


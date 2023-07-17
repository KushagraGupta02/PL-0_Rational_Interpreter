signature BIGINT =
  sig
  type bigint = bool * int list
  val fromString: string -> bigint
  val toString: bigint -> string
  val add:  bigint * bigint -> bigint
  val sub:  bigint * bigint -> bigint
  val mult:  bigint * bigint -> bigint
  val gcd:  bigint * bigint -> bigint
  val lcm:  bigint * bigint -> bigint
  val divide : int list * int list * int list * int list -> int list
  val remainder : int list * int list * int list * int list -> int list
  val less :bigint * bigint -> bool
  val equal : bigint * bigint -> bool
end

structure BigInt:BIGINT=
  struct
    type bigint = bool * int list
    fun fromInt n=
    let
    fun fromInti n = if n < 0 then raise Fail "fromInt: negative argument"
                    else if n = 0 then [0]
                    else let
                           val r = n mod 10
                           val q = n div 10
                         in
                           if q = 0 then [r]
                           else (r :: fromInti q)
                         end
        in
        if n<0 then
        (true,List.rev(fromInti (~ n)))
        else 
        (false,List.rev(fromInti n))
    end 

    fun fromString (n)=
    let fun convert(x: char list)=
        if null x then []
        else 
        let 
        fun anew(x:char list)=
          let val gh = hd x
          in Char.ord(gh)-Char.ord #"0" end
        val gh = anew(x)
        in 
        if 0<=gh andalso gh<=9 then
        gh :: convert(tl x)
        else raise Fail "not an integer" end
        val he =explode(n)
      in
        if hd he = #"+" then (false,convert(tl he))
        else if hd he= #"~" then (true,convert(tl he))
        else (false,convert(he))
      end
    fun toInt a = 
    if a=(false,[]) then 0 else
    let
        val (b,l)=a
        fun toInti [] = 0| toInti(x::xs) = x + 10 * toInti xs
        in 
        if b then ~(toInti(List.rev(l)))
        else toInti(List.rev(l))
        end;

    fun toString(n : bigint)=
    let
    fun toStringi [] = ""
      | toStringi (x::xs) = Int.toString x ^ toStringi xs
    val (sign,list)=n
    in
    if sign then "~"^toStringi((list))
    else 
       toStringi((list))
    end
    fun addi(xs:int list, y:int)=
    let 
    fun addd(xs:int list, y:int, c:int)=
    if null xs then
        if c=0 
        then [] 
        else c::[]
    else
    let 
    val j = hd xs + y + c
    in 
    j mod 10 :: addd(tl xs, 0, j div 10)
    end
    val new = List.rev(xs)
    in
    List.rev(addd(new,y,0))
    end

    fun zero_rem(a : int list)=
                if a = [] then [0]
                else if hd a = 0 then zero_rem(tl a)
                else a
                
    fun adda (a: int list, b: int list) =
        let
            fun carry (0, []) = []
              | carry (c, []) = [c]
              | carry (c, x::xs) = 
                let val s = x + c 
                in (s mod 10) :: carry (s div 10, xs) 
                end

            fun fulladd (0, [], []) = []
              | fulladd (c, [], []) = [c]
              | fulladd (c, [], y::ys) = carry (c, y::ys)
              | fulladd (c, x::xs, []) = carry (c, x::xs)
              | fulladd (c, x::xs, y::ys) = 
              let val s = x + y + c 
              in (s mod 10) :: fulladd (s div 10, xs, ys) end

            val result = fulladd (0, List.rev(a), List.rev(b))
        in
            zero_rem(List.rev(result))
        end


    fun suba(a : int list, b : int list)=
        let 
        fun zero_rem(a : int list)=
        if a = [] then
        [0]
        else 
        if hd a = 0 then
        zero_rem(tl a)
        else
        a
        fun subi (a : int list, b : int list) =
            let
            fun pad_list (lst : int list, n : int) =
                    if n = 0 then lst
                    else pad_list (0::lst, n-1)
                fun subi_lists (a : int list, b : int list, carry : int) = 
                    if a = [] andalso b = [] then
                        if carry = 0 then [] else [carry]
                    else
                        let
                            val a_digit = if a = [] then 0 else hd a
                            val b_digit = if b = [] then 0 else hd b
                            val c_digit = if carry = 0 then 0 else 1
                            val diff = a_digit - b_digit - c_digit
                            val new_carry = if diff < 0 then 1 else 0
                            val new_diff = if diff < 0 then diff + 10 else diff
                        in
                            new_diff :: subi_lists (tl a, tl b, new_carry)
                        end
                val anew=List.rev(a)
                (* val bal=List.rev(b) *)
            in
                let
                    val len_diff = length a - length b
                    val bal = if len_diff > 0 then pad_list(b, len_diff) else b
                in
                let
                val bnew=List.rev(bal)
                in
                let 
                val hey=subi_lists (anew, bnew, 0)
                in
                List.rev(hey)
                end
            end
        end
        end
    val var = subi(a :int list, b : int list)
    val ne = zero_rem(var)
    in 
    ne
    end


fun larger(a : int list, b : int list)=
        if null a andalso null b then 
            false
        else 
        if length a = length b
        then
        if hd a = hd b then
            larger(tl a, tl b)
        else 
            if hd a < hd b then
            false
            else
                true
        else 
        if length a > length b
        then true
        else
        false

  fun add(e: bigint, f:bigint)=
    let
    val (a,c)=e
    val (b,d)=f
    in
    if (a andalso b) orelse (not a andalso not b) then
    (a,adda(c,d))
    else 
    if larger(c,d) then
    (a,suba(c,d))
    else
    (b,suba(d,c))
  end
  fun sub(e: bigint, f:bigint)=
  let
  fun subbb(e: bigint, f:bigint)=
    let
    val (a,c)=e
    val (b,d)=f
    in
    if not ((a andalso b) orelse (not a andalso not b)) then
    (a,adda(c,d))
    else 
    if larger(c,d) then
    (a,suba(c,d))
    else
    (not b,suba(d,c))
    end
    val (g,h)=subbb(e,f)
    in
    if h=[0] then (false,[0])
    else (g,h)
    end
  fun less(a,b)=
  let val ((c,d)) = sub(a,b)
  in c
  end
  fun equal(a,b)=
  let val ((c,d)) = sub(a,b)
  in 
  if not c andalso d=[0] then true else false end
fun multi(xs: int list, y:int, c:int)=
let
fun multii(xs: int list, y:int, c:int)=
    if null xs then
      if c=0 
        then [] 
      else c::[]
    else
    let 
    val j = (hd xs) * y + c
    in 
    j mod 10 :: multii(tl xs, y, j div 10)
    end 
    in List.rev(multii(List.rev(xs), y, c))
    end

fun mult(a:bigint, b:bigint)=
let
val (c,xs)=a
val (d,y)=b
fun multt(xs:int list, y:int list, acc:int list)=
if y=[] then 
                acc
                else
let 
  val new = (xs)
  val hey =multi(new,hd y,0)
  val hoo =zero_rem(hey)
  in
                (multt(xs, tl y, adda(hey,acc)@[0]))
                end
    val mul=List.rev(multt(xs,y,[0]))
    val hh=tl mul
in
  if (c andalso d) orelse (not c andalso not d) then
    (false,List.rev(hh))
  else 
  (true,List.rev(hh))
    end

fun check (a : int list, dive : int list) =
let val neww0 = multi(dive,1,0)
in
if larger(neww0,a) then 0
else
  let val neww1 = multi(dive,2,0)
    in 
    if larger(neww1,a) then 1
    else
  let val neww2 = multi(dive,3,0)
    in 
    if larger(neww2,a) then 2
    else
  let val neww3 = multi(dive,4,0)
    in 
    if larger(neww3,a) then 3
    else
  let 
    val neww4 = multi(dive,5,0)
    in 
    if larger(neww4,a) then 4
   else
  let 
    val neww5 = multi(dive,6,0)
    in 
    if larger(neww5,a) then 5
   else
  let 
    val neww6 = multi(dive,7,0)
    in 
    if larger(neww6,a) then 6
   else
  let 
    val neww7 = multi(dive,8,0)
    in 
    if larger(neww7,a) then 7
   else
  let 
    val neww8 = multi(dive,9,0)
    in 
    if larger(neww8,a) then 8
  else 9
  end
  end
  end
  end
  end
  end
  end
  end
  end
  
 fun divide(a : int list, b : int list, quo : int list, rem : int list)= 
    if zero_rem(b)=[0] then raise Fail "Division by zero"
    else
    if null a then
    quo
    else
    let 
        val t = hd a
        val ai = tl a
        val remn = zero_rem(rem @ [t])
        val imp = check (remn, b) 
        val quot =  zero_rem(quo@ [imp])
        val heee =zero_rem(multi(b,imp,0))
        val rere = suba(remn,heee)
        in  
        divide(ai, b, quot, rere)
        end

fun remainder(a : int list, b : int list, quo : int list, rem : int list)= 
    if zero_rem(b)=[0] then raise Fail "Division by zero"
    else
    if null a then
    rem
    else
    let 
        val t = hd a
        val ai = tl a
        val remn = zero_rem(rem @ [t])
        val imp = check (remn, b) 
        val quot =  zero_rem(quo@ [imp])
        val heee =zero_rem(multi(b,imp,0))
        val rere = suba(remn,heee)
        in  
        remainder(ai, b, quot, rere)
        end

fun gcd(e:bigint, f:bigint)=
let
val (a,c)=e
    val (b,d)=f
fun  gcda([0], m) = m
| gcda(n, m) = gcda(remainder(m,n,[0],[0]), n) 
in
(false,gcda(zero_rem(c),zero_rem(d)))
end

fun lcm(e:bigint, f:bigint)= 
let
val (a,c)=e
    val (b,d)=f
    val (g,h)=mult(e,f)
    val (j,i)=gcd(e,f)
in    
(false,divide(h,i,[0],[0]))
end
end

signature RATIONAL =
sig
type rational= BigInt.bigint * BigInt.bigint 
exception rat_error
val rat: BigInt.bigint -> rational
val reci: BigInt.bigint -> rational option
val make_rat: BigInt.bigint * BigInt.bigint -> rational option
val add : rational * rational -> rational
val subtract : rational * rational -> rational
val multiply : rational * rational -> rational
val divide: rational * rational -> rational option
val inverse : rational -> rational option
val neg: rational -> rational
val showRat : rational -> string
val showDecimal : rational -> string
val fromDecimal : string -> rational
val equal : rational * rational -> bool (* equality *)
val less : rational * rational -> bool (* less than *)
val toDecimal : rational -> string
end;

functor Rationalize (BigInt: BIGINT) :
RATIONAL
=
struct
    type rational= BigInt.bigint * BigInt.bigint  
    exception rat_error

    fun make_rat(a:BigInt.bigint, b:BigInt.bigint) =
        let
            val (c,n)=a
            val (e,d)=b
            val (h,g) = BigInt.gcd(a, b)
        in
          if BigInt.divide(d, g, [0],[0])=[0] then NONE
          else
          if (c andalso e) orelse (not c andalso not e)then
            SOME ((false,BigInt.divide(n, g, [0], [0])), (false,BigInt.divide(d, g, [0],[0])))
          else
            SOME ((true,BigInt.divide(n, g, [0], [0])), (false,BigInt.divide(d, g, [0],[0])))
        end

    fun add(a : rational ,b : rational)=
          let
            val (c,n)=a
            val (e,d)=b
            val new=BigInt.mult(n,d)
            val ne=BigInt.mult(c,d)
            val me=BigInt.mult(n,e)
            val he = valOf (make_rat(BigInt.add(me,ne),new))
          in
          he
        end
    (* fun simplify (n, d) = make_rat (n, d) *)
    fun  subtract(a : rational ,b : rational)=
          let
            val (c,n)=a
            val (e,d)=b
            val new=BigInt.mult(n,d)
            val ne=BigInt.mult(c,d)
            val me=BigInt.mult(n,e)
            val ans = valOf (make_rat(BigInt.sub(ne,me),new))
          in
          ans
        end
    fun multiply(a : rational ,b : rational)=
          let
            val (c,n)=a
            val (e,d)=b
            val new=BigInt.mult(n,d)
            val ne=BigInt.mult(c,e)
            val ans = valOf (make_rat(ne,new))
          in
          ans
        end
    fun divide(a : rational ,b : rational)=
          let
            val (c,n)=a
            val (e,d)=b
            val new=BigInt.mult(n,e)
            val ne=BigInt.mult(c,d)
          in
          make_rat(ne,new)
        end
    fun inverse(a:rational)=
          divide(((false,[1]),(false,[1])),a)
    fun neg(a:rational)=
        let
            val (c,n)=a
            val (d,e)=c
            val ne=(not d,e)
            val ans = valOf (make_rat(ne,n))
        in
          ans
        end
    fun rat(a:BigInt.bigint)=
    let val ans=
      valOf (make_rat(a,(false,[1])))
      in ans end
    fun reci(a)=
    make_rat((false,[1]),a)
    
    fun showRat((a:BigInt.bigint,b:BigInt.bigint))=
    BigInt.toString(a)^"/"^BigInt.toString(b)

    fun showDecimal(((a,b),(c,d)):rational)=
    let 
      fun findIndex (x : int list, []) = NONE
      | findIndex (x : int list, y::ys : int list list) =
      if x = y then SOME 0
      else case findIndex (x, ys) of
            NONE => NONE
          | SOME index => SOME (index + 1)
      val quo=BigInt.divide(b,d,[0],[0])
      val rem=BigInt.remainder(b,d,[0],[0])
      val str=BigInt.toString(a,quo)
      fun toStringi [] = ""
        | toStringi (x::xs) = Int.toString x ^ toStringi xs

    
      fun showDe((b : int list, rem : int list, rlist:int list list,qu: int list))=
        let
        val new=BigInt.divide(rem@[0],b,[0],[0])
        val neww=BigInt.remainder(rem@[0],b,[0],[0])
        val total=toStringi(qu@new)
        in
        if neww=[0] then (total,"0")
        else
        if findIndex(neww@[0],rlist)=NONE then
        showDe(b:int list, neww,rlist@[neww@[0]],qu@new)
        else
        let val i = valOf (findIndex(neww@[0],rlist))
        val total=toStringi(qu@new)
        val first=String.substring(total,0,i)
        val second=String.substring(total,i,String.size total-i)
        in
        (first,second)
        end

      end
      val (first,second)=showDe(d,rem,[rem@[0]],[])
      in
      if rem=[0] then str^".(0)" else
      (str^"."^first^"("^second^")")
      end

fun fromDecimal(s: string)=
let val he = explode(s)
    fun helper(c : char list, toggle1 : bool, toggle2: bool, toggle3: bool, d : string list, e: string)=
    if toggle1 andalso toggle2 andalso toggle3 then
      if c=[] then d
      else raise rat_error
    else
      if c=[] then raise rat_error else
      if hd c = #"." then 
        if not toggle1 andalso not toggle2 andalso not toggle3 then
          if e="" then helper(tl c, true, false, false, "0"::d,"")
          else helper(tl c, true, false, false, e::d,"")
        else raise rat_error
      else if hd c = #"(" then
        if toggle1 andalso not toggle2 andalso not toggle3 then
          if e = "" then helper(tl c, true, true, false, ""::d,"")
          else helper(tl c, true, true, false, e::d,"")
        else raise rat_error
      else if hd c = #")" then
        if toggle1 andalso toggle2 andalso not toggle3 then
          if e = "" then raise rat_error
          else helper(tl c, true, true, true, e::d,"")
        else raise rat_error
      else if (0 <= Char.ord(hd c)-Char.ord #"0" andalso Char.ord(hd c)-Char.ord #"0" <= 9) then
          helper(tl c, toggle1, toggle2, toggle3, d,e^ String.str (hd c))
      else raise rat_error
    
    fun convert(x: char list)=
    if null x then []
    else 
    let 
    fun anew(x:char list)=
      let val gh = hd x
      in Char.ord(gh)-Char.ord #"0" end
    val gh = anew(x)
    in gh :: convert(tl x) end
    fun zero_rem(a : int list)=
        if a = [] then
        [0]
        else 
        if hd a = 0 then
        zero_rem(tl a)
        else
        a
    fun num (a, b, c) = 
      if b = 0 then a else c:: num (a, b-1,c)
    in
    if hd he = #"~" orelse hd he = #"+"  then 
      let
      val split = List.rev(helper(tl he, false, false, false, [], ""))
      val h = zero_rem(convert(explode(hd split ^ hd (tl split))))
      val k = zero_rem(convert(explode(hd (tl (tl split)))))
      val g = num([],length (explode(hd(tl split))),0)
      val f = num([],length k,9)
      val rat = valOf (make_rat((false,h),(false,1::g)))
      val ratnew= valOf (make_rat((false,k),(false,f@g)))
      in
        if hd he = #"~" then neg(add(rat,ratnew))
        else add(rat,ratnew) end
    else 
      let
    val split = List.rev(helper(he, false, false, false, [], ""))
      val h = (convert(explode(hd split ^ hd (tl split))))
      val k = (convert(explode(hd (tl (tl split)))))
      val g = num([],length (explode(hd(tl split))),0)
      val f = num([],length k,9)
      val rat = valOf (make_rat((false,h),(false,1::g)))
      val ratnew = valOf (make_rat((false,k),(false,f@g)))
      in
        add(rat,ratnew)
      end
  end
  fun toDecimal(a:rational)=showDecimal(a)
  fun equal (a,b)=make_rat(a)=make_rat(b)
  fun less(a,b)=
  let val ((c,d),(e,f)) = subtract(a,b)
  in c
  end
end;
structure Rational = Rationalize(BigInt)
val a = BigInt.less ((false,[3]),(false,[2]))
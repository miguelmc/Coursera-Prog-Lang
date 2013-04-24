(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* was i supposed to implement this? *)
fun filter(f,xs) =
    case xs of
	[] => []
      | x::xs => if f x
		  then x::(filter (f,xs))
		  else filter (f,xs)
(* p1 *)
fun only_capitals(strings : string list) =
    List.filter (fn s => (Char.isUpper o String.sub) (s,0)) strings

val p1 = only_capitals(["Hola", "hola", "Jaja", "jaja"])


(* p2 *)
fun longest_string1 strings =
    List.foldl (fn (s,acc) => if(String.size s > String.size acc) then s else acc) "" strings

(* p3 *)
fun longest_string2 strings =
    List.foldl (fn (s,acc) => if(String.size s >= String.size acc) then s else acc)  ""  strings

val p2 = longest_string1(["extremadamente", "hola", "a", "", "jaja", "extremamamente"])
val p3 = longest_string2(["extremadamente", "hola", "a", "", "jaja", "extremamamente"])


(* p4 *)
fun longest_string_helper f strings =
    foldl f "" strings

val longest_string3 = longest_string_helper (fn (s,acc) => if(String.size s > String.size acc) then s else acc)

val longest_string4 = longest_string_helper (fn (s,acc) => if(String.size s >= String.size acc) then s else acc)

(* p5 *)
val longest_capitalized = foldl (fn (s,acc) => 
				   if(String.size s > String.size acc) 
				   then if((Char.isUpper o String.sub) (s,0))
					then
					    s
					else
					    acc
				   else
				       acc) ""

(* p6 *)
fun rev_string s =
    (String.implode o List.rev o String.explode) s

(* p7 *)
fun first_answer f lista =
    case lista of
	[] => raise NoAnswer
      | x::xs => case f x of
		     NONE => first_answer f xs
		   | SOME v => v

(* p8 
probably a little different than intended *) 
fun all_answers f lista =
    case lista of
	[] => SOME []
      | x::xs => case f x of
		     (* if fx = NONE exits immediately *)
		     NONE => NONE
		     (* checks if rest of list has a NONE in it and avoids crash *)
		   | SOME i => case (all_answers f xs) of
				   NONE => NONE
				 | SOME j => SOME (i@j)


datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu


fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(* p9 *)
fun count_wildcards p = 
    g (fn () => 1) (fn s => 0) p

fun count_wild_and_variable_lengths p =
    g (fn() => 1) (fn s => String.size s) p 

fun count_some_var (str, p) =
    g (fn() => 0) (fn s => if(s=str)then 1 else 0) p


(* p10 *)
fun check_pat p =
    let
	fun makeList patron =
	    case patron of
		Variable s => [s]
	      | TupleP ps => List.foldl (fn(i,acc) => (makeList i) @ acc) [] ps
	      | _ => []

	fun check lista =
	    case lista of
		[] => true
	      | x::xs => if((List.exists(fn y => if(x = y)
						 then true 
						 else false) xs)) 
			 then 
			     false
			 else
			     true andalso check xs
    in
	(check o makeList) p
end



fun match (v,p) =
    case p of
	Wildcard => SOME[]
      | Variable s => SOME[(s,v)]
      | UnitP => (case v of
		     Unit => SOME[]
		   | _ => NONE)
      | ConstP n => (case v of
			 Const m => if(n = m)then SOME [] else NONE
		      | _ => NONE)
      | TupleP ps => (case v of
			  Tuple vs =>  if(List.length ps = List.length vs)
				       then
					   all_answers match (ListPair.zip(vs,ps))
				       else
					   NONE
			| _ => NONE)
      | ConstructorP(s1,pe) => (case v of
				    Constructor(s2,ve) => if(s1=s2)then match(ve,pe) else NONE
				  | _ => NONE)

fun first_match v ps =
    let
	val x = (first_answer (fn p => match(v,p)) ps) handle NoAnswer => []
    in
    case x of
	[] => SOME[]
      | _ => SOME x
    end

(*fun mystery f xs =
    let
        fun g xs =
           case xs of
             [] => NONE
           | x::xs' => if f x then SOME x else g xs'
    in
	case xs of
            [] => NONE
	  | x::xs' => if f x then g xs' else mystery f xs'
    end
*)
fun mystery f = fn xs =>
    let
        fun g xs =
           case xs of
             [] => NONE
           | x::xs' => if f x then SOME x else g xs'
    in
	case xs of
            [] => NONE
	  | x::xs' => if f x then g xs' else mystery f xs'
    end


signature COUNTER =
sig
    type t
    val newCounter : int -> int
    val increment : t -> t
    val first_larger : t * t -> bool
end


structure NoNegativeCounter :> COUNTER = 
struct

exception InvariantViolated

type t = int

fun newCounter i = if i <= 0 then 1 else i

fun increment i = i + 1

fun first_larger (i1,i2) =
    if i1 <= 0 orelse i2 <= 0
    then raise InvariantViolated
    else (i1 - i2) > 0

end


fun null xs = case xs of [] => true | _ => false

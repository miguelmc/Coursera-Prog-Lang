(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(s, strings) =
    case strings of
	[] => NONE
      | a::b => if(same_string(a,s))
		then SOME b
		else 
		    let
			val tail = all_except_option(s,b)
		    in
			case tail of
			    NONE => NONE
			  | SOME i => SOME (a::i)
		    end
    
fun get_substitutions1(substitutions, s) = 
    case substitutions of
	[] => []
      | list::lists => case all_except_option(s,list) of
			   NONE => [] @ get_substitutions1(lists,s)
			 | SOME i => i @ get_substitutions1(lists,s)

fun get_substitutions2(substitutions, s) =
    let
	fun aux(subs, acc) =
	    case subs of
		[] => acc
	      | list::lists => case all_except_option(s,list) of
				   NONE => aux(lists, acc @ [])
				 | SOME i => aux(lists, acc @ i)
    in
	aux(substitutions, [])
    end

fun similar_names(substitutions,{first=f, middle=m ,last=l}) = 
    let
	val subs = get_substitutions1(substitutions, f)
	fun getNames(s : string list) =
	    case s of
		    [] => []
		  | a::b => {first=a, middle=m, last=l}::getNames(b)
    in
	{first=f,middle=m,last=l} :: getNames(subs)
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(s, r) =
    case s of
	Clubs => Black
      | Spades => Black
      | _ => Red

fun card_value(s, r) =
    case r of
	Ace => 11
      | Num n => n
      | _ => 10

fun remove_card(cs, c, e) =
    case cs of
	[] => raise e	
      | head::tail => if(head = c)
		      then tail
		      else head::remove_card(tail, c, e)

fun all_same_color(cs) =
    case cs of
	[] => true
      | _::[] => true
      | c1::c2::[] => card_color c1 = card_color c2
      | c1::c2::tail => if(card_color c1 = card_color c2)
			then true andalso all_same_color(c2::tail)
			else false

fun sum_cards(cs) = 
    let
	fun aux(cards, acc) =
	    case cards of
		[] => acc
	      | c::tail => aux(tail, acc+card_value c)
    in
	aux(cs, 0)
    end

fun score(held, goal) =
    let
	val sum = sum_cards(held)
	val same = all_same_color(held)
    in
	if(sum>goal)
	then
	    if same
	    then
		(3* (sum- goal)) div 2
	    else
		3* (sum- goal)
	else 
	    if(same)
	    then
		(goal - sum) div 2
	    else 
		goal - sum
    end

fun officiate(card_list, move_list, goal) =
    let
	fun turn(cs,mv,hand) =
	    (* achieved goal *)
	    if(sum_cards(hand)>goal) 
	    then score(hand, goal)
	    else
		case mv of
		    (* no more moves *)
		    [] => score(hand, goal)
		  | (Discard c)::left => turn(cs, left, remove_card(hand, c, IllegalMove))
		  | Draw::left => case cs of
				      (* no more cards left to draw *)
				      [] => score(hand, goal)
				    | top::rest => turn(rest, left, top::hand)
    in
	turn(card_list, move_list, [])
    end


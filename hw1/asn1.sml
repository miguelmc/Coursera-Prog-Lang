(* Was this supposed to be made by recursion? whaa? *)
fun is_older(pr1 : int*int*int, pr2 : int*int*int) = 
    (* Compares the years first, if true, there is no need to keep 
       comparing; first date is already older. *)
    if(#1 pr1 < #1 pr2) 
    then true
    else
	(* Same as previous but with the months. *)
	if(#1 pr1 = #1 pr2 andalso #2 pr1 < #2 pr2)
	then true
	else
	    (* Finally checks if first day is older because of the day. 
                If not, it is knows that the second date comes before. *)
	    if(#2 pr1 = #2 pr2 andalso #3 pr1 < #3 pr2)
	    then true
	    else false

(* returns # of dates that have a certian month *)
fun number_in_month(dates : (int*int*int) list, month : int) =
    (* makes sure dates is not empty *)
    if(null dates)
    then 0
    else 
	(* checks if head of dates is equal to month, if it is it calls 
        itself and adds one, if not, it just calls itself and ignores the head *)
	if((#2 (hd dates)) = month)
	then 1 + number_in_month(tl dates, month)
	else number_in_month(tl dates, month)

(* note that delimiter is no longer the dates list, but the months list *)
fun number_in_months(dates : (int*int*int) list, months : int list) =
    if(null months)
    then 0
    (* takes advantage of last method on the left side of the left 
    (one can assume is a number) and recursively calls itself with 
    the tail of the months list *)
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int*int*int) list, month : int) =
    if(null dates)
    (* initializes alpha list *)
    then []
    else 
	if(#2(hd dates) = month)
	(* in both situations the function must call itself, but 
           only adds a date if it is of the month searched for *)
	then hd dates :: dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)

(* asuming months dont repeat; tragic if it does *)
fun dates_in_months(dates : (int*int*int) list, months : int list) =
    if(null months)
    then []
    (* same note than number_in_months *)
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* repeats until element you are looking for is on the head (element 1) *)
fun get_nth(strings : string list, n : int) =
    if(n = 1)
    then hd strings
    else get_nth(tl strings, n-1)

fun date_to_string(date : (int*int*int)) =
    let
	(* local variable with months *)
	val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

(* will crash if sum > sum of al list 
     will return 0 if sum is already lower than the first element*)
fun number_before_reaching_sum(sum : int, lista : int list) =
    let
	(* to avoid having 2 substraction statements *)
	val new_sum = sum - hd lista
    in
	if(new_sum < 1)
	then 0
	else 1 + number_before_reaching_sum(new_sum, tl lista)
    end

(* no leap years *)
fun what_month(day : int)  =
    let
	val months = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	(* adds one since range is 0-11 and should be 1-12 *)
	1 + number_before_reaching_sum(day, months)
    end

(* I hope this was it.. *)
fun month_range(day1 : int, day2 : int) =
    if(day1 > day2)
    then []
    else what_month(day1) :: month_range(day1+1, day2)


fun oldest(dates  : (int*int*int) list) =
    if null dates
    then NONE
    else
	let val tl_ans = oldest(tl dates)
	in if(isSome tl_ans andalso is_older(valOf tl_ans, hd dates))
	   then tl_ans
	   else SOME(hd dates)
	end


(* sloppy, but it works..? *)
fun reasonable_date(date : (int*int*int)) =
    let
	val months = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	let
	    (* like string function but with ints *)
	    fun get_nth_int(ints : int list, n : int) =
		if(n = 1)
		then hd ints
		else get_nth_int(tl ints, n-1)

	in
	    (* making sure all looks okay *)
	    if(#1 date >= 1 andalso #2 date >= 1 andalso #2 date <= 12 andalso #3 date >= 1)
	    then if(#2 date = 2 andalso #3 date = 29)
		 (* leap year case *)
		 then
		     (* mod was not seen in class, but i found no other logical way *)
		     if(#1 date mod 400 = 0 orelse (#1 date mod 4 = 0 andalso #1 date mod 100 <> 0))
		     then true
		     else false
		 (* any other case *)
		 else 
		     if(#3 date <= get_nth_int(months, #2 date))
		     then true
		     else false
	    (* not even close *)		      
	    else
		false
	end
    end

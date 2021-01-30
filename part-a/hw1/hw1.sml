(* "dates" have type int*int*int = year/month/day *)

fun is_older(d1 : int*int*int, d2 : int*int*int) =
    if (#1 d1) < (#1 d2)
    then true
    else if (#1 d2) < (#1 d1)
    then false
    else
	if (#2 d1) < (#2 d2)
	then true
	else if (#2 d2) < (#2 d1)
	then false
	else
	    (#3 d1) < (#3 d2)


fun number_in_month(x : (int*int*int) list, month : int) =
    if null x
    then 0
    else
	if (#2 (hd x)) = month
	then 1 + number_in_month(tl x, month)
	else
	    number_in_month(tl x, month)


fun number_in_months(x : (int*int*int) list, months : int list) =
    if null months
    then 0
    else
	number_in_month(x, hd months) + number_in_months(x, tl months)


fun dates_in_month(x : (int*int*int) list, month : int) =
    if null x
    then []
    else
	if (#2 (hd x)) = month
	then (hd x)::dates_in_month(tl x, month)
	else
	    dates_in_month(tl x, month)


fun dates_in_months(x : (int*int*int) list, months : int list) =
    if null months
    then []
    else
	dates_in_month(x, hd months) @ dates_in_months(x, tl months)


fun get_nth(x : string list, n : int) =
    if n = 1
    then hd x
    else
	get_nth(tl x, n - 1)


fun date_to_string(date : int*int*int) =
    let val months = ["January",
		  "February",
		  "March",
		  "April",
		  "May",
		  "June",
		  "July",
		  "August",
		  "September",
		  "October",
		  "November",
		  "December"
		 ]
    in
	get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end


fun number_before_reaching_sum(sum : int, x : int list) =
    let fun prefix_sum(x : int list, sum : int) =
	    if null x
	    then []
	    else
		(sum + (hd x))::prefix_sum(tl x, sum + (hd x))
	val psum = prefix_sum(x, 0)
	fun first_gteq(x : int list) =
	    if (hd x) >= sum
	    then 0
	    else
		1 + first_gteq(tl x)
    in
	first_gteq(psum)
    end


fun what_month(day : int) =
    let val month_days = [
	    31,
	    28,
	    31,
	    30,
	    31,
	    30,
	    31,
	    31,
	    30,
	    31,
	    30,
	    31
	]
    in
	number_before_reaching_sum(day, month_days) + 1
    end


fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else
	what_month(day1)::month_range(day1 + 1, day2)


fun oldest(x : (int*int*int) list) =
    if null x
    then NONE
    else
	let fun oldest'(x : (int*int*int) list) =
		if null (tl x)
		then hd x
		else
		    let val tl_oldest = oldest'(tl x)
		    in
			if is_older(hd x, tl_oldest)
			then hd x
			else tl_oldest
		    end
	in
	    SOME (oldest' x)
	end


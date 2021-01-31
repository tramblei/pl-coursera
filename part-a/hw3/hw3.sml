(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

val only_capitals = List.filter (fn x => (Char.isUpper (String.sub (x, 0))))

val longest_string1 = List.foldl (fn (x, max_so_far) => if String.size x > String.size max_so_far then x else max_so_far) ""

val longest_string2 = List.foldl (fn (x, max_so_far) => if String.size x >= String.size max_so_far then x else max_so_far) ""

fun longest_string_helper f = List.foldl (fn (x, max_so_far) => if f (String.size x, String.size max_so_far) then x else max_so_far) ""

val longest_string3 = longest_string_helper (fn (x,y) => x > y)

val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

val longest_capitalized = List.foldl (fn (x, max_so_far) => if String.size x > String.size max_so_far andalso (Char.isUpper o String.sub) (x, 0) then x else max_so_far) ""

val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
        | x::xs' => case f x of
			NONE => first_answer f xs'
			| SOME v => v

fun all_answers f xs =
	case xs of
		[] => SOME []
		| x::xs' => (case f x of
			NONE => NONE
			| SOME v => (case all_answers f xs' of
				NONE => NONE
				| SOME vs => SOME (v @ vs)))

val count_wildcards = g (fn () => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn () => 1) (String.size)

fun count_some_var (s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
	let fun get_variables p =
		case p of
			Variable x => [x]
			| TupleP ps => List.foldl (fn (p,i) => i @ (get_variables p)) [] ps
			| ConstructorP(_,p) => get_variables p
			| _ => []
		fun is_unique xs =
		case xs of
			[] => true
			| x::xs' => not (List.exists (fn z => x = z) xs')
	in
		is_unique (get_variables p)
	end

fun match (v, p) =
	case (v,p) of
		(_,Wildcard) => SOME []
		| (_, Variable s) => SOME [(s,v)]
		| (Unit, UnitP) => SOME []
		| (Const x1, ConstP x2) => if x1 = x2 then SOME [] else NONE
		| (Tuple vs, TupleP ps) => if List.length ps <> List.length vs then NONE else all_answers (fn (v',p') => match (v', p')) (ListPair.zip (vs, ps))
		| (Constructor (s2, v'), ConstructorP (s1, p')) => if s1 = s2 then match (v', p') else NONE
		| _ => NONE 

fun first_match v ps =
	SOME (first_answer (fn p => match (v, p)) ps)
	handle NoAnswer => NONE
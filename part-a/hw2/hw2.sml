(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

fun all_except_option (str : string, xs : string list) =
    case xs of
        [] => NONE
        | x::xs' => if same_string(str, x)
            then SOME xs'
            else case all_except_option(str, xs') of
                NONE => NONE
                | SOME ys => SOME (x::ys)

fun get_substitutions1 (xs : (string list) list, str : string) =
    case xs of
        [] => []
        | x::xs' => case all_except_option (str, x) of
            NONE => get_substitutions1 (xs', str)
            | SOME ys => ys @ get_substitutions1(xs', str)


fun get_substitutions2 (xs : (string list) list, str : string) =
    let fun helper (xs, acc) =
        case xs of
        [] => acc
        | x::xs' => case all_except_option (str, x) of
                    NONE => helper (xs', acc)
                    | SOME ys => helper(xs', ys @ acc)
    in
        helper(xs, [])
    end


fun similar_names (xs : (string list) list, {first=first, middle=middle, last=last}) =
    let val firstNames = get_substitutions1 (xs, first)
        fun helper(names) =
            case names of
                [] => []
                | name::names' => {first=name, middle=middle, last=last}::helper(names')
    in
        helper(first::firstNames)
    end


fun card_color (c : card) =
    case c of
        (Clubs, _) => Black
        | (Spades,_) => Black
        | (Hearts, _) => Red
        | (Diamonds, _) => Red


fun card_value (c : card) =
    case c of
        (_, Ace) => 11
        | (_, Num x) => x
        | _ => 10


fun remove_card (cs : card list, c : card, ex) =
    case cs of
        [] => raise ex
        | x::cs' => if c = x
            then cs'
            else x::remove_card(cs', c, ex)


fun all_same_color (cs : card list) =
    case cs of
        [] => true
        | x::[] => true
        | x::y::cs' => if card_color(x) = card_color(y)
            then all_same_color(y::cs')
            else false


fun sum_cards (cs : card list) =
    let fun aux(cs, acc) =
        case cs of
            [] => acc
            | c::cs' => aux(cs', acc + card_value(c))
    in
        aux(cs, 0)
    end


fun score (cs : card list, goal) =
    let val sum = sum_cards(cs)
        fun prelim () =
            if sum > goal
            then 3 * (sum - goal)
            else goal - sum
    in
        if all_same_color(cs)
        then prelim () div 2
        else prelim ()
    end


fun officiate (card_list, move_list, goal) =
    let fun helper(held, card_list, move_list) =
        case move_list of
            [] => score(held, goal)
            | move::move_list' => (case move of
                Draw => (case card_list of
                    [] => score(held, goal)
                    | c::card_list' => if sum_cards(c::held) > goal
                        then score(c::held, goal)
                        else helper(c::held, card_list', move_list'))
                | Discard c => helper(remove_card(held, c, IllegalMove), card_list, move_list') )
    in
        helper([], card_list, move_list)
    end

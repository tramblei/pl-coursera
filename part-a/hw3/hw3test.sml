(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1a = only_capitals ["A","b","C"] = ["A","C"]
val test1b = only_capitals ["a","b","c"] = []


val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2a = longest_string1 ["A","bc","Cabsf"] = "Cabsf"
val test2b = longest_string1 [] = ""
val test2c = longest_string1 ["ab","bc"] = "ab"


val test3 = longest_string2 ["ab","bc"] = "bc"


val test4a = longest_string3 ["Ab","bc","C"] = "Ab"

val test4b = longest_string4 ["A","B","C"] = "C"


val test5 = longest_capitalized ["A","bc","C"] = "A"
val test5a = longest_capitalized ["","bc","C"] = "C"


val test6 = rev_string "abc" = "cba"


val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4


val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8a = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3] = SOME [2,3]


val test9a = count_wildcards Wildcard = 1
val test9aa = count_wildcards (TupleP [Wildcard, UnitP, ConstP 7, Variable "ian", ConstructorP ("ian", TupleP [Wildcard, Wildcard])]) = 3

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9ba = count_wild_and_variable_lengths (TupleP [Wildcard, UnitP, ConstP 7, Variable "ian", ConstructorP ("ian", TupleP [Wildcard, Wildcard])]) = 6


val test9c = count_some_var ("x", Variable("x")) = 1
val test9ca = count_some_var ("ian", TupleP [Wildcard, UnitP, ConstP 7, Variable "ian", ConstructorP ("ian", TupleP [Wildcard, Wildcard])]) = 1


val test10 = check_pat (Variable("x")) = true
val test10a = check_pat (TupleP [Variable("x"), Variable ("x")]) = false
val test10b = check_pat (TupleP [Wildcard, UnitP, ConstP 7, Variable "ian", ConstructorP ("ian", TupleP [Wildcard, Wildcard])]) = true


val test11 = match (Const(1), UnitP) = NONE
val test11a = match (Const(1), ConstP(1)) = SOME []
val test11b = match (Const(1), ConstP(2)) = NONE
val test11c = match (Const(1), Wildcard) = SOME []
val test11d = match (Constructor ("x", Const(1)), Variable "x") = SOME [("x", Constructor ("x", Const(1)))]
val test11e = match (Tuple [Constructor("x", Const(1))], TupleP [ConstructorP("x", Variable("x"))]) = SOME [("x", Const(1))]
val test11g = match (Tuple [Constructor("x", Const(1)), Constructor("y", Const(2)), Unit], TupleP [ConstructorP("x", ConstP(1)), ConstructorP("y", Wildcard)]) = NONE



val test12 = first_match Unit [UnitP] = SOME []
val test12a = first_match Unit [ConstP(1)] = NONE
val test12b = first_match (Const(1)) [UnitP, Variable("x")] = SOME [("x", Const(1))]


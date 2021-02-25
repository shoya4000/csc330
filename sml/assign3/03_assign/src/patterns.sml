(*
Your name: Sho Ya Voorthuyzen
Your student id: V00730770
*)

structure Patterns =

struct

exception NoAnswer
exception NotFound

datatype tree = emptyTree |
                nodeTree of int * tree * tree


datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype value = Const of int
	       | Unit
	       | Tuple of value list
	       | Constructor of string * value


(* write your tree functions here *)

(* Part #1 *)
(* val tree_insert_in_order : tree * int -> tree *)
fun tree_insert_in_order(t: tree, v: int) = 
      case t of
        emptyTree => nodeTree(v, emptyTree, emptyTree)
      | nodeTree (n, left, right) => 
        if v > n then nodeTree(n, left, tree_insert_in_order(right, v))
        else nodeTree(n, tree_insert_in_order(left, v), right)

(* val tree_height : tree -> int *)
fun tree_height (t: tree) = 
      case t of
        emptyTree => 0
      | nodeTree (n, left, right) =>
        let
          val leftHeight = 1 + tree_height(left)
          val rightHeight = 1 + tree_height(right)
        in 
          if leftHeight >= rightHeight then leftHeight else rightHeight
        end

(* val tree_fold_pre_order : (int * 'a -> 'a) -> 'a -> tree -> 'a *)
fun tree_fold_pre_order f acc t =
  case t of
        emptyTree => acc
      | nodeTree (n, left, right) =>
        let
          val rightFold = tree_fold_pre_order f acc right
        in
          f(n, tree_fold_pre_order f rightFold left)
        end


(* val tree_max : tree -> int option *)
val tree_max = fn (t: tree) => 
      case t of
        emptyTree => NONE
      | nodeTree (n, left, right) => SOME (tree_fold_pre_order (fn(x,y) => if x > y then x else y) ~1073741824 t)

(* val tree_delete : tree * int -> tree *)
fun tree_delete(t: tree, v: int) = 
      case t of
        emptyTree => raise NotFound
      | nodeTree (n, left, right) => 
        if v > n then nodeTree(n, left, tree_delete(right, v))
        else if v < n then nodeTree(n, tree_delete(left,v), right)
        else
          let fun remove(subt) =
              case subt of
                (emptyTree, emptyTree) => emptyTree
              | (left, emptyTree) => left
              | (emptyTree, right) => right
              | (left, right) => let val leftMax = valOf(tree_max left) in nodeTree(leftMax, tree_delete(left, leftMax), right) end
          in
            remove(left, right)
          end

(* val tree_to_list : tree -> int list *)
val tree_to_list = fn (t: tree) =>
      case t of
        emptyTree => []
      | nodeTree (n, left, right) => tree_fold_pre_order (op::) [] t

(* val tree_filter : (int -> bool) -> tree -> tree *)
fun tree_filter f t =
  case t of
        emptyTree => emptyTree
      | nodeTree (n, left, right) =>
        if f n = false then let val subt = tree_delete(t,n) in tree_filter f subt end else nodeTree(n, tree_filter f left, tree_filter f right)

(* val tree_sum_even : tree -> int *)
val tree_sum_even = fn (t: tree) => 
  tree_fold_pre_order (op +) 0 (tree_filter (fn x => x mod 2 = 0) t)

(* Part #2 *)
(* val first_answer : ('a -> 'b option) -> 'a list -> 'b *)
fun first_answer f lst =
  case lst of
      [] => raise NoAnswer
    | [x] => if isSome(f x) then x else raise NoAnswer
    | x::xs' => if isSome(f x) then x else first_answer f xs'

(* val all_answers : ('a -> 'b list option) -> 'a list -> 'b list option *)
fun all_answers f lst =
  if List.all (fn x => isSome(f x)) lst then SOME(List.filter (fn x => isSome(f x)) lst) else NONE

(* val pattern_to_stringList : pattern -> string list option *)
fun pattern_to_stringList(p: pattern) =
  case p of 
      Wildcard => NONE
		 | Variable str => SOME [str]
		 | UnitP => NONE
		 | ConstP _ => NONE
     | ConstructorP (str,subp) => SOME(str::getOpt(pattern_to_stringList(subp),[]))
		 | TupleP [] => NONE
     | TupleP [x] => pattern_to_stringList(x)
     | TupleP (x::xs') => SOME(getOpt(pattern_to_stringList(x), []) @ getOpt(pattern_to_stringList(TupleP xs'), []))

(* val has_duplicates : string list -> bool*)
fun has_duplicates(lst: string list) =
  case lst of
      [] => false
    | [x] => false
    | x::xs' => List.exists (fn y => x = y) xs' orelse (has_duplicates xs')

(* val check_pattern : pattern -> bool *)
fun check_pattern(p: pattern) = 
  let val strList = getOpt(pattern_to_stringList(p), [])
  in
    not(has_duplicates(strList))
  end

(* val match : value * pattern -> (string * value) list option *)
fun match (v: value, p: pattern) = 
  case (v,p) of 
      (_, Wildcard) => NONE
		 | (_, Variable str) => SOME[(str, v)]
		 | (Unit, UnitP) => SOME []
		 | (Const x, ConstP y) => if x = y then SOME [] else NONE
     | (Constructor(str1,subv), ConstructorP (str2,subp)) => let val subMatch = match(subv,subp) in if str1 = str2 andalso isSome(subMatch) then subMatch else NONE end
		 | (Tuple vs, TupleP ps) => NONE(*let val tupMatch = List.collate match (vs, ps) in if length vs = length ps andalso isSome(tupMatch) then tupMatch else NONE end*)
     | (_,_) => NONE

(* val first_match : value -> pattern list -> (string * value) list option *)
fun first_match v plist =
  NONE
  (*first_answer (fn p => match(v,p)) plist*)
  (*first_answer (fn p => match(v, p)) plist*)
  (*first_answer (fn p => if isSome(match(v, p)) then match(v, p) else NONE) plist*)


(* leave the following functions untouched *)

fun tree_root t =
    case t of
        emptyTree => NONE
      | nodeTree (n, _, _) => SOME n

fun tree_equal t1 t2  =
    case (t1, t2) of
        (emptyTree, emptyTree) => true
      | (emptyTree, _) =>  false
      | (_, emptyTree) => false
      | (nodeTree(n1, l1, r1),nodeTree(n2, l2, r2)) =>
        n1 = n2 andalso (tree_equal l1 l2) andalso (tree_equal r1 r2)

infix 9 ++
infix 9 --
infix 7 == 

fun t ++ v = tree_insert_in_order(t, v)
fun t -- v = tree_delete(t, v)
fun t1 == t2 = tree_equal t1 t2

end

(*
   Your name and student id
   Sho Ya Voorthuyzen V00730770
*)

structure Set =
struct
local
  open Csc330
in

datatype 'a set = EmptySet of ('a * 'a -> order) | Set of 'a list * ('a * 'a -> order)

exception SetIsEmpty

infix 1 IDENTICAL
infix 3 EXCEPT
infix 4 IS_SUBSET_OF
infix 5 INTERSECT
infix 6 UNION
infix 7 IN
infix 8 CONTAINS        
infix 9 ++
infix 9 --

fun is_empty_set s =
    case s of
        EmptySet _ => true
    | Set(_, _) => false

fun min_in_set s =
    case s of
        EmptySet(_) => raise SetIsEmpty
    | Set(lst, _) => hd(lst)

fun max_in_set s =
    case s of
        EmptySet(_) => raise SetIsEmpty
    | Set(lst, _) => hd(rev(lst))

fun set_to_list s =
    case s of
        EmptySet(_) => []
    | Set(lst, _) => lst

fun insert_into_set(s,v) =
    case s of
        EmptySet(order) => Set([v],order)
    | Set(_,order) =>
        let fun insert(lst, v) =
            case lst of 
                [] => raise SetIsEmpty
            |[x] => if order(v,x) = LESS then v::[x] else if order(v,x) = EQUAL then [x] else x::[v]
            |x::xs' => if order(v,x) = LESS then v::x::xs' else if order(v,x) = EQUAL then x::xs' else x::insert(xs',v)
        in
            Set(insert(set_to_list(s),v), order)
        end

fun in_set(s, v) =
    case s of
        EmptySet(order) => raise SetIsEmpty
    | Set(_,order) =>
        let fun find(lst, v) =
            case lst of 
                [] => raise SetIsEmpty
            |[x] => (order(v,x) = EQUAL)
            |x::xs' => (order(v,x) = EQUAL orelse(find(xs',v)))
        in
            find(set_to_list(s),v)
        end

fun remove_from_set(s,v) =
    case s of
        EmptySet(order) => s
    | Set(_,order) =>
        let fun remove(lst, v) =
            case lst of 
                [] => raise SetIsEmpty
            |[x] => if order(v,x) = EQUAL then [] else [x]
            |x::xs' => if order(v,x) = EQUAL then xs' else x::remove(xs',v)
        in
            Set(remove(set_to_list(s),v), order)
        end
    
fun size_set(s: 'a set) =
    length(set_to_list(s))

fun getOrderFn s =
    case s of
        EmptySet order => order
    | Set(_, order) => order

fun equal_set(s, t) =
    let fun compLists (x,y,order) =
        case (x,y) of
            ([],[]) => true
        | (x::xs', y::ys') => (order(x,y) = EQUAL andalso compLists(xs',ys',order))
        | (_,_) => false
    in
        compLists(set_to_list(s), set_to_list(t), getOrderFn(s))
    end

fun union_set(s, t) =
    if equal_set(s,t) then s else
    let val x = set_to_list(s)
        fun union_lst (xs,t) =
            case xs of
                [] => t
            | [x] => insert_into_set(t,x)
            | x::xs' => union_lst(xs, insert_into_set(t,x))
    in
        union_lst(x,t)
    end

fun intersect_set(s, t) =
    if equal_set(s,t) then s else if (is_empty_set(s) orelse is_empty_set(t)) then EmptySet(getOrderFn(s)) else
    let val x = set_to_list(s)
        fun intersect_lst (xs,t,acc) =
        case xs of
            [] => raise SetIsEmpty
        | [x] => if in_set(t,x) then x::acc else acc
        | x::xs' => if in_set(t,x) then intersect_lst(xs',t, x::acc) else intersect_lst(xs',t,acc)
    in
        Set(intersect_lst(x, t, []),getOrderFn(s))
    end

fun except_set(s, t) =
    if equal_set(s,t) then EmptySet(getOrderFn(s)) else
    let val x = set_to_list(t)
        fun except_lst (xs,s) =
            case xs of
                [] => s
            | [x] => remove_from_set(s,x)
            | x::xs' => except_lst(xs', remove_from_set(s,x))
    in
        except_lst(x,s)
    end

fun is_subset_of(s, t) =
    is_empty_set(s) orelse size_set(intersect_set(s,t)) <> 0
        
fun list_to_set(lst, f) =
    case lst of
        [] => EmptySet f
    | _ => Set(lst,f)

fun str_set (s, fstr) =
    case s of
        EmptySet(_) => "{}"
    | Set([x], _) => fstr(x)
    | Set(x::xs', order) => fstr(x) ^ "," ^ str_set(Set(xs', order),fstr)
    | _ => raise SetIsEmpty
      
fun map_set (s, fcomp, f) =
    EmptySet fcomp

fun s -- v = remove_from_set(s,v)
fun s ++ v = insert_into_set(s,v)
fun s IDENTICAL t = equal_set(s, t)
fun s UNION t = union_set(s, t)
fun s INTERSECT t = intersect_set(s, t)
fun s EXCEPT t = except_set(s, t)
fun v IN s = in_set(s, v)
fun s IS_SUBSET_OF t = is_subset_of(s, t)

datatype options = IntList of int list | StringList of string list

fun sum(nums: int list) =
    case nums of
        [] => 0
    | [x] => x
    | x::xs' => sum(xs') + x

fun concat(strs: string list) =
    case strs of
        [] => ""
    | [x] => x
    | x::xs' => concat(xs') ^ x

fun comp_list_any (a: 'a list, b: 'a list, fcomp : ('a * 'a) -> order) =
    (*case a of IntList a => fcomp(sum(a), sum(b))
    | StringList a => fcomp(concat(a), concat(b))*)
    EQUAL

                          
end
end    

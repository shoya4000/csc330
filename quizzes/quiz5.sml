fun append (xs,ys) =
    let fun aux(xs, ys, acc) =
        case xs of
        [] => ys
        | x::xs' => x :: aux(xs',ys, x::acc)
    in
        aux(xs, ys, [])
    end

val x = [1,2]
val y = [2,3]
val res = append(x,y)

exception myExp

fun mistery(xs) =
    case xs of
    [] => raise myExp
    | [x] => x
    | x::xs' => mistery(xs')
   (*if null xs' then xs'
      raise myExp
   else if null (tl xs) then
      hd xs
   else
       mistery (tl xs)*)


val res2 = mistery(y)
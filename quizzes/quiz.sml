fun split x =
    (x +1, x+2)

val f = split

fun g x = 
  case x of
    (a, b) => f a
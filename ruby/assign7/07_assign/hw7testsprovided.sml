use "hw7.sml";
 
(* Must implement preprocess_prog and Shift before running these tests *)


fun real_equal(x,y) = Real.compare(x,y) = General.EQUAL;

datatype Test = ttest of (unit -> bool) * string

;
(*---------------------------------PREPROCESS--------------------------*)

val test1 = ttest(fn () => 
                let
                  val p1 = preprocess_prog(LineSegment(3.2,4.1,3.2,4.1))
                  val p2 = Point(3.2,4.1)
                in
                  case (p1, p2) of
                      (Point(a,b),Point(c,d)) =>
                      real_equal(a,c) andalso real_equal(b,d)
                    | _ => false
                end, "preprocess converts a LineSegment to a Point\n" )
                
val test2 = ttest(fn () => 
                let
                  val l1 = preprocess_prog (LineSegment(3.2,4.1,~3.2,~4.1))
                  val l2 = LineSegment(~3.2,~4.1,3.2,4.1)
                in
                  case (l1, l2) of
                      (LineSegment(a,b,c,d),  LineSegment(e,f,g,h)) =>
                      real_equal(a,e) andalso real_equal(b,f) andalso real_equal(c,g) andalso real_equal(d,h)
                    | _ => false
                end, 
             "flip an improper LineSegment")



val test3 = ttest(fn () => 
                let
                  val l1 = preprocess_prog (LineSegment(3.2,4.1,3.2,6.1))
                  val l2 = LineSegment(3.2,4.1,3.2,6.1)
                in
                  case (l1, l2) of
                      (LineSegment(a,b,c,d),  LineSegment(e,f,g,h)) =>
                      real_equal(a,e) andalso real_equal(b,f) andalso real_equal(c,g) andalso real_equal(d,h)
                    | _ => false
                end, "flip improper line segment 2")

val test4 = ttest(fn () =>
                let
                  val l1 = preprocess_prog (LineSegment(1.2,4.1,3.2,6.1))
                  val l2 = LineSegment(1.2,4.1,3.2,6.1)
                in
                  case (l1, l2) of
                      (LineSegment(a,b,c,d),  LineSegment(e,f,g,h)) =>
                      real_equal(a,e) andalso real_equal(b,f) andalso real_equal(c,g) andalso real_equal(d,h)
                    | _ => false
                end, "preprocess flips an improper LineSegment")

val test5 = ttest(fn () =>
                let
                  val l1 = preprocess_prog (LineSegment(6.2,4.1,3.2,6.1))
                  val l2 = LineSegment(3.2,6.1,6.2,4.1)
                in
                  case (l1, l2) of
                      (LineSegment(a,b,c,d),  LineSegment(e,f,g,h)) =>
                      real_equal(a,e) andalso real_equal(b,f) andalso real_equal(c,g) andalso real_equal(d,h)
                    | _ => false
                end, "preprocess flips an improper LineSegment 2")
;
(*---------------------------------eval_prog--------------------------*)
(* eval_prog tests with Shift*)
(* Using a NoPoints *)

val test6 = ttest(fn () => 
                      case (eval_prog (preprocess_prog (Shift(3.0, 4.0, NoPoints)), [])) of
	                             NoPoints => true
	                           | _ => false,
                  "eval_prog with NoPoints");

(* Using a Point *)

val test7 = ttest(fn () =>
                     let
                       val p1 = (eval_prog (preprocess_prog (Shift(3.0, 4.0, Point(4.0,4.0))), []))
                       val p2 = Point(7.0,8.0)
                     in
                       case (p1, p2) of
                           (Point(a,b),Point(c,d)) =>
                           real_equal(a,c) andalso real_equal(b,d)
                         | _ => false
                     end,
                  "eval_prog with empty enviroment");

(* Using a Var *)

val test8 = ttest(fn () =>
                     let
                       val p1 = (eval_prog (Shift(3.0,4.0,Var "a"), [("a",Point(4.0,4.0))]))
                       val p2 = Point(7.0,8.0)
                     in
                       case (p1, p2) of
                           (Point(a,b),Point(c,d)) =>
                           real_equal(a,c) andalso real_equal(b,d)
                         | _ => false
                     end,
                  "eval_prog with 'a' in environment");

(* Using a Line *)
val test9 = ttest(fn () =>
                     let
                       val l1 = (eval_prog (Shift(~3.0,4.0, Line(1.0, 5.0)), []))
                       val l2 = Line(1.0,12.0)
                     in
                       case (l1, l2) of
                           (Line(a,b),  Line(c,d)) =>
                           real_equal(a,c) andalso real_equal(b,d)
                         | _ => false
                     end,
                  "eval_prog with Line");
 
(* Using a LineSegment *)
val test10 = ttest(fn () =>
                      let
                        val l1 = (eval_prog (Shift(3.0,4.0, LineSegment(1.0, 2.0, 3.0, 4.0)), []))
                        val l2 = LineSegment(4.0, 6.0, 6.0, 8.0)
                      in
                        case (l1, l2) of
                            (LineSegment(a,b,x1,y1),  LineSegment(c,d,x2,y2)) =>
                            real_equal(a,c) andalso real_equal(b,d) andalso real_equal(x1,x2) andalso real_equal(y1,y2)
                         | _ => false
                      end,
                   "eval_prog with LineSegment");
 
(* With Variable Shadowing *)
val test11 = ttest(fn () =>
                       let
                         val p1 = (eval_prog (Shift(3.0,4.0,Var "a"), [("a",Point(4.0,4.0)),("a",Point(1.0,1.0))]))
                         val p2 = Point(7.0,8.0)
                       in
                         case (p1, p2) of
                             (Point(a,b),Point(c,d)) =>
                             real_equal(a,c) andalso real_equal(b,d)
                          | _ => false
                       end,
                    "eval_prog with shadowing 'a' in environment");

(* what about non-ops *)

val test12 = ttest(fn () =>
                     let
                       val p1 = (eval_prog (Shift(1.0,2.0, Shift(0.0,0.0,Var "a")), [("a",Point(4.0,4.0))]))
                       val p2 = Point(5.0,6.0)
                     in
                       case (p1, p2) of
                           (Point(a,b),Point(c,d)) =>
                           real_equal(a,c) andalso real_equal(b,d)
                         | _ => false
                     end,
                  "eval_prog with non-ops in environment");



val tests = [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12]

fun do_test (t:Test):bool =
    case t of
        ttest (f, m) =>
        let
          val result = f()
        in
          if f() then
            false
          else
            (print( "Test [" ^ m ^ "] failed\n"); true)
        end
;  

val _ = print "\nStarting tests...\n\n";

val _ = List.filter do_test tests

val _ = print "\nTests completed.\n\n";



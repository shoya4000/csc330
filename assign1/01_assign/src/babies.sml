(*
   Your name and student id
   Sho Ya Voorthuyzen V00730770
*)
structure Babies =
struct
local
  open Csc330
in

(* 
   Process the file into list of babies
   takes in the file string
   return list of names, with data associated with each
   string -> (string * string list) list
*)

fun processBabies (babyFile: string) =
  let val babyList = split_at(babyFile, #"\n")
  in
    if null babyList
    then []
    else
      let
        fun convertToTuples (babyList: string list) =
          let
            val lineList = split_at(hd(babyList), #",")
          in
            if null (tl(babyList))
            then
              [(hd(lineList),  tl(lineList))]
            else
              (hd(lineList),  tl(lineList)) :: convertToTuples(tl(babyList))
          end
      in
        convertToTuples(babyList)
      end
  end

(*
   gets length of a list
   list -> int
*)
fun listLength (list: 'A list) =
  if null list
  then 0
  else 1 + listLength(tl(list))

(*
   checks if baby name is in list
   string * (string * string list) list -> string option
*)
fun isIn(name: string, list: (string * string list) list) =
  if null list
  then
    NONE
  else
    if #1 (hd(list)) = name
    then
      SOME (#2 (hd(list)))
    else
      isIn(name, tl(list))

(*
   Uses the total at the end of the line to confirm calculated total
   string list -> int
*)
fun totalConfirm(data: string list) =
  if null(tl(data))
  then valOf(fromString(hd(data)))
  else totalConfirm(tl(data))

(*
   Calculates total
   string list -> string
*)
fun total(data: string list) =
  let fun calcTotal(data: string list) = 
    if null(tl(data)) (*Last entry is total for consistency check*)
    then 0
    else valOf(fromString(hd(data))) + calcTotal(tl(data))
  in
    let val calcedTotal = calcTotal(data)
    in
      if calcedTotal = totalConfirm(data)
      then " Total: " ^ int_to_string(calcedTotal) ^ "\n"
      else " Error in data - total inconsistent"
    end
  end

(*
   Checks how many years had more than 0 babies with name
   string list -> string
*)
fun years(data: string list) =
  let fun inYears(data: string list) = 
    if null(tl(data)) (*Last entry is total for consistency check*)
    then 0
    else 
      if valOf(fromString(hd(data))) = 0
      then inYears(tl(data))
      else 1 + inYears(tl(data))
  in
    " Years: " ^ int_to_string(inYears(data)) ^ "\n"
  end

(*
   Checks second to last value for 2019
   string list -> string
*)
fun for2019(data: string list) =
  let fun get2019(data: string list) =
    if null(tl(tl(data)))
    then hd(data)
    else get2019(tl(data))
  in
    " 2019: " ^ get2019(data) ^ "\n"
  end

(*
   Find first non-zero value in string list
   string list -> int
*)
fun findFirstNonZero (data: string list) =
  if null(tl(data)) (*Last entry is total for consistency check*)
    then valOf(fromString(hd(data)))
    else 
      if valOf(fromString(hd(data))) <> 0
      then valOf(fromString(hd(data)))
      else findFirstNonZero(tl(data))

(*
   Find first index where value appears
   string list * int -> int
*)
fun findFirstIndexOfValue (data: string list, value: int) =
  if valOf(fromString(hd(data))) = value
  then 0
  else 1 + findFirstIndexOfValue(tl(data), value)

(*
   Find first non-zero year
   string list * string -> string
*)
fun first(data: string list, yearSt: string) =
  let val value = findFirstNonZero(data)
  in
    let val index = findFirstIndexOfValue(data,value)
    in " First: " ^ int_to_string(valOf(fromString(yearSt)) + index) ^ " "^ int_to_string(value) ^ "\n"
    end
  end

(*
   Find last non-zero year
   string list * string -> string
*)
fun last(data: string list, yearSt: string) =
  let val reversedData = tl(rev(data)) (*Last entry is total for consistency check*) 
  in
    let val value = findFirstNonZero(reversedData)
    in
      let val index = findFirstIndexOfValue(reversedData,value)
      in " Last: " ^ int_to_string(valOf(fromString(yearSt)) + 99 - index) ^ " "^ int_to_string(value) ^ "\n"
      end
    end
  end

(*
   Find minimum non-zero year
   string list * string -> string
*)
fun min(data: string list, yearSt: string) =
  let fun calcMin(data: string list) = 
    if null(tl(data)) (*Last entry is total for consistency check*)
    then totalConfirm(data)
    else 
      let
        val result = calcMin(tl(data))
      in
        if valOf(fromString(hd(data))) < result andalso valOf(fromString(hd(data))) <> 0
        then valOf(fromString(hd(data)))
        else result
      end
  in
    let val index = findFirstIndexOfValue(data,calcMin(data))
    in " Min: " ^ int_to_string(valOf(fromString(yearSt)) + index) ^ " " ^ int_to_string(calcMin(data)) ^ "\n"
    end
  end

(*
   Find maximum non-zero year
   string list * string -> string
*)
fun max(data: string list, yearSt: string) =
  let fun calcMax(data: string list) = 
    if null(tl(data)) (*Last entry is total for consistency check*)
    then 0
    else 
      let
        val result = calcMax(tl(data))
      in
        if valOf(fromString(hd(data))) > result
        then valOf(fromString(hd(data)))
        else result
      end
  in
    let val reversedData = tl(rev(data)) (*Last entry is total for consistency check*) 
    in
      let val index = findFirstIndexOfValue(reversedData,calcMax(data))
      in " Max: " ^ int_to_string(valOf(fromString(yearSt)) + 99 - index) ^ " " ^ int_to_string(calcMax(data)) ^ "\n"
      end
    end
  end

(*
   Find avg for name use over 100 years of data
   string list -> string
*)
fun avg(data: string list) =
  " Avg: " ^ real_to_string(int_to_real(totalConfirm(data))/100.0) ^ "\n"

(*
   Get the appropriate data output from the data and processed list.
   string * (string * string list) list * string -> string
*)
fun getDataForNames(input: string, processed: (string * string list) list, yearSt: string) =
  let
    val nameList = split_at(input, #"\n")
  in
    let
      fun search (nameList: string list) =
        if null nameList
        then
          ""
        else
          let
            val name = hd(nameList)
            val nameData = isIn(name, processed)
          in
            if isSome(nameData)
            then name ^ "\n" ^ total(valOf(nameData)) ^ years(valOf(nameData)) ^ for2019(valOf(nameData)) ^ first(valOf(nameData), yearSt) ^ last(valOf(nameData), yearSt) ^ min(valOf(nameData), yearSt) ^ max(valOf(nameData), yearSt) ^ avg(valOf(nameData)) ^ search(tl(nameList))
            else name ^ "\nBaby name [" ^ name ^ "] was not found\n" ^ search(tl(nameList))
          end
    in
      search(nameList)
    end
  end

fun babies_program (fileName, yearSt) =
    let
      val file = read_file(fileName)
      val processed = processBabies(file)
      val length = listLength(processed)
      val _ = print("Read " ^ int_to_string(length) ^ " babies. Starting year "^ yearSt ^". Each baby has 100 entries.\n")
      
      val names = read_stdin()
      val output = getDataForNames(names, processed, yearSt)
      val _ = print(output)
    in
      ()
    end
        

(*
do not modify below this point
*)
        
fun main (prog_name, args) =
    let
      val (_, fileName, offsetSt) = parse_command_line args
      val _ = babies_program(fileName, offsetSt)
    in
      exit()
    end

end

end
    

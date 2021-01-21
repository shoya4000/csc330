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
   split line into name, values, and total
   string list -> string * string list * string
*)
fun splitLine(lineList: string list) = 
  let fun splitEnd(lineList: string list, total: string) =
    if null(tl(tl(lineList)))
    then ([hd(lineList)], hd(tl(lineList)))
    else 
      let val splits = splitEnd(tl(lineList),total)
      in (hd(lineList):: #1 splits, #2 splits)
      end
  in 
    let val split = splitEnd(tl(lineList), "")
    in (hd(lineList), #1 split, #2 split)
    end
  end 

(* 
   Process the file into list of babies
   takes in the file string
   return list of names, with data associated with each
   string -> (string * string list * string) list
*)

fun processBabies (babyFile: string) =
  let val babyList = split_at(babyFile, #"\n")
  in
    if null(babyList)
    then []
    else
      let fun convertToTuples (babyList: string list) =
        let val lineList = split_at(hd(babyList), #",")
        in
          if null(tl(babyList))
          then [splitLine(lineList)]
          else splitLine(lineList) :: convertToTuples(tl(babyList))
        end
      in
        convertToTuples(babyList)
      end
  end

(*
   checks if baby name is in list
   string * (string * string list * string) list -> (string list * string) option
*)
fun isIn(name: string, list: (string * string list * string) list) =
  if null list
  then NONE
  else
    if #1 (hd(list)) = name
    then
      SOME (#2(hd(list)), #3(hd(list)))
    else
      isIn(name, tl(list))

(*
   uses fromString to convert string to int option, and then gets int from that 
   string list -> int
*)
fun getVal(string: string) =
  valOf(fromString(string))

(*
   Calculates total
   string list * string -> string
*)
fun total(data: string list * string) =
  let fun calcTotal(data: string list) = 
    if null(data)
    then 0
    else getVal(hd(data)) + calcTotal(tl(data))
  in
    let val calcedTotal = calcTotal(#1 data)
    in
      if calcedTotal = getVal(#2 data)
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
    if null(data)
    then 0
    else 
      if getVal(hd(data)) = 0
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
    if null(tl(data))
    then hd(data)
    else get2019(tl(data))
  in
    " 2019: " ^ get2019(data) ^ "\n"
  end

(*
   Find first non-zero value in string list and the index of it
   string list * int -> int * int
*)
fun findFirstNonZero (data: string list, index: int) =
  if null(data)
    then (getVal(hd(data)), index)
    else 
      if getVal(hd(data)) <> 0
      then (getVal(hd(data)), index)
      else findFirstNonZero(tl(data), index + 1)

(*
   Find first non-zero year
   string list * string -> string
*)
fun first(data: string list, yearSt: string) =
  let val value = findFirstNonZero(data, 0)
  in
    " First: " ^ int_to_string(getVal(yearSt) + #2 value) ^ " "^ int_to_string(#1 value) ^ "\n"
  end

(*
   Find last non-zero year
   string list * string -> string
*)
fun last(data: string list, yearSt: string) =
  let val reversedData = rev(data)
  in
    let val value = findFirstNonZero(reversedData, 1)
    in
      " Last: " ^ int_to_string(getVal(yearSt) + length(data) - #2 value) ^ " "^ int_to_string(#1 value) ^ "\n"
    end
  end

(*
   Find minimum non-zero year
   string list * string * string -> string
*)
fun min(data: string list, total: string, yearSt: string) =
  let fun findMin(data: string list, index: int) = 
    if null(data)
    then (getVal(total), index)
    else 
      let
        val result = findMin(tl(data), index + 1)
      in
        if getVal(hd(data)) <= #1 result andalso getVal(hd(data)) <> 0
        then (getVal(hd(data)), index)
        else result
      end
  in
    let val value = findMin(data, 0)
    in
      let val index = #2 value
      in " Min: " ^ int_to_string(getVal(yearSt) + index) ^ " " ^ int_to_string(#1 value) ^ "\n"
      end
    end
  end

(*
   Find maximum non-zero year
   string list * string -> string
*)
fun max(data: string list, yearSt: string) =
  let fun findMax(data: string list, index: int) = 
    if null(data)
    then (0, index)
    else 
      let
        val result = findMax(tl(data), index + 1)
      in
        if getVal(hd(data)) > #1 result
        then (getVal(hd(data)), index)
        else result
      end
  in
    let
      val value = findMax(data, 0)
    in
      " Max: " ^ int_to_string(getVal(yearSt) + #2 value) ^ " " ^ int_to_string(#1 value) ^ "\n"
    end
  end

(*
   Find avg for name use over 100 years of data
   string list -> string
*)
fun avg(total: string) =
  " Avg: " ^ real_to_string(int_to_real(getVal(total))/int_to_real(100)) ^ "\n"

(*
   Get the appropriate data output from the data and processed list
   string * (string * string list * string) list * string -> string
*)
fun getDataForNames(input: string, processed: (string * string list * string) list, yearSt: string) =
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
            then 
              let
                val numData = #1(valOf(nameData))
                val totalVal = #2(valOf(nameData))
              in name ^ "\n" ^ total(valOf(nameData)) ^ years(numData) ^ for2019(numData) ^ first(numData, yearSt) ^ last(numData, yearSt) ^ min(numData, totalVal, yearSt) ^ max(numData, yearSt) ^ avg(totalVal) ^ search(tl(nameList))
              end
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
      val _ = print("Read " ^ int_to_string(length(processed)) ^ " babies" ^ dot ^ " Starting year "^ yearSt ^"" ^ dot ^ " Each baby has 100 entries" ^ dot ^ "\n")

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
    

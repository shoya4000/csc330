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
      let val (values, total) = splitEnd(tl(lineList),total)
      in (hd(lineList):: values, total)
      end
  in 
    let val (values, total) = splitEnd(tl(lineList), "")
    in (hd(lineList),values, total)
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
    let
      val (entryName, entryData, entryTotal) = hd(list)
    in
      if entryName = name
      then
        SOME (entryData, entryTotal)
      else
        isIn(name, tl(list))
    end
    

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
  let val (value, index) = findFirstNonZero(data, 0)
  in
    " First: " ^ int_to_string(getVal(yearSt) + index) ^ " "^ int_to_string(value) ^ "\n"
  end

(*
   Find last non-zero year
   string list * string -> string
*)
fun last(data: string list, yearSt: string) =
  let val reversedData = rev(data)
  in
    let val (value, index) = findFirstNonZero(reversedData, 1)
    in
      " Last: " ^ int_to_string(getVal(yearSt) + length(data) - index) ^ " "^ int_to_string(value) ^ "\n"
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
    let val (value, index) = findMin(data, 0)
    in
      " Min: " ^ int_to_string(getVal(yearSt) + index) ^ " " ^ int_to_string(value) ^ "\n"
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
      val (value, index) = findMax(data, 0)
    in
      " Max: " ^ int_to_string(getVal(yearSt) + index) ^ " " ^ int_to_string(value) ^ "\n"
    end
  end

(*
   Find avg for name use over number of years of data
   string list * int -> string
*)
fun avg(total: string, numEntries: int) =
  " Avg: " ^ real_to_string(int_to_real(getVal(total))/int_to_real(numEntries)) ^ "\n"

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
                val (numData, totalVal) = valOf(nameData)
              in name ^ "\n" ^ total(valOf(nameData)) ^ years(numData) ^ for2019(numData) ^ first(numData, yearSt) ^ last(numData, yearSt) ^ min(numData, totalVal, yearSt) ^ max(numData, yearSt) ^ avg(totalVal, length(numData)) ^ search(tl(nameList))
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
      val numEntries = length(#2(hd(processed)))
      val _ = print("Read " ^ int_to_string(length(processed)) ^ " babies" ^ dot ^ " Starting year "^ yearSt ^"" ^ dot ^ " Each baby has " ^ int_to_string(numEntries) ^ " entries" ^ dot ^ "\n")

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
    

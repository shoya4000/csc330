fun f xs =
   case xs of
       [] => "empty"
      | [x] => "non empty"
      | [x]::xs => "many"
      | []::xs => "many"
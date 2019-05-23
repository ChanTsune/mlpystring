
val mul : string -> int -> string
val ( *$ ) : string -> int -> string
val ( ^$ ) : char -> string -> string
val ( $^ ) : string -> char -> string
val char_of_string : char -> string 
val at : string -> int -> char
val center : string -> ?fillchar:char -> int -> string
val capitalize : string -> string
val count : string -> ?start:int -> ?fin:int -> string -> int
val endswith : string -> ?start:int -> ?fin:int -> string -> bool
val find : string -> ?start:int -> ?fin:int -> string -> int 
val get : string -> int -> char
val join : string -> string list -> string
val replace : string -> ?count:int -> string -> string -> string
val split : string -> ?count:int -> string -> string list

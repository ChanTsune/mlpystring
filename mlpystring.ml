
let ( *$ ) str n =
  let rec iter result i =
    if i = 0 then
      result
    else
      iter (result^str) (i-1) (** Buffer を利用した最適化の余地あり*)
  in iter "" n;;

let char_of_string c =  String.make 1 c;;

let ($^) s c = s ^ char_of_string c;; (* append *)
let (^$) c s = char_of_string c ^ s;; (* prepend *)

let capitalize str = String.capitalize_ascii str;;

let center str ?(fillchar=' ') width =
  let slen = String.length str in
  if slen >= width then
    str
  else
    let a = width - slen in
    let b = a/2 + a mod 2 in (** Buffer を利用した最適化の余地あり*)
    (String.make b fillchar) ^ str ^ (String.make (a-b) fillchar);;

let unwrap_ n default =
  match n with
  None -> default
  | Some(i) -> i;;

let back_index_ n len =
  if n < 0 then
    len + n
  else
    n;;

let int_adjust_index_ strlen start fin =
  if fin > strlen then
    back_index_ start strlen , strlen
  else
    back_index_ start strlen , back_index_ fin strlen;;

let adjust_index_ strlen start fin =(** Option type *)
  int_adjust_index_ strlen (unwrap_ start 0 ) (unwrap_ fin strlen);;

let get str n =
  let i = back_index_ n (String.length str) in
  String.get str i;;

let at str n = get str n;;


let slice ?(start=0) ?(fin=max_int) ?(step=1) str = 1;;

let simple_slice ?(start=0) ?(fin=max_int) str = 
  let start,fin = int_adjust_index_ (String.length str) start fin in
  String.sub str start (fin-start);;

(** 入力値は文字列 返却値は スキップテーブル(Hashtbl) *)
let wArray_set a n x = 
  Printf.printf "%c:%d\n" (char_of_int n) x;
  Array.set a n x;
  a;;

let make_table str = 
  Printf.printf "%s\n" str;
  let ascii = 256 in
  let slen = String.length str in
  let table = Array.make ascii slen in
  let rec table_update i t = 
    if i < slen then
      table_update (i+1) (wArray_set t (int_of_char str.[i]) ((slen -i) -1))
    else
      t in
  (** スキップする長さは　入力された文字列の長さ - その文字の文字列の中での位置 -1 *)
  table_update 0 table;;

let find text ?(start=0) ?(fin=max_int) suffix = 
  let start, fin = int_adjust_index_ (String.length text) start fin in  (** 開始インデックスの調整*)
  let shift_table = make_table suffix in
  let skip text pos = 
    Printf.printf "%s ,p: %d\n" text pos;
    Array.get shift_table (int_of_char text.[pos]) in
  let rec search_iter text suffix i p = 
    if p >= 0 && i < fin then
      if text.[i] = suffix.[p] then
        search_iter text suffix (i-1) (p-1)
      else
        i,p
    else
      i,p
  in
  let rec pos_iter text suffix i = 
    Printf.printf "i: %d\n" i;
    if i < fin then
      let i,p = search_iter text suffix i (String.length suffix -1) in
      if p < 0 then
        i+1
      else
        pos_iter text suffix (i + max (skip suffix p) (String.length suffix - p))
    else
      -1
  in pos_iter text suffix (String.length suffix + start - 1);;

let count str ?(start=0) ?(fin=max_int) sub =
  let start, fin = int_adjust_index_ (String.length str) start fin in
  let sublen = String.length sub in
  let rec iter cnt cursor = 
    if cursor <> -1 && (cursor+sublen) <= fin+1 then
      iter (cnt+1) (find str sub ~start:cursor+sublen)
    else
      cnt
  in iter 0 (find str sub ~start:start ~fin:fin);;

let endswith text ?(start=0) ?(fin=max_int) suffix = 
  let start, fin = int_adjust_index_ (String.length text) start fin in
  let sublen = String.length suffix in
  if (fin - start) < sublen then
    false
  else
    let sub = simple_slice text ~start:(fin-sublen) ~fin:fin in
    sub = suffix;;

let split text ?(count=max_int) sep = 
  let rec iter lst txt cnt s = 
    if cnt = 0 then
      List.rev (txt::lst)
    else
      let cusor = find txt sep in
      if cusor = -1 then
        List.rev (txt::lst)
      else
        let hd = simple_slice txt ~start:0 ~fin:cusor in
        let tl = simple_slice txt ~start:(cusor+(String.length sep)) in
        iter (hd::lst) tl (cnt-1) s
  in iter [] text count sep;;

let join str lst = 
  let rec iter result lst = 
    match lst with
    | [s] -> result ^ s
    |hd::tl -> iter (result^hd^str) tl
    | [] -> result
  in iter "" lst;;

let replace text ?(count=max_int) old new_ = 
    let lst = split text old ~count:count in
    join new_ lst;;

let expandtabs ?(tabsize=8) str = 
 replace str "\t" (String.make tabsize ' ')

let index text ?(start=0) ?(fin=max_int) suffix = 
  let i = find text ~start:start ~fin:fin suffix in
  if i = -1 then
    raise Not_found
  else
    i;;

let cisdigit c =
  '0' <= c && c <= '9';;

let cisupper c =
  'A' <= c && c <= 'Z';;

let cislower c =
  'a' <= c && c <= 'z';;

let cisalpha c =
  cislower c || cisupper c;;

let cisalnum c =
  cisalpha c || cisdigit c;;

let cisascii c =
  '\000' <= c && c <= '\127'

let cisspace c =
  ('\009' <= c && c <= '\013') || ('\028' <= c && c <= '\032');;

let cisprintable c =
  '\032' <= c && c <= '\126';;

let cisrowboundary c =
  match c with
    '\n' -> true
  | '\r' -> true
  | '\011' -> true
  | '\012' -> true
  | '\028' -> true
  | '\029' -> true
  | '\030' -> true
  | '\133' -> true
  |  _ -> false;;


let isx str ?(zero=false) f =
  let l = String.length str in
  if l = 0 then
    zero
  else
  let rec iter slen =
    if slen = 0 then
      true
    else if not (f (String.get str (slen-1)) ) then
      false
    else
      iter (slen-1)
  in iter l;;

let isalnum str = isx str cisalnum;;

let isalpha str = isx str cisalpha;;

let isascii str = isx str cisascii ~zero:true;;

let isdecimal str = isx str cisdigit;;

let isdigit str = isx str cisdigit;;

let islower str = isx str cislower;;

let isnumeric str = isdecimal str;;

let isprintable str = isx str cisprintable ~zero:true;;

let isspace str = isx str cisspace;;

let isupper str = isx str cisupper;;

let ljust str ?(fillchar=' ') width =
  let l = String.length str in
  if l >= width then
    str
  else
    String.make (width-l) fillchar ^ str;;

let rjust str ?(fillchar=' ') width =
  let r = String.length str in
  if r >= width then
    str
  else
    str ^ String.make (width-r) fillchar;;

let lower str = String.lowercase_ascii str;;

let lstrip str = 
  let rec iter cnt = 
    if cisspace (String.get str cnt) then
      iter (cnt+1)
    else
      cnt
  in 
  simple_slice str ~start:(iter 0);;

let rstrip str = 
  let rec iter cnt = 
    if cisspace (String.get str cnt) then
      iter (cnt-1)
    else
      cnt
  in
  simple_slice str ~fin:((iter ((String.length str)-1))+1);;

let strip str = lstrip (rstrip str);;

let mul str n = str *$ n;;
let loop f =
  let rec iter result f =
    iter result f
  in iter 0 f;;


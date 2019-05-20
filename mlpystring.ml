
let ( *$ ) str n =
  let rec iter result i =
    if i = 0 then
      result
    else
      iter (result^str) (i-1) (** Buffer を利用した最適化の余地あり*)
  in iter "" n;;
let ($^) s c = s ^ Char.escaped c;; (* append *)
let (^$) c s = Char.escaped c ^ s;; (* prepend *)

let capitalize str = String.capitalize_ascii str;;

let center str ?(fillchar=' ') width =
  let slen = String.length str in
  if slen >= width then
    str
  else
    let a = width - slen in
    let b = a/2 + a mod 2 in (** Buffer を利用した最適化の余地あり*)
    let fill = Char.escaped fillchar in
    fill *$ b ^ str ^  fill *$ (a-b);;

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
  back_index_ start strlen , back_index_ fin strlen;;

let adjust_index_ strlen start fin =(** Option type *)
  int_adjust_index_ strlen (unwrap_ start 0 ) (unwrap_ fin strlen);;

let get str n =
  let i = back_index_ n (String.length str) in
  String.get str i;;

let at str n = get str n;;


let slice ?(start=0) ?(fin= -1) ?(step=1) str = 1;;


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

let find text ?(start=0) ?(fin= -1) suffix = 
  let start, fin = int_adjust_index_ (String.length text) start fin in  (** 開始インデックスの調整*)
  let shift_table = make_table suffix in
  let skip text pos = 
    Printf.printf "%s ,p: %d\n" text pos;
    Array.get shift_table (int_of_char text.[pos]) in
  let rec search_iter text suffix i p = 
    if p >= 0 && i <= fin then
      if text.[i] = suffix.[p] then
        search_iter text suffix (i-1) (p-1)
      else
        i,p
    else
      i,p
  in
  let rec pos_iter text suffix i = 
    Printf.printf "i: %d\n" i;
    if i <= fin then
      let i,p = search_iter text suffix i (String.length suffix -1) in
      if p < 0 then
        i+1
      else
        pos_iter text suffix (i + max (skip suffix p) (String.length suffix - p))
    else
      -1
  in pos_iter text suffix (String.length suffix + start - 1);;

let count str ?(start=0) ?(fin= -1) sub =
  let start, fin = int_adjust_index_ (String.length str) start fin in
  let sublen = String.length sub in
  let rec iter cnt cursor = 
    Printf.printf "count ->  %d: %d\n" cnt cursor;
    if cursor <> -1 && (cursor+sublen) <= fin+1 then
      iter (cnt+1) (find str sub ~start:cursor+sublen)
    else
      cnt
  in iter 0 (find str sub ~start:start ~fin:fin);;



let mul str n = str *$ n;;



let loop f = 
  let rec iter result f = 
    iter result f
  in iter 0 f;;


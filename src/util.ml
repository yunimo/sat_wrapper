#load "str.cma";; (* Strモジュールは明示的にロードする必要がある *)

(* シグネチャ(インターフェース) *)
module type Util_type = sig
  
  val filter : ('a -> bool) -> 'a list -> 'a list
  val take : int -> 'a list -> 'a list
  val drop : int -> 'a list -> 'a list
  val take_ijk : int -> int -> int -> 'a list -> 'a list
  val take_drop : int -> 'a list -> 'a list * 'a list
  val concat : 'a list list -> 'a list

  val forInc : int -> int -> (int -> 'a) -> 'a list
  val forDec : int -> int -> (int -> 'a) -> 'a list
  val forIncDouble : int -> int -> (int * int -> 'a) -> 'a list
  val foreach    : 'a list -> ('a -> 'b) -> 'b list
  val foreachInc : int -> 'a list -> (int * 'a -> 'b) -> 'b list
  val foreachDec : int -> 'a list -> (int * 'a -> 'b) -> 'b list

  val forCkInc : int -> int -> (int -> bool) -> (int -> 'a) -> 'a list
  val forCkDec : int -> int -> (int -> bool) -> (int -> 'a) -> 'a list
  val foreachCk    : 'a list -> ('a -> bool) -> ('a -> 'b) -> 'b list
  val foreachCkInc : int -> 'a list -> ('a -> bool) -> (int * 'a -> 'b) -> 'b list
  val foreachCkDec : int -> 'a list -> ('a -> bool) -> (int * 'a -> 'b) -> 'b list

  val member : 'a -> 'a list -> bool
  val forall : ('a -> bool) -> 'a list -> bool
  val forsome : ('a -> bool) -> 'a list -> bool

  val union : 'a list -> 'a list -> 'a list
  val intersection : 'a list -> 'a list -> 'a list
  val uniq : 'a list -> 'a list

  val qsort : ('a * 'a -> bool) -> 'a list -> 'a list

  type ('a, 'b) alist = ('a * 'b) list
  val find : 'a -> ('a, 'b) alist -> 'b option
  val get_key : 'b -> ('a, 'b) alist -> 'a option
  val replace : 'a * 'b -> ('a, 'b) alist -> ('a, 'b) alist
  val append_alist : ('a, 'b) alist -> ('a, 'b) alist -> ('a, 'b) alist option

  val readDIMACS : string -> string list list

end;;

(* ストラクチャ *)
module Util : Util_type = struct
  
  (* リストの中から述語を満たす要素を抜き出す *)
  let rec filter p xs =
    match xs with
      [] -> []
    | x::xss -> if p x then x :: (filter p xss) else filter p xss;;
  
  (* リスト xs の先頭から n 個の要素を取り出したリストを返す *)
  let rec take n xs =
    match xs with
      [] -> []
    | x::xss -> if n <= 0 then [] else x :: (take (n-1) xss);;
  
  (* リスト xs の先頭から n 個の要素を取り除く *)
  let rec drop n xs =
    match xs with
      [] -> []
    | x::xss -> if n <= 0 then x::xss else drop (n-1) xss;;

  (* リスト xs の指定された要素番号 i から要素番号 j 個飛ばしたリストを返す(新規追加) *)
  let rec take_ijk s i n xs =
  let rec take_skip count skip ls =
    if count = 0 then []
    else
      match ls with
      | [] -> []
      | h :: t ->
         if skip = 0 then
          h :: take_skip (count - 1) i t
        else
          take_skip count (skip - 1) t
  in
  take_skip n s xs;;
  
  (* リスト xs を先頭から要素 n 個で分割してその組を返す *)
  let rec take_drop n xs =
    match xs with
      [] -> ([], [])
    | x::xss -> if n <= 0 then ([], x::xss)
                else
                  let (xs1, xs2) = take_drop (n-1) xss in
                  (x::xs1, xs2);;
  
  (* concat [xs1,xs2,…,xsn] = xs1@xs2@…@xsn *)
  let rec concat xss =
    match xss with
      [] -> []
    | xs::xsss -> xs @ (concat xsss);;
  
  (* [f m,f (m+1),…,f n]を返す *)
  let forInc m n f =
    List.map f (List.init (n - m + 1) (fun i -> i + m));;
  
  (* [f m,f (m-1),…,f n]を返す *)
  let forDec m n f =
    List.map f (List.init (m - n + 1) (fun i -> m - i));;

  (* [f (m,m), f (m,m+1),...,f (m,n), f (m+1,m),...,f (n,n)]を返す(新規追加) *)
  let forIncDouble m n f =
  let range = List.init (n - m + 1) (fun i -> i + m) in
  let combinations =
    List.concat (List.map (fun i -> List.map (fun j -> (i, j)) range) range)
  in
  List.map f combinations;;
  
  (* xs = [x0,x1,…,xm]とした時， [f x0,f x1,…,f xm]を返す *)
  let foreach xs f = List.map f xs;;
  
  (* xs = [x0,x1,…,xm]とした時， [f (n,x0),f (n+1,x1),…,f (n+m,xm)]を返す *)
  let foreachInc n xs f =
    List.mapi (fun i x -> f (n + i, x)) xs;;
  
  (* xs = [x0,x1,…,xm]とした時， [f (n,x0),f (n-1,x1),…,f (n-m,xm)]を返す *)
  let foreachDec n xs f =
    List.mapi (fun i x -> f (n - i, x)) xs;;
  
  (* [f i | m ≦ i ≦ n, p(i)] を返す *)
  let rec forCkInc m n p f =
    if m > n then [] else
      if p m then (f m) :: (forCkInc (m+1) n p f)
      else forCkInc (m+1) n p f;;
  
  (* [f i | m ≧ i ≧ n, p(i)] を返す *)
  let rec forCkDec m n p f =
    if m < n then [] else
      if p m then (f m) :: (forCkDec (m-1) n p f)
      else forCkDec (m-1) n p f;;
  
  (* foreachCk xs p f = [f x | x ∈ xs, p(x)] *)
  let rec foreachCk xs p f =
    match xs with
      [] -> []
    | x::xss -> if p x then (f x) :: (foreachCk xss p f)
                else foreachCk xss p f;;
  
  (* xs = [x0,x1,…,xm]とした時， [f (n+i, xi) | 0 ≦ i ≦ m, p(xi)] を返す *)
  let rec foreachCkInc n xs p f =
    match xs with
      [] -> []
    | x::xss -> if p x then (f (n, x)) :: (foreachCkInc (n+1) xss p f)
                else foreachCkInc (n+1) xss p f;;
  
  (* xs = [x0,x1,…,xm]とした時， [f (n-i, xi) | 0 ≦ i ≦ m, p(xi)] を返す *)
  let rec foreachCkDec n xs p f =
    match xs with
      [] -> []
    | x::xss -> if p x then (f (n, x)) :: (foreachCkDec (n-1) xss p f)
                else foreachCkDec (n-1) xss p f;;
  
  (* x がリスト xs の要素のとき true を，そうでないとき false を返す *)
  let rec member x xs = 
    match xs with
      [] -> false
    | m::ms -> if x = m then true else member x ms;;
  
  (* ∀x ∈ xs. p(x) *)
  let rec forall p xs =
    match xs with
      [] -> true
    | x::xss -> if p x then forall p xss else false;;
  
  (* ∃x ∈ xs. p(x) *)
  let rec forsome p xs =
    match xs with
      [] -> false
    | m::ms -> if p m then true else forsome p ms;;
  
  (* 和集合(重複あり) *)
  let union xs ys =
    match xs, ys with
      [],     []     -> []
    | x::xss, []     -> x::xss
    | []    , y::yss -> y::yss
    | x::xss, y::yss ->
       let rec uni us rslt =
         match us, rslt with
           [], rslt -> rslt @ ys
         | u::uss, rslt -> if member u ys then uni uss rslt
                           else uni uss (u::rslt)
       in
       uni xs [];;
  
  (* 積集合(重複なし) *)
  let intersection xs ys =
    match xs, ys with
      [], [] -> []
    | x::xss, [] -> []
    | [], y::yss -> []
    | x::xss, y::yss ->
       let rec its us rslt =
         match us, rslt with
           [], rslt -> rslt
         | u::uss, rslt -> if member u ys then its uss (u::rslt)
                           else its uss rslt
       in
       its xs [];;
  
  (* リスト xs の要素の重複を取り除く *)
  let rec uniq xs =
    match xs with
      [] -> []
    | x::xss -> if member x xss then uniq xss
                else x :: (uniq xss);;
  
  (* 比較用関数 cmp に従いリスト xs をクイックソートする *)
  let rec qsort cmp xs =
    match xs with
      [] -> []
    | x::xss -> (qsort cmp (filter (fun y -> cmp (x,y)) xs))
                @ (x :: (qsort cmp (filter (fun y -> not (cmp (x,y))) xs)));;
  
  (* 連想リストを表現する型 *)
  type ('a, 'b) alist = ('a * 'b) list;;
  
  (* (key, v) ∈ xs のときは SOME v を， そうでないときは NONE を返す *)
  let rec find key xs =
    match xs with
      [] -> None
    | (k,v)::xss -> if key = k then Some v else find key xss;;
  
  (* (key, v) ∈ xs のときは SOME key を， そうでないときは NONE を返す *)
  let rec get_key v alist =
    match alist with
      [] -> None
    | (key,value)::alists -> if v = value then Some key else get_key v alists;;
  
  (* key, v') ∈ xs のときは v' を v に書き替え， そうでないときは xs に (key, v) を追加したリストを返す *)
  let rec replace (key, value) xs =
    match xs with
      [] -> [(key, value)]
    | (k,v)::xss -> if key = k then (k,v) :: xss
                    else (k, v) :: (replace (key, value) xss);;
  
  (* 連想リスト xs と ys の連接が 連想リスト zs になるときは SOME zs を， そうでないときは NONE を返す *)
  let rec append_alist alist1 alist2 =
    match alist2 with
      [] -> Some alist1
    | (k, v) :: alist2s -> match find k alist1 with
                             Some v1 -> if v1 = v then append_alist alist1 alist2s else None
                           | None -> match append_alist alist1 alist2s with
                                       Some alist -> Some ((k, v) :: alist)
                                     | None -> None;;
  
  (* DIMACS形式のファイルからコメント行を削除し，有用な情報行のリストを返す関数 *)
  let readDIMACS file =    
    let cnl = open_in file in    
    let rec readDIMACSline str = (* Str.split = strを[]内の正規表現で区切ったリストを返す→メモリの削減?? *)
      let str_list = Str.split (Str.regexp "[ \t\n\013]+") str in (* \t = タブ文字, \n = 改行文字, \013 = 垂直タブ, [ ]+ = 1回以上繰り返し *)
      match str_list with
      | "c" :: _ -> []  (* コメント行 *)
      | _ -> str_list   (* 全部読み込み終わりは分割したリストのリストを返す *)
    in    
    let rec loop () = (* input_line = 与えられたチャンネルから改行文字までの文字を読み込む *)
      let line = try input_line cnl with End_of_file -> "UNSAT" in
      if line = "UNSAT" then begin close_in cnl; [] end
      else begin
          if readDIMACSline line = []
          then loop ()                        (* コメント行のとき *)
          else readDIMACSline line :: loop () (* プログラム行のとき *)
        end
    in 
    loop ();;
end;;

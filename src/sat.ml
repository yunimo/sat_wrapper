(*** 命題論理式をSATソルバに変換し，結果を返す関数 ***)

(* module type シグネチャ名 = sig ... end *)
(* module F (引数 : 引数のシグネチャ) = struct ... end *)
                   
(* ファンクタはストラクチャにパラメータをもたせたもの⇒一部だけ違うストラクチャを作成可能
   今回は命題ごとに型(PVNAME)が異なるためストラクチャではなくファンクタで実装 *)

(* module type のシグネチャ *)
module type PVNAME = sig
  type pvname
end
                   
(* 依存型ファンクタ Satfunc のシグネチャの定義 *)
module Satfunc (Pvname : PVNAME) :
  sig
    type pvname = Pvname.pvname
    type litInt

    val print : unit -> unit
    val display_file_contents : string -> unit
                
    val sat_prop_start: unit -> unit
    val sat_prop_return: litInt -> (float * int * int * (Pvname.pvname, bool) Util.alist option)
      
    val true_: litInt
    val false_: litInt
    val pvar_: Pvname.pvname -> litInt
    val not_: litInt -> litInt
    val and_: litInt -> litInt -> litInt
    val ands_: litInt list -> litInt
    val or_: litInt -> litInt -> litInt
    val ors_: litInt list -> litInt
    val imp_: litInt -> litInt -> litInt
    val iff_: litInt -> litInt -> litInt
    val xor_: litInt -> litInt -> litInt
    val atmost1_: litInt list -> litInt
    val unique_: litInt list -> litInt
    val prop: Pvname.pvname Prop.prop -> litInt
      
    val sat_prop: Pvname.pvname Prop.prop -> (float * int * int * (Pvname.pvname, bool) Util.alist option)
      
  end = struct
  
  open Prop;;
  
  (* pvnameの定義 *)
  type pvname = Pvname.pvname;;
  
  (* 0は使用不可，1 = True，-1 = FALSE，-x = NOT(x) *)
  type litInt = int;;
  let inv i = -1 * i;; (* 負の整数のときは(-3)などで入力しないとエラー出る *)
  
  (* 作業用変数を定義 *)
  (* ref = レコード型の参照型を定義 *)
  let in_file = ref "";;   (* SATソルバの入力ファイル名 *)
  let in_fp = ref (out_channel_of_descr stderr);; (* 入力ファイルへの出力チャネル *)
  let out_file = ref "";;  (* SATソルバの出力ファイル名 *)
  let cur_var = ref 0;;    (* 変数番号の現在値 *)
  let cur_clause = ref 0;; (* 現在の節の個数 *)
  let var_table : (int, pvname) Util.alist ref = ref [];; (* Tseitin変換前の変数を格納するリスト  *)
  let rslt : pvname list ref = ref [];; (* 入力論理式を真にする解釈を返す *)

  (* 作業用変数の初期化関数 *)
  (* ! = 参照の内容を取り出す演算子，:= = 参照に代入する演算子 *)
  let init_var () =
    (in_file := Filename.temp_file "input" "";   (* SATソルバ入力用の一時ファイルの作成*)
     in_fp := open_out (!in_file);               (* 書き込み用のファイルopen *)
     out_file := Filename.temp_file "output" ""; (* SATソルバ出力用の一時ファイルの作成*)
     cur_var := 1;                               (* 1番は True 表現用に予約 *)
     cur_clause := 0;                            (* 最初は節を0個に設定 *)
     var_table := [];
     rslt := []);;

  (* 変数の数と節の数の確認(test用) *)
  let print () = (print_int (!cur_var); print_newline ();
                  print_int (!cur_clause); print_newline ());;

  (* ファイル内容の確認(test用) *)
  let display_file_contents file_name =
      let in_channel = open_in file_name in
      try
        while true do
          let line = input_line in_channel in
          print_endline line
        done
      with
      | End_of_file -> close_in in_channel;;
  
  (* Tseitin変換でのフレッシュ変数に対する変数番号生成 *)
  let gen_var_fresh () =
    (cur_var := !cur_var + 1;
     !cur_var);;
  
  (* 入力論理式中の変数に対する変数番号生成 *)
  let gen_var_input x =
    match (Util.get_key x (!var_table)) with
    | Some i -> i (* すでに使われている変数のとき *)
    | None ->     (* まだ使われていない変数のとき *)
       let i = gen_var_fresh () in
       var_table := (i, x)::(!var_table);
       i;;

  (* 入力ファイルのプログラム行を書き込む *)
  let write_literal l =
    (if l < 0 then
       output_string (!in_fp) ("-" ^ (string_of_int (inv l))) (* lが負の場合 *)
     else
       output_string (!in_fp) (string_of_int l);
     output_char (!in_fp) ' ');; (* lが正の場合 *)
  
  let rec write_clause = function
    | [] ->
       (cur_clause := !cur_clause + 1;
        output_string (!in_fp) "0\n")
    | l::ls ->
       (write_literal l;
        write_clause ls);;

  (* 入力ファイル1行目の前半を書き込む(p cnf ) *)
  let write_fstline_front () =
    output_string (!in_fp) "p cnf \n";;
    
  (* 入力ファイル1行目の後半を書き込む(変数の数，節の数) *)
  let write_fstline_rear () =
    let script = "'1s/$/" ^ (string_of_int (!cur_var)) ^ " " ^ (string_of_int (!cur_clause)) ^ "/'" in
    Exec.exec SetUp.sed_cmd ["-i"; script; !in_file];;

  (* 出力ファイルの読み込み *)
  (* SATソルバの結果が 1 -2 -3 4 ... 0 のように順に出力されていることを仮定 *)
  (* 各 (i,v) ∈ var_table で 2≦i を仮定 *)
  let read_rslt () =
  let out_fp = open_in (!out_file) in
  let rec next n =
    match input_char out_fp with
    | ' ' -> next2 n (* 空白行のとき⇒次に充足解が来る *)
    | _   -> next n  (* Trueの分の1,'-'を読み取った残りの数字のとき再帰 *)
  and next2 n =      (* 充足解の読み取り *)
    match n with
    | 1 -> (match input_char out_fp with
              ' ' -> next2 1
            | '-' -> false
            | _   -> true)
    | n -> (match input_char out_fp with (* 節の後に出てきた変数 *)
              ' ' -> next n
            | _   -> next (n-1))
  in
  match input_line out_fp with
  | exception End_of_file -> (close_in out_fp; None) (* ファイルの終端またはエラーが発生したとき *)
  | line ->
     if String.sub line 0 1 = "U" then (* 1行目にUNSATが表示されているとき *)
       (close_in out_fp; None)
     else                              (* 1行目にSATが表示されているとき *)
       let rec entries i alist = (* var_tableの中身を[(1,1);(2,1);(3,1)...]の形にする *)
         match alist with
           [] -> []
         | (j,v)::alists -> (v, j-i) :: entries j alists in
       let sort_tuple_list lst = (* 変数が順番に出力されるようにsortする *)
         let compare_tuples (a, _) (b, _) = compare a b in
         List.sort compare_tuples lst in
       let rslt = (List.map (fun (a,b) -> (a, next b)) (sort_tuple_list (entries 1 (List.rev(!var_table))))) in (* next関数適用 *)
       close_in out_fp;
       Some rslt;;

  (* SATソルバとの対話開始 *)
  let sat_start () =
    (init_var ();
     write_fstline_front (); (* 入力ファイルの1行目の前半(p cnf )の書き込み完了 *)
     write_clause [1]);; (* True の分の節 *)

  (* SATソルバとの対話開始 (CNFモード) *)
  let sat_cnf_start () = sat_start ();;

  (* SATソルバとの対話開始 (Propモード) *)
  let sat_prop_start () = sat_start ();;

  (* SATソルバとの対話終了 (CNFモード) *)
  let sat_cnf_return () =
    let _ = close_out (!in_fp) in
    let _ = write_fstline_rear () in (* 入力ファイルの1行目の後半(変数の数 節の数)の書き込み完了 *)

    (* SATソルバ用入力ファイルを表示 *)
    (*let _ = display_file_contents (!in_file) in*)
    
    let (tm, _) = Exec.exec_o SetUp.sat_cmd [!in_file; !out_file] in

    (* SATソルバ用出力ファイルを表示 *)
    (*let _ = display_file_contents (!out_file) in*)
    
    let rslt = read_rslt () in
    let _ = Sys.remove (!in_file) in
    let _ = Sys.remove (!out_file) in
    (tm, !cur_var, !cur_clause, rslt);;

  (* SATソルバとの対話終了 (Propモード) *)
  let sat_prop_return l = (write_clause [l]; sat_cnf_return ());;

  (*** Tseitin変換 ***)
  let true_ = 1;;
  
  let false_ = -1;;

  let pvar_ x = gen_var_input x;;

  let not_ l = inv l;;

  let and_ l1 l2 =
    let i = gen_var_fresh () in
    write_clause [inv i; l1];
    write_clause [inv i; l2];
    write_clause [i; inv l1; inv l2];
    i;;

  let ands_ = function
    | [] -> true_
    | [l] -> l
    | ls ->
      let i = gen_var_fresh () in
      List.iter (fun l -> write_clause [inv i; l]) ls;
      write_clause (i::(List.map inv ls));
      i;;

  let or_ l1 l2 =
    let i = gen_var_fresh () in
    write_clause [i; inv l1];
    write_clause [i; inv l2];
    write_clause [inv i; l1; l2];
    i;;

  let ors_ = function
    | [] -> false_
    | [l] -> l
    | ls ->
       let i = gen_var_fresh () in
       List.iter (fun l -> write_clause [i; inv l]) ls;
       write_clause ((inv i)::ls);
       i;;

  let imp_ l1 l2 =
    let i = gen_var_fresh () in
    write_clause [i; l1];
    write_clause [i; inv l2];
    write_clause [inv i; inv l1; l2];
    i;;

  let iff_ l1 l2 =
    let i = gen_var_fresh () in
    write_clause [i; l1; l2];
    write_clause [i; inv l1; inv l2];
    write_clause [inv i; inv l1; l2];
    write_clause [inv i; l1; inv l2];
    i;;

  let xor_ l1 l2 =
    let i = gen_var_fresh () in
    write_clause [i; inv l1; l2];
    write_clause [i; l1; inv l2];
    write_clause [inv i; l1; l2];
    write_clause [inv i; inv l1; inv l2];
    i;;
  
(*****************************************************************
  (* 列挙法: [t,f,f,f],[f,t,f,f],[f,f,t,f],[f,f,f,t]の1つが真 *)
  let rec uni ls = forInc 1 (List.length ls) (fun i ->
                       ands_ (foreachInc 1 ls (fun (j,l) ->
                                  if i=j then l else inv l)));;
  
  let rec unique_ = function
    | [] -> false_
    | [l] -> l
    | ls -> ors_ (uni ls);;
  
  let rec atmost1_ = function
    | [] -> true_
    | [l] -> true_
    | ls ->  ors_ ((ands_ (List.map inv ls))::(uni ls));;
*****************************************************************)      

  (* 温存法：Orsを利用して見かけ上小さくする *)
  let rec atmost1_sub = function
    | [l1; l2] -> [not_ (and_ l1 l2)]
    | l :: ls -> (imp_ l (not_ (ors_ ls))) :: (atmost1_sub ls)
    | _ -> [];;
  
  let rec atmost1_ = function
    | [] -> true_
    | [l] -> true_
    | ls -> ands_ (atmost1_sub ls);;
  
  let rec unique_ = function
    | [] -> false_
    | [l] -> l
    | ls -> ands_ ((ors_ ls) :: (atmost1_sub ls));;

  let rec prop = function
      TRUE -> 1
    | FALSE -> -1
    | PVAR x -> pvar_ x
    | NOT q -> not_ (prop q)
    | AND (q1, q2) -> and_ (prop q1) (prop q2)
    | ANDs qs -> ands_ (List.map prop qs)
    | OR (q1, q2) -> or_ (prop q1) (prop q2)
    | ORs qs -> ors_ (List.map prop qs)
    | IMP (q1, q2) -> imp_ (prop q1) (prop q2)
    | IFF (q1, q2) -> iff_ (prop q1) (prop q2)
    | XOR (q1, q2) -> xor_ (prop q1) (prop q2)
    | ATMOST1 qs -> atmost1_ (List.map prop qs)
    | UNIQUE qs -> unique_ (List.map prop qs)

  (* 命題直接入力用のSAT *)
  let sat_prop p = (sat_prop_start (); sat_prop_return (prop p))

end;;

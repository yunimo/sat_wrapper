(* nQueen問題 *)

(* SATソルバラッパーを読み込む *)
#use "/mnt/fs/student/nishimoto/sat-wrapper/load.ml";;

(* nQueen問題の n *)
let n = 8;;

(******************
 *** 出力の表示 ***
 ******************)

(* 出力結果の整形用関数 *)
let pprint_cell (i, j) vs =
  match (Util.find (i, j) vs) with
    Some value -> if value then print_char 'Q' else print_char ' '
  | None -> print_char ' ';;

let rec pprint_HR = function
    0 -> print_string "+\n"
  | n -> (print_string "+-"; pprint_HR (n-1));;

let rec pprint vs =
  (forDec n 1 (fun j ->
       (pprint_HR n;
        forInc 1 n (fun i ->
            (print_string "|";
             pprint_cell (i,j) vs));
        print_string "|\n"));
   pprint_HR n);;

(* 見やすく出力 *)
let print_rslt (tm, sum_v, sum_c, rslt) =
  (print_string "=== result ===================";
   Printf.printf "\ntime: %fms" (1000.0 *. tm);
   Printf.printf "\nVariables: %d" sum_v;
   Printf.printf "\nClauses: %d" sum_c;
   match rslt with
   | None -> print_string "\nUNSAT"
   | Some vs -> (print_string "\nSAT:\n"; pprint vs;
                 print_string "\n"));;

(*******************************
 *** SATソルバで問題を解こう ***
 *******************************)

(* 命題変数の型の定義: x_ij を (i,j) で表現することにする *)
type pv = int * int;;

(* 与えた命題変数の型に対応する充足可能性判定用のモジュールを作成 *)
module Sat = Satfunc (struct type pvname = pv end);;
open Sat;;

(* SATソルバへの入力開始: 必須です．絶対に忘れないように *)
sat_prop_start ();;

(* 命題P1: 横の列にQueenが必ず1つだけ置かれる *)
let l1 = ands_ (forInc 1 n (fun i ->
                    unique_ (forInc 1 n (fun j ->
                                 pvar_ (i,j)))));;

(* 命題P2: 縦の列にQueenが必ず1つだけ置かれる *)
let l2 = ands_ (forInc 1 n (fun j ->
                    unique_ (forInc 1 n (fun i ->
                                 pvar_ (i,j)))));;

(* 命題P3: 右斜めの列にQueenが高々1つしか置かれない *)
let l3 = ands_ (forInc 2 (2*n) (fun k ->
                    atmost1_ (forCkInc 1 n
                                (fun i -> 1<=(k-i) && (k-i)<=n)
                                (fun i -> pvar_ (i,k-i)))));;

(* 命題P4:  左斜めの列にQueenが高々1つしか置かれない *)
let l4 = ands_ (forInc (1-n) (n-1) (fun k ->
                    atmost1_ (forCkInc 1 n
                                (fun i -> 1<=(i-k) && (i-k)<=n)
                                (fun i -> pvar_ (i,i-k)))));;

(* nQueen問題 *)
let l_rslt = ands_ [l1;l2;l3;l4];;

(* 出力 *)
let _ = print_rslt (sat_prop_return l_rslt);;

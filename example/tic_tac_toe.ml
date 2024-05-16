(* n目並べ(偶数のみ)の後手必勝法における盤面への番号付け *)

(* SATソルバラッパーを読み込む *)
#use "/mnt/fs/student/nishimoto/sat-wrapper/load.ml";;

(* 4目並べ *)
let n = 4;;

(* 4目並べの盤面番号付けの数 *)
let m = (n*n)/2;;

(******************
 *** 出力の表示 ***
 ******************)

(* Trueが割り当てられた変数のリストを返す *)
let rec get_true_number vs =
  match vs with
    [] -> []
  | ((i,j,k),tf)::vss -> if tf then (i,j,k)::(get_true_number vss) else get_true_number vss;;

(* タプルの第一要素でソートしたリストを返す *)
let sort_tuples lst =
  List.sort (fun (a, _, _) (b, _, _) -> compare a b) lst;;

(* 後手必勝盤面を形成 *)
let rec print_tic_tac_toe ss =
  match ss with
  | [] -> print_string ""
  | (i, j, k)::sss ->
     if j < 4 then (
       print_string (" | " ^ string_of_int k);
       print_tic_tac_toe sss)
     else (
       print_string (" | " ^ string_of_int k ^ " |\n\n");
       print_tic_tac_toe sss
     );;

(* 見やすく出力 *)
let print_rslt (tm, sum_v, sum_c, rslt) =
  (print_string "=== result ===================";
   Printf.printf "\ntime: %fms" (1000.0 *. tm);
   Printf.printf "\nVariables: %d" sum_v;
   Printf.printf "\nClauses: %d" sum_c;
   match rslt with
   | None -> print_string "\nUNSAT"
   | Some vs -> (print_string "\nSAT:\n";
                 print_tic_tac_toe (sort_tuples (get_true_number vs));
                 print_string "\n"));;

(*******************************
 *** SATソルバで問題を解こう ***
 *******************************)

(* 命題変数の型の定義: x_ij を (i,j) で表現することにする *)
type pv = int * int * int;;

(* 与えた命題変数の型に対応する充足可能性判定用のモジュールを作成 *)
module Sat = Satfunc (struct type pvname = pv end);;
open Sat;;

(* SATソルバへの入力開始: 必須です．絶対に忘れないように *)
sat_prop_start ();;
print ();;
  
(* 命題P1: 1つのマスには番号が必ず1つだけ割り当てられる *)
let l1 = ands_ (forInc 1 n (fun i ->
                    ands_ (forInc 1 n (fun j ->
                               unique_ (forInc 1 m (fun k -> pvar_ (i,j,k)))))));;

(* 命題P2: 各行に同じ番号が重複(2つ以上存在)している *)
let l2 = ands_ (forInc 1 n (fun i ->
                    unique_ (forInc 1 m (fun k ->
                                 not_ (atmost1_ (forInc 1 n (fun j -> pvar_ (i,j,k))))))));;

(* 命題P3: 各列に同じ番号が重複(2つ以上存在)している *)
let l3 = ands_ (forInc 1 n (fun j ->
                    unique_ (forInc 1 m (fun k ->
                                 not_ (atmost1_ (forInc 1 n (fun i -> pvar_ (i,j,k))))))));;

(* 命題P4: 1~mが必ず2つ使われる *)
let l4 = ands_ (forInc 1 m (fun k ->
                    not_ (atmost1_ (forIncDouble 1 n (fun (i,j) -> pvar_ (i,j,k))))));;

(* n目並べの後手必勝の盤面番号付け *)
let l_rslt = ands_ [l1;l2;l3;l4];;

print();;

(* 出力 *)
let _ = print_rslt (sat_prop_return l_rslt);;

(* グラフの強連結性判定 *)

(* SATソルバラッパーを読み込む *)
#use "/mnt/fs/student/nishimoto/sat-wrapper/load.ml";;

(**********************
 *** 問題の入力処理 ***
 **********************)

(* 入力ファイル *)
(*let input_file = "example/sc_sample/6-sc.txt";;*)
let input_file = "example/sc_sample/6-nsc.txt";;
(*let input_file = "example/sc_sample/3-nsc.txt";;*)

exception ParseError;;
let rec parse = function
  | ["p"; "graph"; k]::sss ->
     ((int_of_string k), parse_edge sss)
  | _ -> raise ParseError
and parse_edge = function
  | [] -> []
  | ([n1; n2]::sss) ->
     ((int_of_string n1, int_of_string n2) :: parse_edge sss)
  | _ -> raise ParseError;;

let (given_nodes_sum, given_edgs) = parse (readDIMACS input_file);;

(******************
 *** 出力の表示 ***
 ******************)

(* 出力結果の解析用関数 *)
let rec get_hanrei = function
  | [] -> None
  | (v, true)::vs -> get_hanreiF v vs (* １つ目の変数が true *)
  | (v, false)::vs -> get_hanreiT vs v (* 1つ目の変数が false *)
and get_hanreiF v1 = function
  | (v2, false)::_ -> Some (v1, v2) (* (trueの変数,falseの変数)のセット *)
  | _::vs -> get_hanreiF v1 vs
  | _ -> None
and get_hanreiT = function
  | (v1, true)::_ -> fun v2 -> Some (v1, v2) (* (trueの変数,falseの変数)のセット *)
  | _::vs -> get_hanreiT vs
  | _ -> fun _ -> None;;

let print_hanrei (nd1, nd2) =
  Printf.printf "\nNo path from %d to %d" nd1 nd2;;

(* 見やすく出力 *)
let print_rslt (tm, sum_v, sum_c, rslt) =
  Printf.printf "=== result ===================";
  Printf.printf "\ntime: %.10f ms" (1000.0 *. tm);
  Printf.printf "\nVariables: %d" sum_v;
  Printf.printf "\nClauses: %d" sum_c;
  match rslt with
  | None -> Printf.printf "\nStrongly Connected \n" (* 強連結のとき *)
  | Some vs ->                                   (* 強連結でないとき *)
      (match get_hanrei vs with
       | Some path -> print_hanrei path
       | None -> ()); Printf.printf "\n\n";;

(*******************************
 *** SATソルバで問題を解こう ***
 *******************************)

(* 命題変数の型の定義: グラフの各ノードを int 型で表現することにする *)
type pv = int;;

(* 与えた命題変数の型に対応する充足可能性判定用のモジュールを作成 *)
module Sat = Satfunc (struct type pvname = pv end);;
open Sat;;

(* SATソルバへの入力開始: 必須です．絶対に忘れないように *)
sat_prop_start ();;

(* ∧_(i,j)∈given_edgs (xi ⇒ xj) *)
let l1 = ands_ (foreach given_edgs (fun (nd1, nd2) -> imp_ (pvar_ nd1) (pvar_ nd2)));;

(* 少なくとも一つのノードが true *)
let l2 = ors_ (forInc 1 given_nodes_sum (fun nd -> pvar_ nd));;

(* 少なくとも一つのノードが false *)
let l3 = ors_ (forInc 1 given_nodes_sum (fun nd -> not_ (pvar_ nd)));;

(* 強連結でない事の判定 *)
let l_rslt = ands_ [l1; l2; l3];;

(* 出力 *)
let _ = print_rslt (sat_prop_return l_rslt);;

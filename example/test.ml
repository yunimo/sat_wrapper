(* SATソルバラッパーを読み込む *)
#use "/mnt/fs/student/nishimoto/sat-wrapper/load.ml";;

(*******************************
 *** SATソルバで問題を解こう ***
 *******************************)

(* 命題変数の型の定義: グラフの各ノードを int 型で表現することにする *)
type pv = int;;

(* 与えた命題変数の型に対応するSATソルバインターフェースの作成 *)
module Sat = Satfunc (struct type pvname = pv end);;
open Sat;;

(* SATソルバへの入力開始: 必須です．絶対に忘れないように *)
sat_prop_start ();;

(* 強連結でない事の判定 *)
let l = or_ (imp_ (pvar_ 1) (pvar_ 2)) (and_ (pvar_ 1) (pvar_ 2));;

(* 出力 *)
let _ = sat_prop_return l;;

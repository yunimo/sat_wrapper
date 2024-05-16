(* 命題入力＆逐次実行モード *)

(* SATソルバラッパーを読み込む *)
#use "/mnt/fs/student/nishimoto/sat-wrapper/load.ml";;

(* 命題変数の型の定義: グラフの各ノードを int 型で表現することにする *)
type pv = string * int;;

(* 与えた命題変数の型に対応するSATソルバインターフェースの作成 *)
module Sat = Satfunc (struct type pvname = pv end);;
open Sat;;

sat_prop_start ();; (* 必須です．絶対に忘れないように *)
let l11 = pvar_ ("x", 1);;
let l12 = pvar_ ("x", 2);;
let l1 = ands_ [l11; l12];;
let l21 = pvar_ ("x", 1);;
let l22 = pvar_ ("x", 3);;
let l2 = and_ l21 l22;;
let l311 = pvar_ ("x", 2);;
let l31 = not_ l311;;
let l32 = pvar_ ("x", 3);;
let l3 = imp_ l31 l32;;
let l = ors_ [l1; l2; l3];;
sat_prop_return l;; (* これで返し値が帰ってくる．start したら例え途中で失敗しても必ず実行してください *)

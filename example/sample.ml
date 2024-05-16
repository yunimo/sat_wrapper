(* グラフの強連結性判定 *)

(* SATソルバラッパーを読み込む *)
#use "/mnt/fs/student/nishimoto/sat-wrapper/load.ml";;

(* 与えた命題変数の型に対応する充足可能性判定用のモジュールを作成 *)
module Sat = Satfunc (struct type pvname = int end);;
open Sat;;

(* SATソルバへの入力開始 *)
sat_prop_start ();;

let e = [(1,2);(2,1);(2,3)];;

(* ∧_(i,j)∈given_edgs (xi ⇒ xj) *)
let l1 = ands_ (foreach e (fun (nd1, nd2) -> imp_ (pvar_ nd1) (pvar_ nd2)));;

(* 少なくとも一つのノードが true *)
let l2 = ors_ (forInc 1 3 (fun nd -> pvar_ nd));;

(* 少なくとも一つのノードが false *)
let l3 = ors_ (forInc 1 3 (fun nd -> not_ (pvar_ nd)));;

(* 強連結でない事の判定 *)
sat_prop_return (ands_ [l1; l2; l3]);;

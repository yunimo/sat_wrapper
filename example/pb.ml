(* 命題入力＆一括実行モード *)

(* SATソルバラッパーを読み込む *)
#use "/mnt/fs/student/nishimoto/sat-wrapper/load.ml";;

(* 命題変数の型の定義 *)
type pv = string * int;;

(* 与えた命題変数の型に対応するSATソルバインターフェースの作成 *)
module Sat = Satfunc (struct type pvname = pv end);;
open Sat;;

sat_prop (ORs [ANDs [PVAR ("x", 2); PVAR ("x", 3)];
               AND (PVAR ("x", 1), PVAR ("x", 3));
               IMP (NOT (PVAR ("x", 1)), PVAR ("x", 3))]);;

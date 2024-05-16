(*** コマンドのパスを指定 ***)

(* minisatコマンド *)
let sat_command = "/usr/bin/minisat";;

(* sedコマンド⇒入力されたテキストデータを1行ずつ読み込んで指定した処理を適用して出力を行う *)
let sed_command = "/bin/sed";;

(* 上述のコマンドを設定．諸般の事情により冗長になっている *)
module type SigSetUp = sig
  val sat_cmd : string
  val sed_cmd : string
end;;
module SetUp : SigSetUp = struct
  let sat_cmd = sat_command
  let sed_cmd = sed_command
end;;

(* ソースの読み込み *)
#use "/home/fs/student/nishimoto/sat-wrapper/src/util.ml";; open Util;;
#use "/home/fs/student/nishimoto/sat-wrapper/src/exec.ml";;
#use "/home/fs/student/nishimoto/sat-wrapper/src/prop.ml";;
#use "/home/fs/student/nishimoto/sat-wrapper/src/sat.ml";;

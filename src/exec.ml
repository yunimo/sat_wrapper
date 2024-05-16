(* 外部コマンドを実行する際に使用 *)
#load "unix.cma";;
open Unix;;

module type SigExec = sig
  val exec : string -> string list -> float * process_status
  val exec_o : string -> string list -> float * string
end

module Exec : SigExec = struct
  
  (* 現在時刻を取得 *)
  let cur_time () = Unix.gettimeofday ();;

  let exec cmd args =
    let rec concat = function
        [] -> ""
      | s::ss -> s ^ " " ^ (concat ss) in
    let starttime = cur_time () in
    let st = Unix.system (concat (cmd :: args)) in (* 与えられたシェルコマンドを実行 *)
    (cur_time () -. starttime, st);;

  let exec_o cmd args =
  let starttime = cur_time () in
  let pr = Unix.open_process_in (cmd ^ " " ^ String.concat " " args) in
  let result = ref "" in
  let rec receiveRest () =
    match input_char pr with
    | exception End_of_file -> ()
    | char ->
      result := !result ^ String.make 1 char;
      receiveRest ()
  in
  receiveRest ();
  ignore (Unix.close_process_in pr);
  (cur_time () -. starttime, !result);;
  
end;;

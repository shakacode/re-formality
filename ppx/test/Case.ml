type result =
  { actual : string * string
  ; expected : string * string
  }

let testable =
  let open Alcotest in
  pair string string
;;

let nothing = ""

module Path = struct
  let join xs = xs |> String.concat Filename.dir_sep

  let test_cases_dir =
    let open Filename in
    [ current_dir_name; "ppx"; "test"; "cases" ] |> join
  ;;

  let source x = Filename.concat test_cases_dir (x ^ ".res")
  let snapshot x = Filename.concat test_cases_dir (x ^ ".snapshot")

  let ppx =
    let open Filename in
    concat ([ current_dir_name; "_build"; "default"; "ppx"; "bin" ] |> join) "bin.exe"
  ;;

  let bsc =
    let open Filename in
    concat ([ current_dir_name; "node_modules"; "rescript" ] |> join) "bsc"
  ;;

  let rescript_react =
    let open Filename in
    [ current_dir_name; "node_modules"; "@rescript"; "react"; "lib"; "ocaml" ] |> join
  ;;

  let re_formality =
    let open Filename in
    [ current_dir_name; "node_modules"; "re-formality"; "lib"; "ocaml" ] |> join
  ;;
end

module Bsc = struct
  let errors = "+A"

  let cmd case =
    let open Path in
    bsc
    ^ " -ppx "
    ^ ppx
    ^ " -I "
    ^ re_formality
    ^ " -I "
    ^ rescript_react
    ^ " -w "
    ^ errors
    ^ " -warn-error "
    ^ errors
    ^ " -uncurried "
    ^ " -color never "
    ^ " -bs-cmi-only "
    ^ (case |> source)
  ;;
end

let env =
  let path () = "PATH=" ^ Sys.getenv "PATH" in
  let systemroot () = "SYSTEMROOT=" ^ Sys.getenv "SYSTEMROOT" in
  match Sys.os_type with
  | "Win32" -> [| path (); systemroot () |]
  | _ -> [| path () |]
;;

let read_from_channel channel =
  let buffer = Buffer.create 1024 in
  let newline = "\n" in
  (try
     while true do
       channel |> input_line |> Buffer.add_string buffer;
       newline |> Buffer.add_string buffer
     done
   with
   | End_of_file -> ());
  buffer |> Buffer.contents
;;

let run_bsc case =
  let stdout, stdin, stderr = Unix.open_process_full (case |> Bsc.cmd) env in
  let res = stdout |> read_from_channel, stderr |> read_from_channel in
  Unix.close_process_full (stdout, stdin, stderr) |> ignore;
  res
;;

let diff_error_snapshot case =
  let actual = case |> Bsc.cmd in
  let snapshot = case |> Path.snapshot in
  let cmd =
    actual
    ^ " 2>&1"
    ^ (match Sys.os_type with
       | "Win32" -> {| | sed "s|\\test\\cases\\|/test/cases/|g"|}
       | _ -> "")
    ^ {| | diff --ignore-blank-lines --ignore-space-change |}
    ^ snapshot
    ^ {| -|}
  in
  let stdout, stdin, stderr = Unix.open_process_full cmd env in
  let res = stdout |> read_from_channel, stderr |> read_from_channel in
  Unix.close_process_full (stdout, stdin, stderr) |> ignore;
  res
;;

let ok case = { actual = case |> run_bsc; expected = nothing, nothing }
let error case = { actual = case |> diff_error_snapshot; expected = nothing, nothing }

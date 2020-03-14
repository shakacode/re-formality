type result = {
  actual: (string, string),
  expected: (string, string),
};

let testable = Alcotest.(pair(string, string));
let nothing = "";

module Path = {
  let join = xs => xs |> String.concat(Filename.dir_sep);

  let test_cases_dir = Filename.([current_dir_name, "test", "cases"] |> join);

  let source = x => Filename.concat(test_cases_dir, x ++ ".re");
  let snapshot = x => Filename.concat(test_cases_dir, x ++ ".snapshot");

  let ppx =
    Filename.(
      concat(
        [current_dir_name, "_build", "default", "bin"] |> join,
        "bin.exe",
      )
    );
  let bsc =
    Filename.(
      concat([current_dir_name, "node_modules", ".bin"] |> join, "bsc")
    );
  let refmt =
    Filename.(
      concat([current_dir_name, "node_modules", ".bin"] |> join, "bsrefmt")
    );
  let reason_react =
    Filename.(
      [parent_dir_name, "node_modules", "reason-react", "lib", "ocaml"]
      |> join
    );
  let re_formality =
    Filename.(
      [parent_dir_name, "node_modules", "re-formality", "lib", "ocaml"]
      |> join
    );
};

module Bsc = {
  let errors = "+A";

  let cmd = case =>
    Path.(
      bsc
      ++ " -ppx "
      ++ ppx
      ++ " -I "
      ++ re_formality
      ++ " -I "
      ++ reason_react
      ++ " -w "
      ++ errors
      ++ " -warn-error "
      ++ errors
      ++ " -color never"
      ++ " -bs-cmi-only "
      ++ (case |> source)
    );
};

let env = {
  let path = () => "PATH=" ++ Sys.getenv("PATH");
  let systemroot = () => "SYSTEMROOT=" ++ Sys.getenv("SYSTEMROOT");
  switch (Sys.os_type) {
  | "Win32" => [|path(), systemroot()|]
  | _ => [|path()|]
  };
};

let read_from_channel = channel => {
  let buffer = Buffer.create(1024);
  let newline = "\n";
  try(
    while (true) {
      channel |> input_line |> Buffer.add_string(buffer);
      newline |> Buffer.add_string(buffer);
    }
  ) {
  | End_of_file => ()
  };

  buffer |> Buffer.contents;
};

let run_bsc = case => {
  let (stdout, stdin, stderr) = Unix.open_process_full(case |> Bsc.cmd, env);

  let res = (stdout |> read_from_channel, stderr |> read_from_channel);

  Unix.close_process_full((stdout, stdin, stderr)) |> ignore;

  res;
};

let diff_error_snapshot = case => {
  let actual = case |> Bsc.cmd;
  let snapshot = case |> Path.snapshot;

  let cmd =
    actual
    ++ " 2>&1"
    ++ (
      // FIXME: It doesn't work on CI
      switch (Sys.os_type) {
      | "Win32" => {| | sed "s|\\test\\cases\\|/test/cases/|g"|}
      | _ => ""
      }
    )
    ++ {| | diff --ignore-blank-lines --ignore-space-change |}
    ++ snapshot
    ++ {| -|};

  let (stdout, stdin, stderr) = Unix.open_process_full(cmd, env);

  let res = (stdout |> read_from_channel, stderr |> read_from_channel);

  Unix.close_process_full((stdout, stdin, stderr)) |> ignore;

  res;
};

let ok = case => {actual: case |> run_bsc, expected: (nothing, nothing)};

let error = case => {
  actual: case |> diff_error_snapshot,
  expected: (nothing, nothing),
};

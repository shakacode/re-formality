type result = {
  actual: (string, string),
  expected: (string, string),
};

let testable = Alcotest.(pair(string, string));
let nothing = "";

module Case = {
  let join = xs => xs |> String.concat(Filename.dir_sep);
  let dir = Filename.([current_dir_name, "test", "cases"] |> join);
  let source = x => Filename.concat(dir, x ++ ".re");
  let output = x => Filename.concat(dir, x ++ ".output");

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

  let errors = "+A";

  let build = case => {
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
    ++ " -color never "
    ++ " -bs-cmi-only "
    ++ (case |> source);
  };
};

// Uh not pretty
module Win = {
  // Since paths in snapshots are written on macOS,
  // snapshot wouldn't match bsc output on Windows machine.
  // Patching it up for now.
  let patch_snapshot = x =>
    x
    |> Str.global_replace(
         Str.regexp_string("/test/cases/"),
         "\\\\test\\\\cases\\\\",
       );

  // For some reason bsc output contains double EOL
  // in some places in output. Reducing those to one.
  let patch_process_output = x =>
    x |> Str.global_replace(Str.regexp_string("\n\n"), "\n");
};

let read = channel => {
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

  let output = buffer |> Buffer.contents;
  switch (Sys.os_type) {
  | "Win32" => output |> Win.patch_process_output
  | _ => output
  };
};

let actual = case => {
  let env = {
    let path = () => "PATH=" ++ Sys.getenv("PATH");
    let systemroot = () => "SYSTEMROOT=" ++ Sys.getenv("SYSTEMROOT");
    switch (Sys.os_type) {
    | "Win32" => [|path(), systemroot()|]
    | _ => [|path()|]
    };
  };

  let (stdout, stdin, stderr) =
    Unix.open_process_full(case |> Case.build, env);

  let res = (stdout |> read, stderr |> read);

  Unix.close_process_full((stdout, stdin, stderr)) |> ignore;

  res;
};

let output = case => {
  let file = case |> Case.output |> open_in;
  let size = file |> in_channel_length;
  let buf = size |> Bytes.create;
  try(really_input(file, buf, 0, size)) {
  | End_of_file => ()
  };
  file |> close_in;
  let output = buf |> Bytes.to_string;
  switch (Sys.os_type) {
  | "Win32" => output |> Win.patch_snapshot
  | _ => output
  };
};

let ok = case => {actual: case |> actual, expected: (nothing, nothing)};

let error = case => {
  actual: case |> actual,
  expected: (nothing, case |> output),
};

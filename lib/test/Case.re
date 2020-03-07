type result = {
  actual: (string, string),
  expected: (string, string),
};

let testable = Alcotest.(pair(string, string));
let nothing = "";

module Case = {
  let dir = "test/cases/";
  let source = x => dir ++ x ++ ".re";
  let output = x => dir ++ x ++ ".output";

  let ppx = "_build/default/bin/bin.exe";
  let bsc = "node_modules/.bin/bsc";
  let refmt = "node_modules/.bin/bsrefmt";
  let reason_react = "../node_modules/reason-react/lib/ocaml";
  let re_formality = "../node_modules/re-formality/lib/ocaml";
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
    ++ " -bs-cmi-only "
    ++ (case |> source);
  };
};

let read = channel => {
  let buffer = Buffer.create(1024);
  try(
    while (true) {
      channel |> input_line |> Buffer.add_string(buffer);
      "\n" |> Buffer.add_string(buffer);
    }
  ) {
  | End_of_file => ()
  };

  buffer |> Buffer.contents;
};

let actual = case => {
  let (stdout, stdin, stderr) =
    Unix.open_process_full(
      Case.(case |> build),
      [|"PATH=/usr/local/bin/"|] // TODO: Node must be available
    );

  let res = (stdout |> read, stderr |> read);

  Unix.close_process_full((stdout, stdin, stderr)) |> ignore;

  res;
};

let output = case => {
  let file = case |> Case.output |> open_in;
  let size = file |> in_channel_length;
  let buf = size |> Bytes.create;
  really_input(file, buf, 0, size);
  file |> close_in;
  buf |> Bytes.to_string;
};

let ok = case => {actual: case |> actual, expected: (nothing, nothing)};

let error = case => {
  actual: case |> actual,
  expected: (nothing, case |> output),
};

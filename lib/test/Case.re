type result = {
  actual: string,
  expected: string,
};

let testable = Alcotest.string;

module Case = {
  let dir = "test/cases/";
  let source = x => dir ++ x ++ ".re";
  let output = x => dir ++ x ++ ".output";
  let build = x =>
    "node_modules/.bin/bsc -color never -ppx _build/default/bin/bin.exe " ++ x;
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
      Case.(case |> source |> build),
      [|"PATH=/usr/local/bin/"|] // TODO: Node must be available
    );

  let res = stderr |> read;

  Unix.close_process_full((stdout, stdin, stderr)) |> ignore;

  res;
};

let expected = case => {
  let file = case |> Case.output |> open_in;
  let size = file |> in_channel_length;
  let buf = size |> Bytes.create;
  really_input(file, buf, 0, size);
  file |> close_in;
  buf |> Bytes.to_string;
};

let run = case => {actual: case |> actual, expected: case |> expected};

type result = {
  actual: (string, string),
  expected: (string, string),
};

let testable: Alcotest.testable((string, string));

let ok: string => result;
let error: string => result;

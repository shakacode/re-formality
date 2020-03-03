type result = {
  actual: string,
  expected: string,
};
let testable: Alcotest.testable(string);
let run: string => result;

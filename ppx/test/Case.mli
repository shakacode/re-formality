type result =
  { actual : string * string
  ; expected : string * string
  }

val testable : (string * string) Alcotest.testable
val ok : string -> result
val error : string -> result

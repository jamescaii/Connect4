exception Overflow;



let (+) = (a: int, b: int): int => {
  let c = a + b;
  if (a lxor b lor (a lxor lnot(c)) < 0) {
    c;
  } else {
    raise(Overflow);
  };
};

let (-) = (a: int, b: int): int => {
  let c = a - b;
  if (a lxor lnot(b) lor (b lxor c) < 0) {
    c;
  } else {
    raise(Overflow);
  };
};

let ( * ) = (a: int, b: int): int => {
  let c = a * b;
  if (Int64.of_int(c) == Int64.mul(Int64.of_int(a), Int64.of_int(b))) {
    c;
  } else {
    raise(Overflow);
  };
};

let (/) = (a: int, b: int): int =>
  if (a == min_int && b == (-1)) {
    raise(Overflow);
  } else {
    a / b;
  };

let (~-) = (x: int): int =>
  if (x != min_int) {
    - x;
  } else {
    raise(Overflow);
  };

/* take in a string, s, and print it with green color */
let printGreen = (s: string): unit =>
  print_string("\027[32m" ++ s ++ "\027[0m\n");

/* take in a string, s, and print it with red color */
let printRed = (s: string): unit =>
  print_string("\027[31m" ++ s ++ "\027[0m\n");

type result('a) =
  | Actual_Result('a)
  | Expected_Result('a)
  | Actual_Error(string)
  | Expected_Error(string);

type check_result('a) =
  | Test_Passed
  | Test_Failed(result('a), result('a));

/* checkExpect
  Inputs: actual and expected, two 'a and message, a string
  Output: Test_Passed if actual and expected are equal, or Test_Failed otherwise */
  let checkExpect = (actual: 'a, expected: 'a, message: string): check_result('a) =>
  if (actual == expected) {
    printGreen("ce_Success: " ++ message);
    Test_Passed;
  } else {
    printRed("ce_Fail: " ++ message);
    printRed("expected output: ");
    Js.log(expected);
    printRed("actual output: ");
    Js.log(actual)
    Test_Failed(Actual_Result(actual), Expected_Result(expected));
  };



  
/* checkError
  Input: a one-argument procedure 'thunk' that returns the thing you want to test when it's applied to an int
          and a string of the error message of the 'failwith' clause in the procedure
   Output: a Test_Passed or Test_Failed */
let checkError = (input: unit => 'a, expect: string): check_result('a) =>
  /* ignore(input()); */
  try (
    {
      Test_Failed(Actual_Result(input()), Expected_Error(expect));
    }
  ) {
  | Failure(err) when err == expect =>
    printGreen("ce_Success ");
    Test_Passed;
  | Failure(err) =>
    printRed("err_Fail ");
    Test_Failed(Actual_Error(err), Expected_Error(expect));
  };


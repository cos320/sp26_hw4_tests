open Util.Assert
module Typechecker = Oat.Typechecker
module Tctxt = Oat.Tctxt

(** TYPECHECKING UNIT TESTS *)

(*
  Define your test cases below, following the model of example_unit_tests1, then
  add your test cases to the all_student_unit_tests below. (Marked "TODO")

  * The (OCaml) name you give to your test cases should include your group name.

  * Please write unit tests for a judgment represented by one of the following
    functions in Typechecker.ml:

    subtype
    typecheck_exp 
    typecheck_stmt
*)

let example_unit_tests1 = [
  "subtype: |- string? <: string?",
   (fun () ->
       if Typechecker.subtype Tctxt.empty (TNullRef RString) (TNullRef RString) then ()
       else failwith "should not fail")
; ("no subtype: |- string? <: string",
   (fun () ->
       if Typechecker.subtype Tctxt.empty (TNullRef RString) (TRef RString) then
         failwith "should not succeed" else ())
  )
]

let example_unit_tests2 = [
  "subtype: |- int <: int",
   (fun () ->
       if Typechecker.subtype Tctxt.empty TInt TInt then ()
       else failwith "should not fail")
; ("no subtype: |- int <: bool",
   (fun () ->
       if Typechecker.subtype Tctxt.empty TInt TBool then
         failwith "should not succeed" else ())
  )
]


(* Ben Aepli and Vedant Badoni's tests. *)
let googlers_tests = [
  (* TODO *)
]


(* TODO: Add your test cases to this list. *)
let all_student_unit_tests =
  example_unit_tests1 @ 
  example_unit_tests2 @ 
  googlers_tests

let rec n_ones n =
  match n with
  | 0 -> ""
  | n -> "1" ^ n_ones (n - 1)

(** COMPLEX TESTS *)

(** See the project instructions for more details about test case
    requirements.

    Add your test case to this list.

    Each test case is a triple of the form:
    (<filename>,<stdin>,<expected_stdout>)

    - <filename> should name an *.oat file that appears in this directory
      It should should declare the standard oat entry point:

      int program(int argc, string[] argv)

    - <stdin> should be the string passed as the command-line arguments
      to the executable generaged by compiling <filename> with
      the following sequence of commands:

      ./oatc <filename> bin/runtime.c
      ./a.out <stdin>

    - <expected_stdout> is the string representing the expected result
      obtained by running the compiled <filename> on <stdin> and then
      concatenating the status code (0-255) returned by the call.

    You can use the command line to compile and run such tests like this:

    > ./oatc sp24_hw4_tests/demo_color.oat bin/runtime.c
    > ./a.out
    > echo $?
    20

    These test cases will be run via Gradedtests.oat_file_test.  For
    additional examples, see the tests/gradedtests.ml file.
*)
let bplus_tree_expected_4_128 = "quarter:\nx: -32y: 32z: -32\nthree quarters:\nx: 96y: -96z: 96\n 0"
let bplus_tree_expected_8_2048 = "quarter:\nx: -512y: 512z: -512\nthree quarters:\nx: 1536y: -1536z: 1536\n 0"
let bplus_tree_expected_128_64 = "quarter:\nx: -16y: 16z: -16\nthree quarters:\nx: 48y: -48z: 48\n 0"

let student_complex_tests : (string * string * string) list = [
    ("demo_color.oat", "", "20");
    ("bplus_tree.oat", Printf.sprintf "%s %s" (n_ones 4) (n_ones 128), bplus_tree_expected_4_128);
    ("bplus_tree.oat", Printf.sprintf "%s %s" (n_ones 8) (n_ones 2048), bplus_tree_expected_8_2048);
    ("bplus_tree.oat", Printf.sprintf "%s %s" (n_ones 128) (n_ones 64), bplus_tree_expected_128_64)
]

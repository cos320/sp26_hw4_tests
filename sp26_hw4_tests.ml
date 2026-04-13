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

(* Ayush Isaac (ai) Tests *)
let idx = "x"
let ai_test_pos = Oat.Ast.(
  no_loc @@ NewArrInit (
    TInt,
    no_loc @@ CInt 5L,
    idx,
    no_loc @@ Bop (
      Add,
      no_loc @@ Lhs(no_loc @@ Id idx),
      no_loc @@ CInt 1L
    )
  )
)

let ai_test_neg = Oat.Ast.(
  no_loc @@ NewArrInit (
    TInt,
    no_loc @@ CInt 5L,
    idx,
    no_loc @@ CBool true
  )
)

let ai_run_test (e:Oat.Ast.exp Oat.Ast.node) : bool =
  try
    let ty = Typechecker.typecheck_exp Tctxt.empty e in
    (ty = Oat.Ast.(TRef(RArray(TInt))))
  with Typechecker.TypeError _ -> false

let ayush_isaac_test = [
  ("TYP_NEWARRAYINIT: ⊢ new int[5]{x -> x + 1} : int[]",
    (fun () ->
        if ai_run_test ai_test_pos then ()
        else failwith "should not fail"))
  ; ("TYP_NEWARRAYINIT negative: ⊢ new int[5]{x -> true} : TypeError",
      (fun () ->
        if ai_run_test ai_test_neg then failwith "should not pass"
        else ()
    ))
]

(* Ben Aepli and Vedant Badoni's tests. *)
let googlers_struct_ctxt =
  Tctxt.empty
  |> fun c ->
  Tctxt.add_struct c "point"
    Oat.Ast.[ { fieldName = "x"; ftyp = TInt }; { fieldName = "y"; ftyp = TInt } ]

let googlers_tests = [
  ("typecheck CStruct: can have fields in diff order",
    (fun () ->
      let e = Oat.Ast.(no_loc @@ CStruct ("point", [
        ("y", no_loc @@ CInt 7L);
        ("x", no_loc @@ CInt 3L)
      ])) in
      let t = Typechecker.typecheck_exp googlers_struct_ctxt e in
      if t = Oat.Ast.(TRef (RStruct "point")) then ()
      else failwith "should typecheck to TRef"))
; ("typecheck CStruct: wrong field type is rejected",
    (fun () ->
      let e = Oat.Ast.(no_loc @@ CStruct ("point", [
        ("x", no_loc @@ CInt 3L);
        ("y", no_loc @@ CBool true)
      ])) in
      try
        let _ = Typechecker.typecheck_exp googlers_struct_ctxt e in
        failwith "should not pass"
      with Typechecker.TypeError _ -> ()))
]

(* Arnav Ambre and John Wu's tests below *)
let arnav_john_unit_tests = [
  "subtype func: |- (int, int) -> bool <: (int, int) -> bool",
   (fun () ->
       if Typechecker.subtype_func (Tctxt.empty) [TInt; TInt] (RetVal TBool) [TInt; TInt] (RetVal TBool) then ()
       else failwith "should not fail")
; ("no subtype func: |- (int, int) -> bool <: (bool, int) -> int",
   (fun () ->
       if Typechecker.subtype_func Tctxt.empty [TInt; TInt] (RetVal TBool) [TBool; TInt] (RetVal TInt) then
         failwith "should not succeed" else ())
  )
]

(* Daniel Yang (yanda-hw4) unit tests *)
let struct_subtype_ctxt =
  Tctxt.empty
  |> (fun c ->
  Tctxt.add_struct
    c
    "S2"
    Oat.Ast.[ { fieldName = "x"; ftyp = TInt }; { fieldName = "y"; ftyp = TInt } ])
  |> fun c ->
  Tctxt.add_struct
    c
    "S3"
    Oat.Ast.
      [ { fieldName = "x"; ftyp = TInt }
      ; { fieldName = "y"; ftyp = TInt }
      ; { fieldName = "z"; ftyp = TInt }
      ]
;;

let yanda_tests =
  [ ( "subtype (sub_subr_struct): S3 <: S2"
    , fun () ->
        if
          Typechecker.subtype
            struct_subtype_ctxt
            (TRef (RStruct "S3"))
            (TRef (RStruct "S2"))
        then ()
        else failwith "should not fail" )
  ; ( "no subtype (sub_subr_struct): S2 </: S3"
    , fun () ->
        if
          Typechecker.subtype
            struct_subtype_ctxt
            (TRef (RStruct "S2"))
            (TRef (RStruct "S3"))
        then failwith "should not succeed"
        else () )
  ]
;;

(* Raheem ( nico-nico-nii ) unit tests *)
let raheem_unit_test = [
  "subtype: |- string[] <: string[]",
   (fun () ->
       if Typechecker.subtype Tctxt.empty
           (TRef (RArray (TRef RString))) (TRef (RArray (TRef RString)))
       then () else failwith "should not fail")
   ; ("no subtype: |- string[] </: string?[]",
   (fun () ->
       if Typechecker.subtype Tctxt.empty
           (TRef (RArray (TRef RString))) (TRef (RArray (TNullRef RString)))
       then failwith "should not succeed" else ())
  )
]

(* Will Grace unit tests *)
let will_grace_cast = Oat.Ast.(
    no_loc @@ Cast (
      RArray TInt,
      "a",
      no_loc (Lhs (no_loc (Id "arr"))),
      [],
      []
    )
  )

let will_grace_unit_tests = [
  ( "ifq: |- arr:int[]?, if?(int[] a=arr){} else{}"
  , (fun () ->
     let open Oat.Ast in
     let tc = Tctxt.add_local Tctxt.empty "arr" (TNullRef (RArray TInt)) in
     let _ =
       Typechecker.typecheck_stmt tc will_grace_cast (RetVal TInt)
     in
     ()))
; ( "no ifq: |- arr:int[], if?(int[] a=arr){} else{}"
  , (fun () ->
     let open Oat.Ast in
     let tc = Tctxt.add_local Tctxt.empty "arr" (TRef (RArray TInt)) in
     try
       let _ =
         Typechecker.typecheck_stmt tc will_grace_cast (RetVal TInt)
       in
       failwith "negative test succeeded"
     with
     | Typechecker.TypeError _ -> ()
    ))
]

let colin_jishnu_tests =
  [ ("typ_bop: ⊢ 0+1: int",
    fun () ->
      let exp = Oat.Ast.(no_loc (Bop (Add, (no_loc (CInt 0L)), (no_loc (CInt 1L))))) in
      if Typechecker.typecheck_exp Tctxt.empty exp = TInt then () else failwith "should succeed"
    );
    ("typ_intOps Negative: ⊢ false+1: TypeError",
      fun () ->
        let exp = Oat.Ast.(no_loc (Bop (Add, (no_loc (CBool false)), (no_loc (CInt 1L))))) in
        try
          let _ = Typechecker.typecheck_exp Tctxt.empty exp in
          failwith "Should fail"
        with
        | Typechecker.TypeError _ -> ()
        | _ -> failwith "exception other than TypeError"
    )
  ]
;;

(* Team whycantiuseapreviousteamname - Yichi Zhang tests *)
let yichi_zhang_unit_tests = [
  ("subtype fptr: |- (string?) -> string <: (string) -> string?",
   (fun () ->
      let open Oat.Ast in
      let t1 = TRef (RFun ([TNullRef RString], RetVal (TRef RString))) in
      let t2 = TRef (RFun ([TRef RString], RetVal (TNullRef RString))) in
      if Typechecker.subtype Tctxt.empty t1 t2 then ()
      else failwith "should not fail"));
  ("no subtype fptr: |- (string) -> string? </: (string?) -> string",
   (fun () ->
      let open Oat.Ast in
      let t1 = TRef (RFun ([TRef RString], RetVal (TNullRef RString))) in
      let t2 = TRef (RFun ([TNullRef RString], RetVal (TRef RString))) in
      if Typechecker.subtype Tctxt.empty t1 t2 then
        failwith "should not succeed"
      else ()))
]

(* Hita Gupta's unit tests *)
let hitagu_proj_ctxt =
  Tctxt.empty
  |> fun c -> Tctxt.add_struct c "Point"
    Oat.Ast.[{fieldName = "x"; ftyp = TInt}; {fieldName = "y"; ftyp = TBool}]

let hitagu_point_exp =
  Oat.Ast.(no_loc @@ CStruct ("Point",
    [("x", no_loc @@ CInt 1L); ("y", no_loc @@ CBool true)]))

let hitagu_tests = [
  ("proj: ⊢ (new Point{x=1;y=true}).x : int",
    (fun () ->
      match
        (try Some (Typechecker.typecheck_exp hitagu_proj_ctxt
          Oat.Ast.(no_loc @@ Lhs (no_loc @@ Proj (hitagu_point_exp, "x"))))
         with Typechecker.TypeError _ -> None)
      with
      | Some Oat.Ast.TInt -> ()
      | Some _ -> failwith "expected TInt"
      | None -> failwith "unexpected TypeError"))
  ; ("proj negative: ⊢ (new Point{x=1;y=true}).z : TypeError",
    (fun () ->
      match
        (try Some (Typechecker.typecheck_exp hitagu_proj_ctxt
          Oat.Ast.(no_loc @@ Lhs (no_loc @@ Proj (hitagu_point_exp, "z"))))
         with Typechecker.TypeError _ -> None)
      with
      | None -> ()
      | Some _ -> failwith "expected TypeError for unknown field"))
]

(* TODO: Add your test cases to this list. *)
let all_student_unit_tests =
  example_unit_tests1 @
  example_unit_tests2 @
  googlers_tests @
  arnav_john_unit_tests @
  ayush_isaac_test @
  yanda_tests @
  raheem_unit_test @
  will_grace_unit_tests @
  colin_jishnu_tests @
  yichi_zhang_unit_tests @
  hitagu_tests

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

(* Ayush Isaac complex tests *)
let ai_text1 = "sigma sigma on the wall who’s the skibidiest of them all you are you are yes you are "
let ai_text2 = "Im over here taking a nap rn I got a pillow under my head rn sleeping so soundly Im a snoozer man"

let student_complex_tests : (string * string * string) list = [
    ("demo_color.oat", "", "20");
    ("bplus_tree.oat", Printf.sprintf "%s %s" (n_ones 4) (n_ones 128), bplus_tree_expected_4_128);
    ("bplus_tree.oat", Printf.sprintf "%s %s" (n_ones 8) (n_ones 2048), bplus_tree_expected_8_2048);
    ("bplus_tree.oat", Printf.sprintf "%s %s" (n_ones 128) (n_ones 64), bplus_tree_expected_128_64);

    (* Hita Gupta's tests *)
    ("linked_list.oat", "",  "1 1 2 3 3 4 5 5 6 9\n0");
    ("linked_list.oat", "1", "9 6 5 5 4 3 3 2 1 1\n0");
    ("linked_list.oat", "2", "2 2 4 6 6 8 10 10 12 18\n0");
    ("linked_list.oat", "3", "2 4 6\n0");
    ("linked_list.oat", "4", "39\n0");
    ("linked_list.oat", "5", "0 1 1 2 3 3 4 5 5 6 7 8 9\n0");
    ("linked_list.oat", "6", "9 6 5 5 4 3 3 2 1 1\n0");

    (* Raheem's complex test, implements Stalin Sort with Linked List *)
    (* See here: https://www.reddit.com/r/ProgrammerHumor/comments/9s9kgn/ *)
    ("stalin_sort.oat", "6 1 55 0 60 450 3", "6 55 60 450 0");
    ("stalin_sort.oat", "1 2 3 4 5", "1 2 3 4 5 0");
    ("stalin_sort.oat", "69 68 67 67 67 67", "69 0");

    ("will_grace_nodes.oat", "", "6");
    ("colin_jishnu.oat", "", "(4 (1 null null) (9 null null))\n 0");

    (* Isaac and Ayush *)
    ("hash_table_word_counter.oat", "Im " ^ ai_text2, "There were 2 occurences of the word Im.0");
    ("hash_table_word_counter.oat", "you " ^ ai_text1, "There were 3 occurences of the word you.0");
    ("hash_table_word_counter.oat", "are " ^ ai_text1 ^ ai_text1 ^ ai_text1 ^ ai_text1 ^ ai_text1 ^ ai_text1, "There were 18 occurences of the word are.0");

    (* Daniel Yang (yanda-hw4) *)
    ("trie.oat", "", "8 4 1 0 1 0 6 6 5 4 0 0");
    ("yichi_zhang_reverse_list.oat", "", "4 3 2 1 10");

    (* Arnav_john Test Case *)
    ("VEB_Sort.oat", "2", "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 0");
    ("VEB_Sort.oat", "3 3", "81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 0");
    ("VEB_Sort.oat", "4 4 4", "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 0")
]

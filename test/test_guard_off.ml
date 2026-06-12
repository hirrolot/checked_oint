open Checked_oint

let int_modules_list =
    Int_ty.all |> List.map (function Int_ty.Container ty -> (module (val ops ty) : S))
;;

let check_silent msg f =
    try f () with
    | Invalid_argument _ -> Alcotest.fail msg
;;

let monomorphic_ops_good () =
    int_modules_list
    |> List.iter (fun (module S : S) ->
      let x, y = S.of_int_exn 1, S.of_int_exn 2 in
      Alcotest.(check' bool) ~msg:"Good: `S.equal`" ~actual:S.(equal x x) ~expected:true;
      Alcotest.(check' bool)
        ~msg:"Good: not `S.equal`"
        ~actual:S.(equal x y)
        ~expected:false;
      Alcotest.(check' bool)
        ~msg:"Good: `S.compare`"
        ~actual:(S.compare x y < 0)
        ~expected:true)
;;

let polymorphic_ops_silent () =
    int_modules_list
    |> List.iter (fun (module S : S) ->
      let x, y = S.of_int_exn 1, S.of_int_exn 2 in
      check_silent "Good: `Stdlib.( = )`" (fun () -> ignore (Stdlib.( = ) x y));
      check_silent "Good: `Stdlib.( <> )`" (fun () -> ignore (Stdlib.( <> ) x y));
      check_silent "Good: `Stdlib.( < )`" (fun () -> ignore (Stdlib.( < ) x y));
      check_silent "Good: `Stdlib.( > )`" (fun () -> ignore (Stdlib.( > ) x y));
      check_silent "Good: `Stdlib.( <= )`" (fun () -> ignore (Stdlib.( <= ) x y));
      check_silent "Good: `Stdlib.( >= )`" (fun () -> ignore (Stdlib.( >= ) x y));
      check_silent "Good: `Stdlib.compare`" (fun () -> ignore (Stdlib.compare x y));
      check_silent "Good: `Stdlib.min`" (fun () -> ignore (Stdlib.min x y));
      check_silent "Good: `Stdlib.max`" (fun () -> ignore (Stdlib.max x y)))
;;

let () =
    Alcotest.run
      "Guard off"
      [ ( "Polymorphic comparison"
        , [ Alcotest.test_case "Monomorphic operators good" `Quick monomorphic_ops_good
          ; Alcotest.test_case
              "Polymorphic operators silent"
              `Quick
              polymorphic_ops_silent
          ] )
      ]
;;

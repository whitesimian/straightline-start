open Absyn

(*
  altura := 173
 *)

let prog1 = AssignStm ("altura", NumExp 173)


(*
  print(43, 7, 0)
 *)

let prog2 = PrintStm [NumExp 43; NumExp 7; NumExp 0]


(*
  x := 2 + 3;
  print(x)
 *)

let prog3 =
  CompoundStm (AssignStm ("x", OpExp (NumExp 2, Plus, NumExp 3)),
               PrintStm [IdExp "x"])


(*
  x := 2 + 3 * 4;
  print(x)

 *)

let prog4 =
  CompoundStm (AssignStm ("x",
                          OpExp (NumExp 2,
                                 Plus,
                                 OpExp (NumExp 3,
                                        Times,
                                        NumExp 4))),
               PrintStm [IdExp "x"])


(*
  a := 5 + 3;
  b := (print(a, a-1), 10*a);
  print(b)
 *)
 
 (* =============================================================================== *)
 (* TP 1 *)
 (* =============================================================================== *)
 
let rec fat n = 
  if n == 1 then 1 else n * fat (n - 1)
  
let prog = PrintStm [NumExp (fat 5)];;
    
interpret prog;;

maxargs prog;;

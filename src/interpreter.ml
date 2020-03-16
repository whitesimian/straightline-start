type memory = (id, int) Hashtbl.t

let rec run m prog =
  match prog with
  | CompoundStm (s1, s2) -> run m s1; run m s2
  | AssignStm (v, e) -> Hashtbl.replace m v (eval m e)
  | PrintStm list -> List.iter
                       (fun e -> print_int (eval m e); print_char ' ')
                       list;
      print_newline ()
        
  | ReadStm (v) -> 
      let x = read_int() in 
      run m (AssignStm (v, NumExp x)) 
                
  | CondStm (exp, s1, s2) ->
      if eval m exp == 0 then run m s2 else run m s1
          
  | WhileStm (exp, stm) ->
      while eval m exp != 0 do run m stm done
                     
and eval m exp =
  match exp with
  | NumExp cte -> cte
  | IdExp v -> ( try Hashtbl.find m v
                 with Not_found -> 0
               )
  | OpExp (e1, op, e2) -> let v1 = eval m e1 in
      let v2 = eval m e2 in
      ( match op with
        | Plus -> v1 + v2
        | Minus -> v1 - v2
        | Times -> v1 * v2
        | Div -> v1 / v2
      )
  | EseqExp (s, e) -> run m s; eval m e

let interpret prog =
  let m = Hashtbl.create 0 in
  run m prog 

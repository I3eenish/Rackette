#use "setup.ml" ;;
#use "rackette.ml" ;;

(* The Rackette REPL. Exits on (exit), catches errors. *)
let rec rackette_repl () =
   print_string "Rackette > ";
   (try
      let ip = read_line () in
        match ip with
        | "(exit)"  -> failwith "EXITLOOP"
        | _         -> List.iter print_endline (rackette ip)
    with
      | e -> (match e with
         | Failure("EXITLOOP") -> failwith "Exiting Rackette REPL"
         | Failure(str) -> print_endline ("Error: " ^ str)
         | _ -> print_endline "Error: Other exception failure")) ;
   rackette_repl () ;;

rackette_repl () ;;
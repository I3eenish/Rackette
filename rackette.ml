#use "read.ml" ;;
#use "setup.ml" ;;

(* an Rackette identifier *)
type identifier = ID of string ;;

(* a Rackette expression *)
type expression =
| NumE of int
| IdentE of identifier
| AndE of expression * expression
| OrE of expression * expression
| IfE of expression * expression * expression
| CondE of (expression * expression) list
| QuoteE of concrete_program_piece
| LambdaE of identifier list * expression
| ApplicationE of expression list ;;

type definition = identifier * expression ;;

(* a piece of Rackette that can be processed:
 * either a definition or an expression *)
type abstract_program_piece =
| Definition of definition
| Expression of expression ;;

(* a representation of a Rackette program --
 * any number of pieces *)
type abstract_program = abstract_program_piece list ;;

(* a Rackette value: the result of
 * evaluating a Rackette expression *)
type value =
| VNum of int
| VBool of bool
| VSymbol of string
| VList of value list
| VBuiltin of string * (value list -> value)
| VClosure of identifier list * expression * environment
and binding = identifier * value
and environment = binding list ;;

let add_func : value list -> value = function
  | [VNum i ; VNum j] -> VNum (i + j)
  | _ -> 
      failwith "+ expects two arguments, which must evaluate to numbers." ;;

let sub_func : value list -> value = function
  | [VNum i ; VNum j] -> VNum (i - j)
  | _ -> 
      failwith "- expects two arguments, which must evaluate to numbers." ;;

let mult_func : value list -> value = function
  | [VNum i ; VNum j] -> VNum (i * j)
  | _ -> 
      failwith "* expects two arguments, which must evaluate to numbers." ;;

let div_func : value list -> value = function
  | [VNum i ; VNum 0] -> failwith "Division by zero is not permitted."
  | [VNum i ; VNum j] -> VNum (i / j)
  | _ -> 
      failwith "/ expects two arguments, which must evaluate to numbers." ;;

let rem_func : value list -> value = function
  | [VNum i ; VNum 0] -> failwith "Second argument cannot be 0."
  | [VNum i ; VNum j] -> VNum (i mod j)
  | _ -> 
      failwith "remainder expects two arguments (eval to numbers)." ;;

let int_equal_func : value list -> value = function
  | [VNum i ; VNum j] -> VBool (i = j)
  | _ -> 
      failwith "= expects two arguments, which must evaluate to numbers." ;;

let less_than_func : value list -> value = function
  | [VNum i ; VNum j] -> VBool (i < j)
  | _ -> 
      failwith "< expects two arguments, which must evaluate to numbers." ;;

let greater_than_func : value list -> value = function
  | [VNum i ; VNum j] -> VBool (i > j)
  | _ -> 
      failwith "> expects two numbers, which must evaluate to numbers." ;;

let rec equal_func : value list -> value = function
  | [VNum i ; VNum j] -> VBool (i = j)
  | [VBool i ; VBool j] -> VBool (i = j)
  | [VSymbol i ; VSymbol j] -> VBool (i = j)
  | [VList i ; VList j] -> (match (i, j) with
      | [], [] -> VBool true
      | _, [] -> VBool false
      | [], _ -> VBool false
      | head1 :: tail1, head2 :: tail2 -> 
          (match equal_func [head1 ; head2] with
            | VBool true -> equal_func [VList tail1 ; VList tail2]
            | VBool false -> VBool false
            | _ -> failwith "match error."))
  | [x ; y] -> VBool false
  | _ -> failwith
          "equal? expects two numbers, symbols, or lists." ;;

let num_func : value list -> value = function
  | [VNum num] -> VBool true
  | _ -> VBool false ;;

let zero_func : value list -> value = function
  | [VNum x] -> if x = 0 then VBool true else VBool false
  | _ -> 
      failwith "zero? expects one argument (must eval to number)." ;; 

let cons_func : value list -> value = function
  | [VNum num ; VList x] -> VList (VNum num :: x)
  | [VBool boolean ; VList x] -> VList (VBool boolean :: x)
  | [VSymbol symbol ; VList x] -> VList (VSymbol symbol :: x)
  | [VList lst ; VList x] -> VList (VList lst :: x)
  | [x] -> failwith "cons expects two arguments, given one."
  | [] -> failwith "cons expects two arguments, given none."
  | [_ ; VList x] -> failwith "cons expects a number, bool, symbol, or list 
                      as first argument."
  | [x ; _] -> failwith "cons expects a list as second argument." 
  | _ -> failwith "cons expects two arguments - the first being a number,
            bool, symbol, or list and the second being a list." ;;

let car_func : value list -> value = function
  | [VList []] -> failwith "Cannot take the car of an empty list."
  | [VList (hd :: tl)] -> hd
  | _ -> failwith "car expects a single non-empty list." ;;

let cdr_func : value list -> value = function
  | [VList []] -> failwith "Cannot take the cdr of an empty list."
  | [VList (hd :: tl)] -> VList tl
  | _ -> failwith "cdr expects a single non-empty list." ;;

let empty_func : value list -> value = function
  | [VList x] -> VBool (x = [])
  | _ -> failwith "empty? expects a single list." ;;

let filled_func : value list -> value = function
  | [VList x] -> VBool (not (x = []))
  | _ -> failwith "cons? expects a single list." ;;

let not_func : value list -> value = function
  | VBool head :: [] -> VBool (not head)
  | head :: [] -> failwith "not expects a bool, either true or false."
  | _ -> failwith "not expects one argument." ;;

let initial_tle : environment = 
  [(ID "+", VBuiltin ("<builtin:add>", add_func)) ;
  (ID "-", VBuiltin ("<builtin:subtract>", sub_func)) ; 
  (ID "*", VBuiltin ("<builtin:multiply>", mult_func));
  (ID "/", VBuiltin ("<builtin:divide>", div_func)) ; 
  (ID "true", VBool true) ;
  (ID "false", VBool false) ;
  (ID "remainder", VBuiltin ("<builtin:remainder>", rem_func)) ;
  (ID "=", VBuiltin ("<builtin:=>", int_equal_func)) ;
  (ID "<", VBuiltin ("<builtin:less_than>", less_than_func)) ;
  (ID ">", VBuiltin ("<builtin:greater_than>", greater_than_func)) ;
  (ID "equal?", VBuiltin ("<builtin:equal?>", equal_func)) ;
  (ID "number?", VBuiltin ("<builtin:number?>", num_func)) ;
  (ID "zero?", VBuiltin ("<builtin:zero?>", zero_func)) ;
  (ID "cons", VBuiltin ("<builtin:cons>", cons_func)) ;
  (ID "car", VBuiltin ("<builtin:car>", car_func)) ;
  (ID "cdr", VBuiltin ("<builtin:cdr>", cdr_func)) ;
  (ID "empty?", VBuiltin ("<builtin:empty?>", empty_func)) ;
  (ID "cons?", VBuiltin ("<builtin:cons?>", filled_func)) ;
  (ID "not", VBuiltin ("<builtin:not>", not_func))] ;;

let rec parse_ID_list : concrete_program_piece list -> identifier list = 
  function
    | [] -> failwith "No arguments in lambda expression provided"
    | Symbol s :: [] -> [(ID s)]
    | Symbol s :: rest -> (ID s) :: (parse_ID_list rest)
    | _ :: rest -> failwith "Lambda expects symbols as arguments" ;;

let rec parse_expression : concrete_program_piece -> expression = function
  | Number num -> NumE num
  | Symbol symbol -> IdentE (ID symbol)
  | List ((Symbol "and") :: tail) -> (match tail with
      | expr1 :: expr2 :: [] ->
        AndE ((parse_expression expr1), (parse_expression expr2))
      | expr1 :: [] ->
        failwith "and expects two arguments, given one."
      | expr1 :: expr2 :: rest ->
        failwith "and expects two arguments, given more."
      | [] ->
        failwith "and expects two arguments, given none.")
  | List ((Symbol "or") :: tail) -> (match tail with
      | expr1 :: expr2 :: [] ->
        OrE ((parse_expression expr1), (parse_expression expr2))
      | expr1 :: [] ->
        failwith "or expects two arguments, given one."
      | expr1 :: expr2 :: rest ->
        failwith "or expects two arguments, given more."
      | [] ->
        failwith "or expects two arguments, given none.")
  | List ((Symbol "if") :: tail) -> (match tail with
      | pred :: yes_exp :: no_exp :: [] ->
        IfE ((parse_expression pred),
             (parse_expression yes_exp), (parse_expression no_exp))
      | pred :: [] ->
        failwith "if expects three arguments, given one."
      | pred :: yes_exp :: [] ->
        failwith "if expects three arguments, given two."
      | pred :: yes_exp :: no_exp :: rest ->
        failwith "if expects three arguments, given more."
      | [] ->
        failwith "if expects three arguments, given none.")
  | List ((Symbol "cond") :: rest) -> (match rest with
      | head :: tail -> 
          CondE (List.map (function
           | (List [pred; expr]) -> 
              ((parse_expression pred), (parse_expression expr))
           | _ -> failwith "error.") (head :: tail))
      | [] -> 
          failwith "cond expects a list of expression pairs, given nothing.")
  | List ((Symbol "quote") :: tail) -> (match tail with
      | cpp :: [] -> QuoteE (cpp)
      | cpp :: rest -> failwith "quote expects one argument, given more."
      | [] -> failwith "quote expects one argument, given none.")
  | List ((Symbol "lambda") :: tail) -> (match tail with
      | List (id_lst) :: body :: [] -> 
          LambdaE ((parse_ID_list id_lst), (parse_expression body))
      | List(id_lst) :: rest -> 
        failwith "Lambda expects an argument list and body, given more."
      | _ ->  failwith "Lambda expects an argument list and body.")
  | List ((Symbol proc) :: tail) -> (match tail with
    | [] -> failwith "Procedure expects an argument, given nothing."
    | head :: tail -> 
        ApplicationE (IdentE (ID proc) :: (List.map (function expr -> 
          (parse_expression expr)) (head :: tail))))
  | List (List ((Symbol "lambda") :: tail) :: rest) -> (match rest with
      | [] -> failwith "error. Procedure expects at least one argument."
      | hd :: tl -> ApplicationE 
          ((parse_expression (List ((Symbol "lambda") :: tail))) :: 
            (List.map (function cpp -> (parse_expression cpp)) rest)))
  | List [] -> failwith "List expects arguments, given nothing"
  | _ -> failwith "error!" ;;

let parse_definition : concrete_program_piece -> definition = function
  | List ((Symbol "define") :: tail) -> (match tail with
      | Symbol x :: expr :: [] -> (ID x, (parse_expression expr))
      | a :: [] -> failwith "define expects an expression, given nothing"
      | a :: b :: rest ->
          failwith "define expects an ID and expression, given more"
      | [] -> failwith "define expects an ID and expression, given nothing")
  | _ -> failwith "error." ;;

let parse_piece : concrete_program_piece -> abstract_program_piece = function
  cpp -> match cpp with
  | List (Symbol "define" :: _) -> Definition (parse_definition cpp)
  | _ -> Expression (parse_expression cpp) ;;

let parse : concrete_program -> abstract_program = function
  cp -> List.map parse_piece cp ;;

let rec look_up : environment * expression -> value = function
  | [], IdentE (ID ident) -> failwith "Symbol is not bound to any value."
  | (ID id, value) :: tail, IdentE (ID ident) -> 
      if ident = id then value else look_up (tail, IdentE (ID ident))
  | _ -> failwith "error." ;;

let rec bind_f_a : identifier list * value list * environment -> environment = 
  function (formals, actuals, env) -> (match formals, actuals with
    | [], [] -> env
    | [], _ -> failwith "error. Not enough arguments provided."
    | _, [] -> failwith "error. Too many arguments provided."
    | head1 :: tail1, head2 :: tail2 -> 
        (head1, head2) :: bind_f_a (tail1, tail2, env)) ;;

let rec eval : (environment * environment * expression) -> value = function
    (tle, env, expr) -> (match (expr) with
      | NumE n -> VNum n
      | IdentE (ID ident) -> look_up (env@tle, IdentE (ID ident))
      | AndE (expr1, expr2) -> (match eval (tle, env, expr1) with
        | VBool false -> VBool false
        | VBool true -> (match eval (tle, env, expr2) with
          | VBool true -> VBool true
          | VBool false -> VBool false
          | _ -> failwith "Second argument must evaluate to bool.")
        | _ -> failwith "First argument must evaluate to bool.")
      | OrE (expr1, expr2) -> (match eval (tle, env, expr1) with
        | VBool true -> VBool true
        | VBool false -> (match eval (tle, env, expr2) with
          | VBool true -> VBool true
          | VBool false -> VBool false
          | _ -> failwith "Second argument must evaluate to bool.")
        | _ -> failwith "First argument must evaluate to bool." )
      | IfE (pred, yes_exp, no_exp) -> (match eval (tle, env, pred) with
        | VBool true -> eval (tle, env, yes_exp)
        | VBool false -> eval (tle, env, no_exp)
        | _ -> failwith "if expects bool as first argument.")
      | CondE (lst) -> 
          let rec c_eval : (expression * expression) list -> value = function
            | [] -> failwith "cond expects a clause, given empty"
            | (pred, expr) :: [] -> 
                (match eval (tle, env, pred) with
                  | VBool true -> eval (tle, env, expr)
                  | VBool false -> failwith "All question results were false."
                  | _ -> failwith "Question result is not true or false.")
            | (pred, expr) :: tail ->
                (match eval (tle, env, pred) with
                  | VBool true -> eval (tle, env, expr)
                  | VBool false -> c_eval tail
                  | _ -> failwith "Question result is not true or false.") in
          (c_eval lst)
      | QuoteE (cpp) ->
          let rec q_eval : concrete_program_piece -> value = function
            | Number num -> VNum num
            | Symbol symbol -> VSymbol symbol
            | List lst -> (match lst with
                | [] -> VList []
                | hd :: tl -> 
                    VList (List.map (function x -> q_eval x) (hd :: tl))) in
          (q_eval cpp)
      | LambdaE (x, y) -> (match x,y with
          | id_list, expr -> VClosure (id_list, expr, env))
      | ApplicationE (lst) -> (match (lst) with
          | [] -> failwith "error. List is empty."
          | proc :: [] -> 
              failwith "Procedure expects arguments but none were provided."
          | proc :: rest -> (match eval (tle, env, proc) with
              | VBuiltin (str, func) -> 
                  func (List.map (fun expr -> eval (tle, env, expr)) rest) 
              | VClosure (id_list, body, birth) -> 
                  let local = (bind_f_a (id_list, 
                        (List.map (fun expr -> eval (tle, env, expr)) rest), 
                      env)) in
                  (eval (tle, local@birth, body))
              | _ -> failwith "error. Function not provided."))) ;;

let rec add_definition_helper : environment * identifier -> bool = function
  | [], ID ident -> true 
  | (ID id, value) :: tail, ID ident -> 
      if id = ident then false 
      else add_definition_helper (tail, ID ident) ;;

let add_definition : environment * definition -> environment = function
  | tle, (ID ident, expr) ->
      if (add_definition_helper (tle, ID ident)) then 
        (ID ident, (eval (tle, [], expr))) :: tle
      else failwith "error. Symbol is already bound to another value." ;;

let rec string_of_value_helper : string list -> string = function
  | [] -> ")"
  | head :: [] -> head ^ (string_of_value_helper [])
  | head :: tail -> head ^ " " ^ (string_of_value_helper tail) ;;

let rec string_of_value : value -> string = function
  | VNum n -> string_of_int n
  | VBool b -> "#" ^ string_of_bool b
  | VSymbol s -> s
  | VList lst -> "(" ^ string_of_value_helper (List.map string_of_value lst) 
  | VBuiltin (str, func) -> str
  | VClosure (id_lst, expr, env) -> "#<procedure>" ;;

(* process: this procedure processes the abstract_program
 * representation of a Rackette program following the
 * Rackette rules of processing
 * I/P: an abstract_program representation of a Rackette program
 * O/P: the list of values corresponding to
 * the evaluation of any expressions present in pieces *)
let rec process_helper: environment * abstract_program -> value list = function
  (tle, pieces) -> match pieces with
  | [] -> []
  | (Definition def) :: tl -> process_helper ((add_definition (tle, def)), tl)
  | (Expression expr) :: tl -> 
      eval (tle, [], expr) :: process_helper (tle, tl);;

let process : abstract_program -> value list = function
  pieces -> process_helper (initial_tle, pieces);;

(* rackette: this procedure will interpret a Rackette program
 * and return its value as a string, if it has one
 * I/P: a Rackette program represented as a string, program
 * O/P: a list of the string representations of
 *      the evaluated Rackette expressions in programs *)
let rackette : string -> string list = function
  program -> List.map string_of_value (process (parse (read_all program))) ;;

(* test cases *)

check_expect (rackette "(define x 10) (+ x 15)") ["25"] ;;
check_expect (rackette "(empty? (quote ()))") ["#true"] ;;
check_expect (rackette "5") ["5"] ;;
check_expect (rackette "(or true x)") ["#true"] ;;
check_expect (rackette "(+ 1 2)") ["3"] ;;
check_expect (rackette "(quote hi)") ["hi"] ;;
check_expect (rackette "(if (zero? 0) (+ 6 0) (+ 6 1))") ["6"] ;;
check_expect (rackette "(define x 15) (if (= 16 x) 1000 -1)") ["-1"] ;;
check_expect (rackette "(lambda (x) (+ x 0))") ["#<procedure>"] ;;
check_expect (rackette "(quote ())") ["()"] ;;
check_expect (rackette "(car (quote (10 9 8)))") ["10"] ;;
check_expect (rackette "(car (quote ((/ 100 10) 6 12)))") ["(/ 100 10)"] ;;
check_expect (rackette "(car (cdr (cdr (quote (1 2 3)))))") ["3"] ;;
check_expect (rackette "((lambda (x) (+ x 0)) 0)") ["0"] ;;
check_expect (rackette "(define x 25) ((lambda (x) (+ x 0)) 0)") ["0"] ;;
check_expect 
  (rackette 
    "(define sum-angles (lambda (n) (* 180 (- n 2)))) (sum-angles 8)") 
  ["1080"] ;;
check_expect 
  (rackette "((lambda (x y) ((lambda (y) (+ x y)) x)) 17 18)") ["34"] ;;
check_expect
  (rackette "((lambda (x y) ((lambda (x) (+ x y)) x)) 17 18)") ["35"] ;;
check_expect
  (rackette "((lambda (x y) ((lambda (x) (+ x y)) y)) 17 18)") ["36"] ;;
check_expect (rackette "(define x 3) ((lambda (y) (+ x y)) 5)") ["8"] ;;
check_expect (rackette "(define x 3) ((lambda (x) (+ x x)) 5)") ["10"] ;;
check_expect
  (rackette 
    "(define fact (lambda (x) (if (zero? x) 1 (* x (fact (- x 1)))))) 
      (fact 3)") 
  ["6"] ;;

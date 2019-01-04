type raw_program = string

type concrete_program_piece =
  | Number of int
  | Symbol of string
  | List of concrete_program_piece list

type concrete_program = concrete_program_piece list

module Reader : sig
	val read : raw_program -> concrete_program_piece
	val read_all : raw_program -> concrete_program
end =
struct
	(* I/P: a string, s
	 * O/P: a list of characters corresponding to s *)
	let rec char_list_of_string : string -> char list =
		let first (str : string) = String.get str 0
		and rest (str : string) =
			String.sub str 1 (pred (String.length str)) in
		function
	  | "" -> []
	  | str -> first str :: (char_list_of_string (rest str))

	(* I/P: a character, c
	 * O/P: Some ((int) c), or None, if c isn't a digit *)
	let get_digit : char -> int option = function
		| '0' -> Some 0 | '1' -> Some 1 | '2' -> Some 2
		| '3' -> Some 3 | '4' -> Some 4 | '5' -> Some 5
		| '6' -> Some 6 | '7' -> Some 7 | '8' -> Some 8
		| '9' -> Some 9 | _ -> None

	(* adds a digit, digit, to a number by making it the least sigficant digit *)
	let add_digit (digit : int) (number : int) = (number * 10) + digit

	(* I/P: a char, c
	 * O/P: true, if the character is allowable as a Racket identifier
	 * 			false, otherwise *)
	let is_legal_identifier_character : char -> bool = function
		| '`'  | '(' | ')' | '[' | ']' | '{' | '}'
		| '\\' | '|' | ';' | '\'' | '"' -> false (* add backslash *)
		| _ -> true

	(* I/P: a char list, chars, and current, a quoted_syntax option
	 * O/P: the quoted syntax representation of a number or symbol *)
	let rec read_atom (chars : char list)
									(current : concrete_program_piece option) : concrete_program_piece =
		match chars, current with
		| [], None -> failwith "vacuous expression"
		| [], Some cur -> cur
		| '-' :: '-' :: tl, None -> read_atom tl (Some (Symbol "--"))
		| '-' :: [], None -> Symbol "-"
		| '-' :: [], Some x ->
			(match x with
				| Symbol id -> Symbol (id ^ "-")
				| Number n -> Symbol ((string_of_int n) ^ "-")
				| List _ -> failwith "EMF")
		| '-' :: tl, None -> (match read_atom tl None with
													| Number n -> Number (-n)
													| Symbol s -> Symbol ("-" ^ s)
													| List _ -> failwith "EMF")
		| hd :: tl, None ->
			(match get_digit hd with
				| Some d -> read_atom tl (Some (Number d))
				| None -> if is_legal_identifier_character hd then
										read_atom tl (Some (Symbol (String.make 1 hd)))
									else failwith "invalid character identifier")
		| hd :: tl, Some (Number n) ->
			(match get_digit hd with
				| None -> read_atom tl
					(Some (Symbol ((string_of_int n) ^ (String.make 1 hd))))
				| Some d -> read_atom tl (Some (Number (add_digit d n))))
		| hd :: tl, Some (Symbol s) ->
			if is_legal_identifier_character hd then
				read_atom tl (Some (Symbol (s ^ (String.make 1 hd))))
			else failwith "invalid character identifier"
		| _, Some (List _) ->
			failwith "read_atom only handles atomic expressions"

	(* a representation of S-expressions *)
	type expression_tree =
		| Terminal of char list
		| Expression of expression_tree list

	(* trims whitespace from the beginning of a list of characters *)
	let trim_whitespace (input : char list) : char list =
		let rec trim_leading_whitespace : char list -> char list = function
		| [] -> []
		| ' ' :: tl | '\t' :: tl | '\n' :: tl  -> trim_leading_whitespace tl
		| hd :: tl -> hd :: tl
		in List.rev (trim_leading_whitespace
			(List.rev (trim_leading_whitespace input))) ;;

	(* I/P: chars, a character list
	 * O/P: a token and its remainder *)
	let rec until_whitespace (chars : char list) : char list * char list =
		match chars with
		| [] -> [], []
		| ' ' :: tl | '\t' :: tl | '\n' :: tl -> [], tl
		| ')' :: _ | '(' :: _ -> [], chars
		| hd :: tl -> let chars, rest = until_whitespace tl in hd :: chars, rest

	(* I/P: a list of characters, chars
	 * O/P: the expression_tree representation of chars,
	 * 			followed by the remaining characters *)
	let rec tree_of_expression (chars : char list)
	: expression_tree * char list =
		let trimmed_chars = trim_whitespace chars in
		match trimmed_chars with
		| [] -> Terminal [], []
		| '(' :: tl -> let tree_list, rest = make_tree_list tl in
			Expression tree_list, rest
		| hd :: tl -> let symbol, rest = until_whitespace trimmed_chars in
			Terminal symbol, rest
	and make_tree_list (chars : char list) : expression_tree list * char list =
		let trimmed_chars = trim_whitespace chars in
		match trimmed_chars with
		| [] -> failwith "wrong number of parentheses"
		| ')' :: tl -> [], tl
		| hd :: _ ->
			let tree, rest = tree_of_expression chars in
			let rest_list, rest_rest = make_tree_list rest in
				(tree :: rest_list), rest_rest

	(* I/P: expression, a list of characters
	 * O/P: the quoted_syntax representation of expression,
	 * 			followed by the remaining characters *)
	let read_helper (expression : char list) : concrete_program_piece * char list =
		let rec read_tree (tree : expression_tree) : concrete_program_piece =
			match tree with
			| Terminal chars -> read_atom chars None
			| Expression trees -> List (List.map read_tree trees) in
		let tree, rest = tree_of_expression expression in
			read_tree tree, rest

	(* I/P: expression, a string
	 * O/P: the quoted_syntax representation of expression *)
	let read (input : raw_program) : concrete_program_piece =
		let quoted, _ =
			read_helper (char_list_of_string input) in quoted

	(* I/P: program, a string
	 * O/P: a representation of program as a list of quoted_syntax,
	 * 			corresponding to multiple calls to (read) in Racket *)
	let read_all (input : raw_program) : concrete_program =
		let rec read_all_helper : char list -> concrete_program = function
			| [] -> []
			| _ :: _ as chars ->
				let quoted, rest = read_helper chars in
				quoted :: (read_all_helper rest) in
		read_all_helper (char_list_of_string input)
end ;;

open Reader ;;

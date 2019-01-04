# Rackette
Interpreter for a subset of Racket.

The inputted program that is to be interpreted is passed into the read procedure, which produces a concrete_program_piece list. The resulting output is passed through a series of parse procedures, which convert each element from a concrete_program_piece to a definition or expression. Additionally, parsing catches any compile-time errors. The resulting output from parsing is then processed by eval, which utilizes both the top-level environment as well as the local environment in order to produce an appropriate value or error. Finally, string_of_value converts the values produced by eval into strings, which are then outputted to the user.

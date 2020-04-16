
(* This file was auto-generated based on "flambda_parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 3 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 245 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 191 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 180 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 192 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 189 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 185 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 186 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 187 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 9 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 27 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 28 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 38 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 31 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 32 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 34 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 35 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 10 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 11 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 12 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 13 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 17 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 18 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 21 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 4 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 6 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 41 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 178 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 45 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 48 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 52 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 47 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 53 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 54 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 72 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 55 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 56 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 57 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 65 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 70 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 73 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 237 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 239 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 240 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 241 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 50 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 74 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 230 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 231 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 221 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 227 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 222 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 228 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 76 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 77 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 78 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 234 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 235 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 82 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 83 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 84 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 88 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 216 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 95 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 96 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 199 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 97 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 98 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 103 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 104 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 94 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 106 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 107 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 108 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 204 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 205 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 200 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 201 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 217 ->
        "Expected an expression.\n"
    | 209 ->
        "Expected an identifier.\nExamples of kinded variable/argument declarations:\n  arg1\n  (arg2 : val)\n"
    | 211 ->
        "Expected : and a kind.\nExamples of kinded variable/argument declarations:\n  arg1\n  (arg2 : val)\n"
    | 212 ->
        "Expected a kind.\nExamples of kinds:\n  val\n  imm\n"
    | 213 ->
        "Expected ).\n"
    | 109 ->
        "Expected rec or a code id.\nExamples of code definitions:\n  code f my_closure -> k : () = cont k ()\n  code rec f my_closure -> k : () = cont k ()\n"
    | 110 ->
        "Expected a code id.\n"
    | 111 ->
        "Expected a closure id declaration or a newer_version_of declaration or an\nargument list or an identifier for a closure argument.\nExample of a closure id declaration:\n  @ closure_id\nExample of a newer_version_of declaration:\n  newer_version_of code_id\nExamples of argument lists:\n  ()\n  (var1 (var2: imm))\n"
    | 116 ->
        "Expected a code id as an argument to newer_version_of.\nExample:\n  newer_version_of f0\n"
    | 118 ->
        "Expected an argument list in parentheses or an identifier for a closure argument.\nExamples of code definitions:\n  code f2 newer_version_of f1 (arg1 arg2) my_closure -> k = cont k arg1\n  code f2 newer_version_of f1 my_closure -> k : () = cont k ()\n"
    | 119 ->
        "Expected an identifier for a closure argument.\nExample of a code definition:\n  code f my_closure -> k : () = cont k ()\n"
    | 120 ->
        "Expected < followed by variables within a closure, or -> followed by a\ncontinuation.\nExamples of code definitions:\n  code f (arg) my_closure -> k : val = cont k (arg)\n  code f (arg1 (arg2 : imm)) my_closure -> k : val, imm = cont k (arg1 arg2)\n  code f my_closure <var1, (var2 : imm)> -> k : () = cont k ()\n"
    | 133 ->
        "Expected a name for a return continuation.\nExample of a code definition:\n  code f my_closure -> k : () = cont k ()\n"
    | 134 ->
        "Expected * followed by a name for an exception continuation, or : followed by a\nreturn arity, or = followed by an expression. \nExamples of code definitions:\n  code f my_closure -> k * e : () = cont k ()\n  code f my_closure -> k : val, val = cont k (23 42)\n  code f my_closure -> k = cont k (42)\n"
    | 135 ->
        "Expected : followed by a return arity, or = followed by an expression.\nExamples of code definitions:\n  code f my_closure -> k * e = cont k (42)\n  code f my_closure -> k * e : () = cont k ()\n  code f my_closure -> k * e : val, val = cont k (23 42)\n"
    | 145 ->
        "Expected an expression.\nExample of a code definition:\n  code f my_closure -> k = cont k (42)\n"
    | 144 ->
        "Expected = followed by an expression.\nExample of a code definition:\n  code f my_closure -> k : val = cont k (42)\n"
    | 121 ->
        "Expected a declaration of a variable within a closure, or >.\nExamples of lists of variables within a closure:\n  <>\n  <var1 (var2 : val)>\n"
    | 132 ->
        "Expected -> followed by a name for a continuation.\nExample of a code definition:\n  code f my_closure -> k * e = cont k arg1\n"
    | 122 ->
        "Expected an identifier for a variable within a closure.\nExamples of declarations of variables within closures:\n  x\n  (y : val)\n"
    | 123 ->
        "Expected : followed by a kind.\nExamples of declarations of variables within closures:\n  x\n  (y : val)\n"
    | 124 ->
        "Expected a kind.\nExamples of declarations of variables within closures:\n  x\n  (y : val)\n"
    | 125 ->
        "Expected ).\nExamples of declarations of variables within closures:\n  x\n  (y : val)\n"
    | 130 ->
        "Expected a declaration of a variable within a closure or > to end the list.\nExamples of lists of declarations of variables within a closure:\n  <>\n  <var1 (var2 : val)>\n"
    | 112 ->
        "Expected a closure id.\nExample of a code definition specifying a closure id:\n  code f @closure_id my_closure -> k * e = cont k arg1\n"
    | 115 ->
        "Expected a new_version_of declaration, an argument list, or a closure argument.\nExamples of code definitions:\n  code f2 @closure_id newer_version_of f1 arg1 arg2 my_closure <var1 var2>\n    -> k * e : , = cont k arg1 arg2\n  code f2 my_closure -> k * e = cont k arg1\n"
    | 147 ->
        "Expected a continuation to call.\nExample of a continuation call:\n  cont k\n"
    | 148 ->
        "Expected an argument list in parentheses, or end of file, or one of: with } in \nExamples of continuation calls:\n  cont k\n  cont k ()\n  cont k (arg1 arg2)\n"
    | 149 ->
        "Expected a simple value (a symbol, variable, or constant) as an argument to\na continuation.\nExamples of continuation calls:\n  cont k\n  cont k ()\n  cont k (arg1 arg2)\n"
    | 150 ->
        "Expected a simple value (a symbol, variable, or constant).\n"
    | 79 ->
        "Expected a code id.\nExample of a closure:\n  closure f\n"
    | 155 ->
        "Expected a C identifier in [].\nExample of a C call:\n  ccall [f] -> cont * exn_cont\n"
    | 156 ->
        "Expecting an identifier for a C function.\nExample of a C call:\n  ccall [f] -> cont * exn_cont\n"
    | 158 ->
        "Expecting ].\nExample of a C call:\n  ccall [f] -> cont * exn_cont\n"
    | 159 ->
        "Expecting an argument list, a return arity, or a continuation specification.\nExamples of C calls:\n  ccall [f] -> cont * exn_cont\n  ccall [f] (arg1 arg2) : , -> cont * exn_cont\n"
    | 162 ->
        "Expected a continuation.\nExample of a C call:\n  ccall [f] -> cont * exn_cont\n"
    | 163 ->
        "Expected * followed by an exception continuation.\nExample of a C call:\n  ccall [f] -> cont * exn_cont\n"
    | 160 ->
        "Expected : followed by an arity, or -> followed by a continuation.\nExamples of C calls:\n  ccall [f] -> cont * exn_cont\n  ccall [f] () -> cont * exn_cont\n  ccall [f] (arg1 arg2) : , -> cont * exn_cont\n"
    | 136 ->
        "Expected an arity.\nExamples of arities:\n  ()\n  val\n  int32, val, imm\n"
    | 141 ->
        "Expected one of the following: , -> =\nExamples of arities:\n  ()\n  val\n  int32, val, imm\n"
    | 142 ->
        "Expected a kind.\n"
    | 137 ->
        "Expected ).\nExamples of arities:\n    ()\n    val\n    int32, val, imm\n"
    | 161 ->
        "Expected -> followed by a continuation.\nExample of a C call:\n  ccall [f] : val -> cont * exn_cont\n"
    | 165 ->
        "Expected block tag.\nExample of a block:\n  Block 0 (field1 field2) \n"
    | 166 ->
        "Expected value(s) for block, in parentheses.\nExample of a block:\n  Block 0 (field1 field2) \n"
    | 167 ->
        "Expected value(s) for block.\nExample of a block:\n  Block 0 (field1 field2) \n"
    | 170 ->
        "Expected the name of a function to apply.\nExample of an application:\n  apply f (arg1 arg2) -> cont * exn_cont\n"
    | 173 ->
        "Expected an argument list in parentheses, or -> followed by a continuation.\nExamples of applications:\n  apply f -> cont * exn_cont\n  apply f () -> cont * exn_cont\n  apply f (arg1 arg2) -> cont * exn_cont\n"
    | 175 ->
        "Expected a continuation.\nExample of an application:\n  apply f (arg1 arg2) -> cont * exn_cont\n"
    | 176 ->
        "Expected * followed by an exception continuation.\nExample of an application: \n  apply f (arg1 arg2) -> cont * exn_cont\n"
    | 174 ->
        "Expected -> followed by a continuation.\nExample of an application:\n  apply f (arg1 arg2) -> cont * exn_cont\n"
    | _ ->
        raise Not_found

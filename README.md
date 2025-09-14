# Titania Programming Language

Based on the [Oberon-07](https://people.inf.ethz.ch/wirth/Oberon/Oberon07.Report.pdf) programming language designed by the late [Niklaus Wirth](https://en.wikipedia.org/wiki/Niklaus_Wirth).

This is designed to be a language to teach compiler development with.


## Grammar


```
module = "module" ident ";" [import_list] decl_sequence
         ["begin" stmt_sequence] "end" [";"].

import_list = "import" import_decl {"," import_decl} ";".
decl_sequence = ["const" {const_decl ";"}]
                ["type"  {type_decl  ";"}]
                ["var"   {var_decl   ";"}]
                [{proc_decl          ";"}].

const_decl = ident "=" const_expr.
type_decl = ident "="" struct_type.
var_decl = ident_list ":" type.

proc_decl = "proc" ident [formal_parameters] ";" proc_body.
proc_body = decl_sequence ["begin" stmt_sequence] ["return" expr] "end".


const_expr = expr.
expr = simple_expr {relation simple_expr}.

simple_expr = ["+" | "-"] unary_expr {add_operator unary_expr}.
unary_expr = ["+" | "-"] term.
term = factor {mul_operator factor}.

factor = integer | real | string | nil | true | false | set |
         "(" expr ")" | "not" expr | designator.

element = expr [".." expr].

ident_list = ident {"," ident}.
qual_ident = [ident "."] ident.

struct_type = array_type | record_type | pointer_type | proc_type.
array_type = "["" const_expr {"," const_expr} "]" type.
record_type = "record" ["(" qual_ident ")"] [field_list_sequence] "end".
pointer_type = "^" type.
proc_type = "proc" formal_parameters.
field_list = ident_list ":" type.
formal_parmeters = "(" [fp_section {";" fp_section}] [";"] ")".
formal_type = "[" "]" qual_ident.

stmt_sequence = stmt {";" stmt} [";"].
stmt = [assignment | proc_call | if_stmt | case_stmt | while_stmt | repeat_stmt | for_stmt ].

assignment = designator ":=" expr

if_stmt = "if" expr "then" stmt_sequence
          {"elseif" expr "then" stmt_sequence}
          ["else" stmt_sequence]
          "end".

case_stmt = "case" expr "of" case {"|" case} "end".
case = [case_label_list ":" stmt_sequence].
case_list = label_range {"," label_range}.
label_range = label [".." label].
label = integer | string | qual_ident.

while_stmt = "while" expr "do" stmt_sequence
             {"elseif" expr "then" stmt_sequence}
             "end".
repeat_stmt = "repeat" stmt_sequence "until" expr.
for_stmt = "for" ident ":=" expr "to" expr ["by" const_expr] "do" stmt_sequence "end".


designator = qual_ident {selector}.
selector = "." ident | "[" expr_list "]" | "^" | "(" qual_ident ")".
expr_list = expr {"," expr}.


add_operator = "+" | "-" | "xor" | "or".
mul_operator = "*" | "/" | "%"   | "and".
relation     = "=" | "<>" | "<" | "<=" | ">" | ">=" | "in" | "is".
```

### Keywords
```
and    else    import  of      then   while
begin  elseif  in      or      to     xor
by     end     is      proc    true
case   false   module  record  type
const  for     nil     repeat  until
do     if      not     return  var
```


### Operators

```
+    .   (   )   =  <>
-    ,   [   ]   <  <=
*    ;   {   }   >  >=
/    |   :=  :   ..
%    ^
```

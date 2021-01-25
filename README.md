# tree-unification

Perform unification (comparison) of two specified abstract syntax trees of expressions. Tree nodes contain double operations, variable names, or numeric values of type Double. In the general case, as a result of successful unification (comparison) of trees, the values of the variables included in them, as well as pairs of matched variables without values, are determined. 

In case of unsuccessful unification, an error message is displayed.

The Syntax Tree of expressions in their internal form is received at the input of the program, i.e. as non-empty binary trees, according to the type defined for them.

# Example of Syntax Tree:

Node (Var "F") (Node (Op Mul) (Leaf (Val 7)) (Leaf (Var "Y")))  (Leaf (Val 7))

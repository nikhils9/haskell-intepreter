# haskell-intepreter
A crude Haskell interpreter library based on video lectures from Crypto Mongolia 2020

This project demonstrates the convenience that implementing the evaluation of an expression (or processing of any data type) to a monad (monad "Eval" in the current context) helps us to make little to no change to the existing implementation while adding new ones.
Additionally you can make the evaluation of the expression more expressive by just adding more effects to the monad without touching the actual evaluation at all.
Almost every commit to this repository demonstrates the above facts by subsequent additions of new feature to the "Expr" data type which acts as the syntax.

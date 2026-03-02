if [%1]==[] goto nqueens
if [%1]==[hoftr] goto hoftr
if [%1]==[nqueens] goto nqueens
if [%1]==[expression] goto expression
:nqueens
ocamlc -o nqueens.exe util.ml nqueens.ml
if not [%1]==[] goto end
:expression
ocamlc -o expression.exe util.ml expression.ml
if not [%1]==[] goto end
:hoftr
ocamlc -o hoftr.exe util.ml hoftr.ml
if not [%1]==[] goto end
:end

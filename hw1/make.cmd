if [%1]==[] goto warmup
if [%1]==[warmup] goto warmup 
if [%1]==[hanoi] goto hanoi
:warmup
ocamlc -o warmup.exe util.ml warmup.ml
if not [%1]==[] goto end
:hanoi
ocamlc -o hanoi.exe util.ml hanoi.ml
:end
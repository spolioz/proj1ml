all:
	ocamlc -a vecteur.ml -o vecteur.cma
	ocamlc -a vecteur.cma boule2.ml -o boule2.cma
	ocamlc -a graphics.cma boule2.cma barre2.ml -o barre2.cma
	ocamlc -a barre2.cma boule2.cma niveau.ml -o niveau.cma
	ocamlc -a graphics.cma bouton.ml -o bouton.cma
	ocamlc -a bouton.cma niveau.cma graphique2.ml -o graphique2.cma
	ocamlc -a graphique2.cma lancement2.ml -o lancement2.cma
	ocamlc -a lancement2.cma interface2.ml -o interface2.cma
	ocamlc interface2.cma exemples.ml -o exemples.exe

clean:
	rm -rf *.cmi *.cmo *.cma *~


all:
	ocamlc -a vecteur.ml -o vecteur.cma
	ocamlc -a vecteur.cma boule.ml -o boule.cma
	ocamlc -a boule.cma trou.ml -o trou.cma
	ocamlc -a graphics.cma boule.cma barre.ml -o barre.cma
	ocamlc -a barre.cma trou.cma billard.ml -o billard.cma
	ocamlc -a graphics.cma bouton.ml -o bouton.cma
	ocamlc -a bouton.cma billard.cma graphique.ml -o graphique.cma
	ocamlc -a graphique.cma lancement.ml -o lancement.cma
	ocamlc -a interface.ml -o interface.cma
	ocamlc -a graphics.cma interface.cma quadtree.ml -o quadtree.cma
	ocamlc lancement.cma quadtree.cma exemples.ml -o exemples.exe

clean:
	rm -rf *.cmi *.cmo *.cma *~ 

all:
	ocamlc -a vecteur.ml -o vecteur.cma
	ocamlc -a vecteur.cma boule.ml -o boule.cma
	ocamlc -a boule.cma trou.ml -o trou.cma
	ocamlc -a graphics.cma trou.cma billard.ml -o billard.cma
	ocamlc -a billard.cma graphique.ml -o graphique.cma
	ocamlc -a graphique.cma lancement.ml -o lancement.cma
	ocamlc -a interface.ml -o interface.cma
	ocamlc -a graphics.cma interface.cma quadtree.ml -o quadtree.cma
	ocamlc graphics.cma vecteur.cma boule.cma trou.cma billard.cma graphique.cma lancement.cma interface.cma quadtree.cma exemples.ml -o exemples.exe

clean:
	rm -rf *.cmi *.cmo *.cma *~ 

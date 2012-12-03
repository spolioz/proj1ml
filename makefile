all:
	ocamlc -a vecteur.ml -o vecteur.cma
	ocamlc -a vecteur.cma boule.ml -o boule.cma
	ocamlc -a vecteur.cma boule.cma trou.ml -o trou.cma
	ocamlc -a graphics.cma vecteur.cma boule.cma trou.cma billard.ml -o billard.cma
	ocamlc -a graphics.cma vecteur.cma boule.cma trou.cma billard.cma graphique.ml -o graphique.cma
	ocamlc -a graphics.cma vecteur.cma boule.cma trou.cma billard.cma graphique.cma lancement.ml -o lancement.cma
	ocamlc -a graphics.cma vecteur.cma boule.cma billard.cma lancement.cma interface.ml -o interface.cma
	ocamlc graphics.cma vecteur.cma boule.cma billard.cma lancement.cma interface.cma exemples.ml -o exemples.exe

clean:
	rm -rf exemples.exe *.cmi *.cmo *~ 

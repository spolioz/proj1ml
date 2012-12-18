proj1ml
=======

1. Instructions de compilation
	Pour lancer la compilation du projet, il suffit de se placer dans le répertoire du projet via un terminal, et de lancer la commande make. Le makefile se charge alors de la compilation des bibliothèques et exécutables internes au projet.
Vous pouvez supprimer tous les fichiers bibliothèque créés grâce à la commande make clean.

2. Comment utiliser le billard
	Après la compilation, lancer le fichier exemples.exe par la commande ./exemples.exe. La fenere graphique s'ouvre alors, avec une configuration de billard américain. Pour jouer, sélectionner la boule blanche (c'est la seule dans laquelle vous avez le droit de tirer!), et lancez la en cliquant dans la direction dans laquelle vous voulez la lancer, aussi loin de la boule que vous voulez tirer fort. Si par malheur cette boule blanche tombe dans un trou, votre tour s'interrompt, et c'est au joueur adverse de replacer la boule où bon lui s"mble	 sur le billard, et de tirer. La partie s'arrête quand toutes les boules (sauf la blanche, bien sûr) sont tombées dans les trous.
Les points sont comptés de la manière suivante : à chaque tour, vous marquez 100 points pour la première boule rentrée dans un trou, 200 pour la suivante, puis 300, etc. Essayez de marquer plus de points que votre adversaire!
Vous pouvez quitter la partie à tout moment en cliquant sur le bouton de fermeture rouge en haut à droite de la fenêtre graphique.
Vous pouvez également réinitialiser la partie en cliquant sur le bouton reset bleu, à côté du bouton de fermeture.

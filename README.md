proj1ml
=======

1. Instructions de compilation
	Pour lancer la compilation du projet, il suffit de se placer dans le répertoire du projet via un terminal, et de lancer la commande make. Le makefile se charge alors de la compilation des bibliothèques et exécutables internes au projet.
Vous pouvez supprimer tous les fichiers bibliothèque créés grâce à la commande make clean.
Il y a deux makefiles, un dans le dossier billard, et un dans le dossier angry balls. Chacun compile les fichiers .ml présents dans le dossier.

2. Comment utiliser le billard
	Après la compilation, lancer le fichier exemples.exe par la commande ./exemples.exe. La fenere graphique s'ouvre alors, avec une configuration de billard américain. Pour jouer, sélectionner la boule blanche (c'est la seule dans laquelle vous avez le droit de tirer!), et lancez la en cliquant dans la direction dans laquelle vous voulez la lancer, aussi loin de la boule que vous voulez tirer fort. Si au cours du tour la boule blanche tombe dans un trou, alors elle réapparaîtra au tour suivant, et ce sera au joueur adverse de la placer à un endroit légal dans le billard, avant de la relancer. La partie s'arrête quand toutes les boules (sauf la blanche, bien sûr) sont tombées dans les trous.
	Les points sont comptés de la manière suivante : à chaque tour, vous marquez 100 points pour la première boule rentrée dans un trou, 200 pour la suivante, puis 300, etc. Essayez de marquer plus de points que votre adversaire!
	Vous pouvez quitter la partie à tout moment en cliquant sur le bouton de fermeture rouge en haut à droite de la fenêtre graphique.
	Vous pouvez également réinitialiser la partie en cliquant sur le bouton reset bleu, à côté du bouton de fermeture. Vous aurez le choix entre lancer le billard avec ou sans quadtree. Ce choix est conservé si vous terminez la partie et cliquer sur "OUI" pour rejouer. 

3. Comment jouer à Angry Balls
	Les commandes sont quasiment les mêmes que pour le billard, et l'interface fonctionne de la même manière. La seule différence réside dans le changement de tour : au lieu de se faire automatiquement, il est uniquement proposé lorsque les boules ont atteint une vitesse jugée suffisamment faible. Alors il vous suffira de cliquer pour passer au tour suivant, lorsque cela vous sera explicitement proposé par le texte s'affichant à l'écran.
	Vous avez le choix entre deux niveaux implémentés lorsque vous lancez le jeu. Pour chacun d'entre eux, vous avez le droit à 3 coups pour détruire ou faire sortir de l'écran toutes les boules ennemies (vertes). Attention, vous devrez éviter les murs indestructibles pour parvenir à votre but! 
	Les points sont comptés de la même manière : plus vous détruisez de boules consécutives en un tour, et plus vous en marquez! Amusez-vous bien! ;)

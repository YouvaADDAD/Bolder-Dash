Projet realise par:
- CAO Song Toan - 3702980
- Addad Youva - 3872388

1. Changer les types
- Definir un nouveau type "player" ayant 2 champs: pos:int*int & life:bool qui indique la position actuelle du joueur et sa vie. Si le joueur est mort, life = false
- Changer le type "game":
+ changer le type du champ "player" par type player
+ remplacer le champ "diamonds" par "diamonds_rest" qui indique le nombre de diamands restants sur la carte de jeu
+ ajouter le champ "score":int indiquant le score du joueur
+ ajouter le champ "aim":int indiquant le nombre de diamands a recuperer pour gagner le jeu
+ ajouter le champ "exit":int*int indiquant la position de la sortie. La sortie se situe au point de depart du joueur
=> La regle principale du jeu maintenant est de recuperer un nombre suffisant de diamands pour ouvrir la sortie et puis atteindre la sortie pour gagner le jeu
- Ajouter nouveux constructeur pour "cell"
+ Exit of bool: la sortie du jeu, ouverte si true, fermee sinon
+ Magic: le mur magique, il se transforme a l'objet qui se tombe sur lui (Walnut, Boulder, Diamond)
+ Steel: le mur en acier qui est indestructible
+ Butterfly of dir: un ennemi du joueur, il se deplace dans le sens clockwise suivant le long de la bordue des espaces vides, peut etre tue si l'on fait tomber un Boulder sur lui

2. Repandre l'effet de "chute"
Un objet X peut se rouler sur un objet Y si la condition C est satisfait. La chute de l'objet X peut causer un evenement E.
- X peut etre parmi {Boulder, Walnut, Diamond}
- Y peut etre parmi {Boulder, Walnut, Diamond, Stone, Steel, Magic}
- C est definie par
+ si le cas au-dessous de X est vide et le joueur n'y est pas, X se tombe dans ce cas
+ si le cas au-dessous de X est Y, X se rouler vers le droit (gauche) de Y si le cas droit (gauche) de Y est vide et le joueur n'est pas la
- E peut etre:
+ si un Boulder se tombe sur le joueur (un Butterfy), il tue le joueur (le Butterfly)
+ si un Boulder se tombe sur un Stone, le Stone et tous ses voisins s'explodent (sauf les murs en acier Steel et les Diamond)
+ si un Boulder se tombe sur un Walnut, le Walnut se transforme a un Diamond
+ le mur Magic se transforme a l'objet qui se tombe sur lui (Walnut, Boulder, Diamond)

3. Nouvelles characteristiques
- On considere chaque fois un button valide est tape une etape du jeu. Pour chaque etape, on calcule les nouvelles positions des Butterfly puis calcluer la nouvelle position du joueur et en fin fait "world_turn".
- Le nombre de diamands restants et le score sont affiche sur le graphique
- On ne peut pas pousser un Boulder vers North
- Walnut est presente dans la carte par un circle jaune
- Steel est presente dans la carte par un rectangle gris
- Magic est presente dans la carte par un rectangle vert fonce
- Butterfly est presente dans la carte par 2 triangles symmetriques en vert
- La sortie ouverte est presente par un rectangle magenta
- si le joueur est mort, le champ "life" de joueur est change a false

4. Changes des fonctions et nouvelles fonction
- Les fonctions player_turn, find_movable_boulder, position_after_fall, move_boulder_step, world_turn sont changees selon les regles ci-dessus
- voisins: renvoyer une liste contenant (i,j) et ses cas voisins
- explode : faire exploder (i,j) et tous ses cas voisins
- open_exit: ouvrir la sortie si le nombre de diamands collecte est suffisant
- butterfly_turn: calculer la nouvelle position d'un Butterfly 
- all_butterfly_turn: faire bouger tous les Butterfly
- make_game (fichier parse.ml): generer une instance de jeu (pas fini)

5. Utiliser l'instance de jeu "level3.lv" pour tester le fonctionnement total du jeu

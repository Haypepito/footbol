# FootBol

Bonjour à tous, bienvenue sur notre application de réservation de terrain de football.

Pour lancer notre application veuillez utiliser les 2 commandes suivantes dans votre terminal : 

    cobc -x --free app.cob
    ./app

Nous allons vous mettre à disposition un jeu de donnée avec 3 utilisateurs :

| Numéro      | Nom      | Prénom        | Login      | Rôle | Connecté | Mdp            |
|-------------|-------------|------------|------------|------|----------|----------------|
| 1   | Amateur1    | NomAmateur1| Amateur1   |   1  |    N     | 123 |
| 2   | Gerant1     | NomGerant1 | Gerant1    |   2  |    N     | 12345 |
| 3   | Admin1      | NomAdmin1  | Admin1     |   3  |    O     | 1234567 |

Dans notre application, le rôle 1 représente les utilisateurs 'amateurs', le rôle 2 représente les gérants et le rôle 3 représente les administrateurs.

__L'application est destinée à trois types d'utilisateurs :__

-    les administrateurs : ces derniers ont la possibilité de gérer tous les lieux, les terrains, les réservations ainsi que les utilisateurs, c’est-à-dire d’en ajouter, d’en modifier ou bien encore d’en supprimer. L’application leur permet également de manager les gérants. Aussi, c’est eux qui affectent un gérant à leur lieu de travail. 

-    les gérants : ces derniers peuvent gérer plusieurs terrains qui sont disponibles dans leur lieu d’affectation. Ils peuvent consulter, modifier et supprimer des réservations.

-    les clients : ces derniers correspondent aux joueurs amateurs souhaitant réserver un terrain et louer du matériel pour leur réservation.


*Par DAUZON Théo, MAYRAN Loïc, CHARPENTIER François et CARPOT Clément*




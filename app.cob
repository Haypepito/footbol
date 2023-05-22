       IDENTIFICATION DIVISION.
       PROGRAM-ID. footbol.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT futilisateur ASSIGN TO "utilisateurs.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS fu_numutilisateur
           ALTERNATE RECORD KEY IS fu_nom WITH DUPLICATES
           ALTERNATE RECORD KEY IS fu_login
           FILE STATUS IS cr_futilisateur.
       
           SELECT flieu ASSIGN TO "lieux.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS fl_numlieu
           ALTERNATE RECORD KEY IS fl_adresse
           ALTERNATE RECORD KEY IS fl_gerant
           FILE STATUS IS cr_flieu.
       
           SELECT freservation ASSIGN TO "reservations.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS fr_cleres
           ALTERNATE RECORD KEY IS fr_numutilisateur WITH DUPLICATES
           ALTERNATE RECORD KEY IS fr_numterrain WITH DUPLICATES
           FILE STATUS IS cr_freservation.
       
           SELECT fterrain ASSIGN TO "terrains.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS ft_numterrain
           ALTERNATE RECORD KEY IS ft_numlieuT WITH DUPLICATES
           FILE STATUS IS cr_fterrain.
       
           SELECT fstat ASSIGN TO "statistiques.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS fs_cle
           ALTERNATE RECORD KEY IS fs_lieu WITH DUPLICATES
           ALTERNATE RECORD KEY IS fs_mois WITH DUPLICATES
           FILE STATUS IS cr_fstat.
       
       DATA DIVISION.
       
       FILE SECTION.
       FD futilisateur.
          01 tamp_futilisateur.
             02 fu_numutilisateur PIC 9(9).
             02 fu_nom PIC A(30).
             02 fu_prenom PIC A(30).
             02 fu_login PIC A(50).
             02 fu_mdp PIC 9(20).
             02 fu_role PIC 9(1).
             02 fu_connecte PIC A.
       
       FD flieu.
          01 tamp_flieu.
             02 fl_numlieu PIC 9(9).
             02 fl_gerant PIC 9(9).
             02 fl_adresse PIC A(50).
             02 fl_terrain_existant PIC 9(2).
       
       FD freservation.
          01 tamp_freservation.
             02 fr_cleres.
                03 fr_numterrain PIC 9(9).
                03 fr_heure PIC 9(2).
                03 fr_date PIC 9(4).
             02 fr_numutilisateur PIC 9(10).
             02 fr_materiel PIC A(3).
       
       FD fterrain.
          01 tamp_fterrain.
             02 ft_numterrain PIC 9(9).
             02 ft_numlieuT PIC 9(9).
             02 ft_longueur PIC 9(4).
             02 ft_largeur PIC 9(4).
             02 ft_type PIC A(20).
             02 ft_prix PIC 9(5).
             02 ft_couvert PIC A.
       
       FD fstat.
          01 tamp_fstat.
             02 fs_cle.
                03 fs_lieu PIC A(50).
                03 fs_mois PIC 9(2).
             02 fs_nb_reservation PIC 9(9).
             02 fs_type_reservation_gazon PIC 9(9).
             02 fs_type_reservation_synthetique PIC 9(9).
             02 fs_type_reservation_falin PIC 9(9).
             02 fs_nb_reservation_materiel PIC 9(9).
       
       WORKING-STORAGE SECTION.

              77 cr_futilisateur PIC 9(2).
              77 cr_flieu PIC 9(2).
              77 cr_fterrain PIC 9(2).
              77 cr_fstat PIC 9(2).
              77 cr_freservation PIC 9(2).
              77 Wtrouve PIC 9(1).
              77 Wfin PIC 9(1).
              77 Wfinfin PIC 9(1).
              77 WfinfinS PIC 9(1).
              77 Wlogin_valide PIC 9(1).
              77 Wreponse PIC A(1).
              77 attempts PIC 9(1).
              01 W_freservation.
                02 W_fr_cle.
                  03 terrain_saisi PIC 9(9).
                  03 heure_saisie PIC 9(2).
                  03 date_saisie PIC 9(4).
                02 id_utilisateur PIC 9(10).  
                02 materiel PIC A(3).
               01 W_futilisateur.
                02 Wnumutilisateur PIC 9(9).
                02 Wnom PIC A(30).
                02 Wprenom PIC A(30).
                02 Wlogin PIC A(50).
                02 Wmdp PIC 9(20).
                02 Wrole PIC 9(1).
                02 Wconnecte PIC A.
               01 W_flieu.
                02 Wnumlieu PIC 9(9).
                02 Wgerant PIC 9(9).
                02 Wadresse PIC A(50).
                02 Wterrain_existant PIC 9(2).
               01 W_fstat.
                   02 Wcle.
                      03 WlieuS PIC A(50).
                      03 WmoisS PIC 9(2).
                   02 Wnb_reservationS PIC 9(9).
                   02 Wreservation_gazon PIC 9(9).
                   02 Wreservation_synthetique PIC 9(9).
                   02 Wreservation_falin PIC 9(9).
                   02 Wnb_reservation_materiel PIC 9(9).
               01 W_fterrain.
                02 Wnumterrain PIC 9(9).
                02 WnumlieuT PIC 9(9).
                02 Wlongueur PIC 9(4).
                02 Wlargeur PIC 9(4).
                02 Wtype PIC A(20).
                02 Wprix PIC 9(5).
                02 Wcouvert PIC A.
              01 choice PIC A.
              01 exitmenu PIC A.
              01 global_id_user PIC 9(10).
              01 global_role_user PIC 9(2).
              01 WS-CURRENT-DATE-DATA.
                05  WS-CURRENT-DATE.
                    10  WS-CURRENT-YEAR         PIC 9(2).
                    10  WS-CURRENT-MONTH        PIC 9(2).
                    10  WS-CURRENT-DAY          PIC 9(2).
                05  WS-CURRENT-TIME.
                    10  WS-CURRENT-HOURS        PIC 9(2).
                    10  WS-CURRENT-MINUTE       PIC 9(2).
                    10  WS-CURRENT-SECOND       PIC 9(2).
                    10  WS-CURRENT-MILLISECONDS PIC 9(2).
              01 WS-MONTH PIC 9(4).
              01 maxday PIC 9(2).
              01 reste PIC 9(2).
              01 jour PIC 9(2).

        PROCEDURE DIVISION.
                OPEN I-O futilisateur
                IF cr_futilisateur=35 THEN
                        OPEN OUTPUT futilisateur
                END-IF
                CLOSE futilisateur

                OPEN I-O flieu
                IF cr_flieu=35 THEN
                        OPEN OUTPUT flieu
                END-IF
                CLOSE flieu

                OPEN I-O freservation
                IF cr_freservation=35 THEN
                        OPEN OUTPUT freservation
                END-IF
                CLOSE freservation

                OPEN I-O fterrain
                IF cr_fterrain=35 THEN
                        OPEN OUTPUT fterrain
                END-IF
                CLOSE fterrain

                OPEN I-O fstat
                IF cr_fstat=35 THEN
                        OPEN OUTPUT fstat
                END-IF
                CLOSE fstat
                PERFORM AJOUT_UTILISATEUR
                PERFORM AFFICHAGE_UTILISATEUR
                ACCEPT WS-CURRENT-DATE-DATA FROM DATE             
                PERFORM CONNEXION_UTILISATEUR.
        STOP RUN.

       CONNEXION_UTILISATEUR.
           DISPLAY "________________________________________________________________"
            DISPLAY "________________________________________________________________"
            DISPLAY "                    Bienvenue dans FootBol "
            DISPLAY "________________________________________________________________"
            PERFORM UNTIL exitmenu = 'Y'
                DISPLAY "________________________________________________________________"
                DISPLAY "               Bienvenue dans le menu de FootBol "
                DISPLAY "________________________________________________________________"
                DISPLAY "1. Connexion "
                DISPLAY "0. Quitter"
                DISPLAY "Entrez votre choix (0-1):"
                ACCEPT choice
                EVALUATE choice
                    WHEN '1'
                       OPEN I-O futilisateur
                       DISPLAY "Entrez votre login :"
                       ACCEPT Wlogin
                       DISPLAY "Entrez votre mot de passe :"
                       ACCEPT Wmdp
                       SET Wtrouve TO 0
                       SET attempts TO 0
                       MOVE Wlogin TO fu_login
                       PERFORM UNTIL Wtrouve = 1 OR attempts = 3
                           READ futilisateur RECORD KEY IS fu_login
                           INVALID KEY DISPLAY "Trop de tentatives. Programme terminé."
                               CLOSE futilisateur
                               STOP RUN
                           NOT INVALID KEY
                               IF fu_mdp = Wmdp THEN
                                   MOVE fu_numutilisateur TO global_id_user
                                   MOVE fu_role TO global_role_user
                                   MOVE fu_role TO Wrole
                                   MOVE fu_numutilisateur to Wnumutilisateur
                                   MOVE 'O' TO Wconnecte
                                   MOVE Wconnecte TO fu_connecte
                                   REWRITE tamp_futilisateur FROM W_futilisateur
                                   MOVE 1 TO Wtrouve
                                   EVALUATE global_role_user
                                   WHEN 1
                                       DISPLAY "Bienvenue, vous êtes un inscrit simple"
                                       PERFORM MENU_AMATEUR
                                   WHEN 2
                                       DISPLAY "Bienvenue, vous êtes un gérant"
                                       PERFORM MENU_GERANT
                                   WHEN 3
                                       DISPLAY "Bienvenue, vous êtes un administrateur"
                                       PERFORM MENU_ADMIN
                                   END-EVALUATE
                               ELSE
                                   ADD 1 TO attempts
                                   IF attempts = 3 THEN
                                       DISPLAY "Trop de tentatives. Programme terminé."
                                       CLOSE futilisateur
                                       STOP RUN
                                   ELSE
                                       DISPLAY "Mot de passe ou login incorrect. Essayez encore."
                                       ACCEPT Wmdp
                                       READ futilisateur NEXT RECORD
                                   END-IF
                               END-IF
                           END-READ
                       END-PERFORM
                    WHEN '0'
                        MOVE 'Y' TO exitmenu
                        STOP RUN
                    WHEN OTHER
                        DISPLAY "Choix invalide. Veuillez réessayer."
                END-EVALUATE
            END-PERFORM
           
           CLOSE futilisateur.
       MENU_AMATEUR.
                PERFORM UNTIL exitmenu = 'A'
                    DISPLAY "________________________________________________________________"
                    DISPLAY "         Bienvenue dans le menu amateur de FootBol "
                    DISPLAY "________________________________________________________________"
                    DISPLAY "1. Modifier mon compte   "
                    DISPLAY "2. Ajout d'une réservation   "
                    DISPLAY "2. Ajout d'une réservation (Recherche par type de terrain)"
                    DISPLAY "4. Modifier une réservation   "
                    DISPLAY "5. Rerchercher mes réservations "
                    DISPLAY "6. Se déconnecter "
                    DISPLAY "0. Quitter"
                    DISPLAY "Entrez votre choix (0-5):"
                    ACCEPT choice

                    EVALUATE choice
                        WHEN '1'
                            PERFORM MODIF_UTILISATEUR
                        WHEN '2'
                            PERFORM AJOUT_RESERVATION
                        WHEN '3'
                            PERFORM AJOUT_RESERVATION_LIEU_TYPE
                        WHEN '4'
                            PERFORM MODIFIER_RESERVATION
                        WHEN '5'
                            PERFORM RECHERCHER_RESERVATION   
                        WHEN '6'
                            DISPLAY "Vous êtes bien déconnecté"
                            PERFORM DECONNECTER_UTILISATEUR
                            PERFORM CONNEXION_UTILISATEUR
                        WHEN '0'
                            MOVE 'A' TO exitmenu
                            STOP RUN
                        WHEN OTHER
                            DISPLAY "Choix invalide. Veuillez réessayer."
                    END-EVALUATE
                END-PERFORM.

        MENU_GERANT.
            PERFORM UNTIL exitmenu = 'G'
                DISPLAY "________________________________________________________________"
                DISPLAY "         Bienvenue dans le menu Gérant de FootBol "
                DISPLAY "________________________________________________________________"
                DISPLAY "1. Utilisateur"
                DISPLAY "2. Réservation"
                DISPLAY "3. Terrain"
                DISPLAY "4. Se déconnecter "
                DISPLAY "0. Quitter"
                DISPLAY "Entrez votre choix (0-5):"
                ACCEPT choice

                EVALUATE choice
                    WHEN '1'
                        PERFORM MENU_UTILISATEUR
                    WHEN '2'
                        PERFORM MENU_RESERVATION
                    WHEN '3'
                        PERFORM MENU_TERRAIN
                    WHEN '4'
                        DISPLAY "Vous êtes bien déconnecté"
                        PERFORM DECONNECTER_UTILISATEUR
                        PERFORM CONNEXION_UTILISATEUR
                    WHEN '0'
                        MOVE 'G' TO exitmenu
                        STOP RUN
                    WHEN OTHER
                        DISPLAY "Choix invalide. Veuillez réessayer."
                END-EVALUATE
            END-PERFORM. 

        MENU_ADMIN.
            PERFORM UNTIL exitmenu = 'A'
                DISPLAY "________________________________________________________________"
                DISPLAY "       Bienvenue dans le menu Administrateur de FootBol "
                DISPLAY "________________________________________________________________"
                DISPLAY "1. Utilisateur"
                DISPLAY "2. Réservation"
                DISPLAY "3. Terrain"
                DISPLAY "4. Lieu"
                DISPLAY "5. Statistiques"
                DISPLAY "6. Se déconnecter"
                DISPLAY "0. Quitter"
                DISPLAY "Entrez votre choix (0-6):"
                ACCEPT choice

                EVALUATE choice
                    WHEN '1'
                        PERFORM MENU_UTILISATEUR
                    WHEN '2'
                        PERFORM MENU_RESERVATION
                    WHEN '3'
                        PERFORM MENU_TERRAIN
                    WHEN '4'
                        PERFORM MENU_LIEU
                    WHEN '5'
                        PERFORM COLLECTER_STATISTIQUES
                    WHEN '6'
                        DISPLAY "Vous êtes bien déconnecté"
                        PERFORM DECONNECTER_UTILISATEUR
                        PERFORM CONNEXION_UTILISATEUR
                    WHEN '0'
                        MOVE 'A' TO exitmenu
                        STOP RUN
                    WHEN OTHER
                        DISPLAY "Choix invalide. Veuillez réessayer."
                END-EVALUATE
            END-PERFORM. 

       MENU_UTILISATEUR.
                PERFORM UNTIL exitmenu = 'U'
                    DISPLAY "________________________________________________________________"
                    DISPLAY "         Bienvenue dans le menu utilisateur de FootBol "
                    DISPLAY "________________________________________________________________"
                    DISPLAY "1. Affichage des utilisateurs"
                    DISPLAY "2. Ajout des utilisateurs"
                    DISPLAY "3. Modifier un utilisateur   "
                    DISPLAY "4. Modifier un role utilisateur   "
                    DISPLAY "5. Supprimer un utilisateur   "
                    DISPLAY "0. Retour"
                    DISPLAY "Entrez votre choix (0-5):"
                    ACCEPT choice

                    EVALUATE choice
                        WHEN '1'
                            PERFORM AFFICHAGE_UTILISATEUR
                        WHEN '2'
                            PERFORM AJOUT_UTILISATEUR
                        WHEN '3'
                            PERFORM AFFICHAGE_UTILISATEUR
                            PERFORM MODIF_UTILISATEUR
                        WHEN '4'
                            PERFORM MODIF_DROIT
                        WHEN '5'
                            PERFORM SUPPRIMER_UTILISATEUR
                        WHEN '0'
                            IF global_role_user = 2
                            PERFORM MENU_GERANT
                            ELSE IF global_role_user = 3
                            PERFORM MENU_ADMIN
                            END-IF
                        WHEN OTHER
                            DISPLAY "Choix invalide. Veuillez réessayer."
                    END-EVALUATE
                END-PERFORM.

           MENU_RESERVATION.
                PERFORM UNTIL exitmenu = 'R'
                    DISPLAY "________________________________________________________________"
                    DISPLAY "       Bienvenue dans le menu reservation de FootBol "
                    DISPLAY "________________________________________________________________"
                    DISPLAY "1. Affichage des réservation"
                    DISPLAY "2. Ajout d'une réservation"
                    DISPLAY "3. Modifier une réservation   "
                    DISPLAY "4. Rerchercher une réservation"
                    DISPLAY "5. Supprimer une réservation   "
                    DISPLAY "0. Retour"
                    DISPLAY "Entrez votre choix (0-5):"
                    ACCEPT choice

                    EVALUATE choice
                        WHEN '1'
                            IF global_role_user = 2
                            PERFORM AFFICHAGE_RESERVATION_GERANT
                            ELSE
                            PERFORM AFFICHAGE_RESERVATION
                        WHEN '2'
                            PERFORM AJOUT_RESERVATION
                        WHEN '3'
                            PERFORM AFFICHAGE_RESERVATION
                            PERFORM MODIFIER_RESERVATION
                        WHEN '4'
                            PERFORM RECHERCHER_RESERVATION
                        WHEN '5'
                            PERFORM SUPPRIMER_RESERVATION
                        WHEN '0'
                            IF global_role_user = 2
                            PERFORM MENU_GERANT
                            ELSE IF global_role_user = 3
                            PERFORM MENU_ADMIN
                            END-IF
                        WHEN OTHER
                            DISPLAY "Choix invalide. Veuillez réessayer."
                    END-EVALUATE
                END-PERFORM.

           MENU_TERRAIN.
                PERFORM UNTIL exitmenu = 'T'
                    DISPLAY "________________________________________________________________"
                    DISPLAY "         Bienvenue dans le menu terrain de FootBol "
                    DISPLAY "________________________________________________________________"
                    DISPLAY "1. Affichage des terrains"
                    DISPLAY "2. Ajout d'un terrain"
                    DISPLAY "3. Modification d'un terrain   "
                    DISPLAY "4. Supprimer un terrain   "
                    DISPLAY "0. Retour"
                    DISPLAY "Entrez votre choix (0-5):"
                    ACCEPT choice

                    EVALUATE choice
                        WHEN '1'
                            IF global_role_user = 2
                            PERFORM AFFICHAGE_TERRAIN_GERANT
                            ELSE
                            PERFORM AFFICHAGE_TERRAIN
                        WHEN '2'
                            PERFORM AJOUT_TERRAIN
                        WHEN '3'
                            PERFORM MODIFIER_TERRAIN
                        WHEN '4'
                            PERFORM SUPPRIMER_TERRAIN
                        WHEN '0'
                            IF global_role_user = 2
                            PERFORM MENU_GERANT
                            ELSE IF global_role_user = 3
                            PERFORM MENU_ADMIN
                            END-IF
                        WHEN OTHER
                            DISPLAY "Choix invalide. Veuillez réessayer."
                    END-EVALUATE
                END-PERFORM.

           MENU_LIEU.
                PERFORM UNTIL exitmenu = 'L'
                    DISPLAY "________________________________________________________________"
                    DISPLAY "         Bienvenue dans le menu lieu de FootBol "
                    DISPLAY "________________________________________________________________"
                    DISPLAY "1. Affichage des lieux"
                    DISPLAY "2. Ajout d'un lieu"
                    DISPLAY "3. Modifier un lieu   "
                    DISPLAY "4. Supprimer un lieu   "
                    DISPLAY "0. Retour"
                    DISPLAY "Entrez votre choix (0-5):"
                    ACCEPT choice

                    EVALUATE choice
                        WHEN '1'
                            PERFORM AFFICHAGE_LIEU
                        WHEN '2'
                            PERFORM AJOUT_LIEU
                        WHEN '3'
                            PERFORM MODIF-LIEU
                        WHEN '4'
                            PERFORM SUPPRIMER_LIEU
                        WHEN '0'
                            PERFORM MENU_ADMIN
                        WHEN OTHER
                            DISPLAY "Choix invalide. Veuillez réessayer."
                    END-EVALUATE
                END-PERFORM.
       AJOUT_UTILISATEUR.
           OPEN I-O futilisateur
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 0
                   DISPLAY "Numero : "
                   ACCEPT Wnumutilisateur
                   MOVE Wnumutilisateur TO fu_numutilisateur
                   READ futilisateur
                   INVALID KEY  DISPLAY " "
                                MOVE 0 TO Wtrouve
                   NOT INVALID KEY DISPLAY "Numéro déjà utilisé"
                                   MOVE 1 TO Wtrouve
                   END-READ
           END-PERFORM
     
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 0
               DISPLAY "Prenom : "
               ACCEPT Wprenom
               DISPLAY "Nom : "
               ACCEPT Wnom
               READ futilisateur NEXT
               AT END DISPLAY " "
               NOT AT END IF fu_prenom = Wprenom
                             and fu_nom = Wnom
               THEN
                       MOVE 1 TO Wtrouve
               ELSE
                       MOVE 0 TO Wtrouve
               END-IF
              END-READ
           END-PERFORM

           PERFORM WITH TEST AFTER UNTIL Wtrouve = 0
                   DISPLAY "Entrez votre login :"
                   ACCEPT Wlogin
                   MOVE Wlogin TO fu_login
                   READ futilisateur RECORD KEY IS fu_login
                   INVALID KEY DISPLAY " "
                                MOVE 0 TO Wtrouve
                   NOT INVALID KEY DISPLAY "Le login est déjà utilisé."
                                   MOVE 1 TO Wtrouve
                   END-READ
           END-PERFORM

           DISPLAY "Rôle : "
           ACCEPT Wrole
           DISPLAY "Mdp : "
           ACCEPT Wmdp
           MOVE 'N' TO Wconnecte 
           MOVE Wprenom to fu_prenom
           MOVE Wnom to fu_nom
           MOVE Wlogin TO fu_login
           MOVE Wrole to fu_role
           MOVE Wmdp TO fu_mdp

           MOVE W_futilisateur to tamp_futilisateur
           WRITE tamp_futilisateur
           IF cr_futilisateur = "00" THEN
           DISPLAY "Utilisateur ajouté avec succès."
           ELSE
           DISPLAY "Erreur lors de l'écriture de l'utilisateur."
           END-IF
           CLOSE futilisateur.

       MODIF_UTILISATEUR.
           OPEN I-O futilisateur
            IF global_role_user = 1 THEN
               MOVE global_id_user TO fr_numutilisateur
           ELSE
               PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
                   DISPLAY "Numéro de l'utilisateur à modifier : "
                   ACCEPT Wnumutilisateur
                   MOVE Wnumutilisateur TO fu_numutilisateur
                   READ futilisateur
                   INVALID KEY  DISPLAY "Utilisateur introuvable"
                                MOVE 0 TO Wtrouve
                   NOT INVALID KEY MOVE 1 TO Wtrouve
                   END-READ
               END-PERFORM
           END-IF
           IF Wtrouve = 1
               MOVE fu_role TO Wrole
               
               DISPLAY "Nouveau nom : ( actuel " fu_nom" )"
               ACCEPT Wnom
               DISPLAY "Nouveau prénom : ( actuel " fu_prenom" )"
               ACCEPT Wprenom
               DISPLAY "Nouveau login : ( actuel " fu_login" )"
               ACCEPT Wlogin
               DISPLAY "Nouveau mdp : ( actuel " fu_mdp" )"
               ACCEPT Wmdp

               MOVE Wnom TO fu_nom
               Move Wprenom TO fu_prenom
               MOVE Wlogin TO fu_login
               MOVE Wmdp TO fu_mdp
               MOVE Wrole TO fu_role
    
               REWRITE tamp_futilisateur FROM W_futilisateur
               IF cr_futilisateur = "00" THEN
               DISPLAY "Utilisateur modifié avec succès."
               ELSE
               DISPLAY "Erreur lors de l'écriture de l'utilisateur."
               END-IF
           END-IF
           CLOSE futilisateur.

        DECONNECTER_UTILISATEUR.
           OPEN I-O futilisateur
           MOVE 0 TO Wtrouve
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
               MOVE global_id_user TO fu_numutilisateur
               DISPLAY global_id_user
               DISPLAY fu_numutilisateur
               READ futilisateur
               INVALID KEY  DISPLAY "Utilisateur introuvable"
                            MOVE 0 TO Wtrouve
               NOT INVALID KEY MOVE 1 TO Wtrouve
                   MOVE 0 TO global_id_user
                   MOVE 0 TO global_role_user
                   MOVE fu_role TO Wrole
                   MOVE fu_numutilisateur to Wnumutilisateur
                   MOVE 'N' TO Wconnecte
                   REWRITE tamp_futilisateur FROM W_futilisateur
                   IF cr_futilisateur = "00" THEN
                   DISPLAY "Utilisateur déconnecté"
                   ELSE
                   DISPLAY "Déconnection impossible"
                   END-IF
               END-READ
           END-PERFORM
           CLOSE futilisateur.

       MODIF_DROIT.
           OPEN I-O futilisateur
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
               DISPLAY "Numéro de l'utilisateur à modifier : "
               ACCEPT Wnumutilisateur
               MOVE Wnumutilisateur TO fu_numutilisateur
               READ futilisateur
               INVALID KEY  DISPLAY "Utilisateur introuvable"
                            MOVE 0 TO Wtrouve
               NOT INVALID KEY MOVE 1 TO Wtrouve
               END-READ
           END-PERFORM
    
           IF Wtrouve = 1
               MOVE fu_mdp TO Wmdp
               MOVE fu_nom TO Wnom
               MOVE fu_prenom TO Wprenom
               MOVE fu_login TO Wlogin
               
               DISPLAY "Nouveau rôle : ( actuel " fu_role" )"
               ACCEPT Wrole

               MOVE Wnom TO fu_nom
               Move Wprenom TO fu_prenom
               MOVE Wlogin TO fu_login
               MOVE Wmdp TO fu_mdp
               MOVE Wrole TO fu_role
    
               REWRITE tamp_futilisateur FROM W_futilisateur
               IF cr_futilisateur = "00" THEN
               DISPLAY "Utilisateur modifié avec succès."
               ELSE
               DISPLAY "Erreur lors de l'écriture de l'utilisateur."
               END-IF
           END-IF
           CLOSE futilisateur.
       
       SUPPRIMER_UTILISATEUR.
           open I-O futilisateur
       display "Suppression d'un utilisateur"
       display "Nom de l'utilisateur :"
       accept Wnom
       display "Prénom de l'utilisateur :"
       accept Wprenom
       perform with test after until Wtrouve = 1
           read futilisateur NEXT
               at end move 1 to Wtrouve
               not at end
                   if fu_nom = Wnom and fu_prenom = Wprenom
                       display "Utilisateur trouvé"
           display fu_nom " " fu_prenom " " fu_login" "fu_role" "fu_mdp
           display "Confirmer suppression utilisateur ? (O/N)"
                       accept Wreponse
                       if Wreponse = "O" or Wreponse = "o"
                           open I-O freservation
                           MOVE 0 TO Wfin
                           MOVE fu_numutilisateur TO fr_numutilisateur
                           START freservation,KEY IS = fr_numutilisateur
                           INVALID KEY DISPLAY "Pas de réservation"
                           NOT INVALID KEY
                               perform with test after until Wfin = 1
                                   read freservation NEXT
                                       at end move 1 to Wfin
                                       not at end
                                           IF fr_numutilisateur = fu_numutilisateur
                                               delete freservation
                                               display "Réservation supprimée"
                                           end-if
                                   end-read
                               end-perform
                            END-START
                           close freservation
                           delete futilisateur
                           display "Utilisateur supprimé"
                       else
                           display "Suppression annulée"
                       end-if
                       move 1 to Wtrouve
                   end-if
           end-read
       end-perform
       close futilisateur.
      
        AFFICHAGE_UTILISATEUR.
            OPEN INPUT futilisateur
            MOVE 1 TO Wfin
            PERFORM WITH TEST AFTER UNTIL Wfin=0
                    READ futilisateur NEXT
                    AT END MOVE 0 TO Wfin
                    NOT AT END
                       DISPLAY "Numéro : "fu_numutilisateur
                       DISPLAY "Prénom : " fu_prenom
                       DISPLAY "Nom : "fu_nom
                       DISPLAY "Login : "fu_login
                       DISPLAY "Role : "fu_role
                       DISPLAY "Connecté : "fu_connecte
                       DISPLAY "Mdp : "fu_mdp
                       DISPLAY "________________________________"
                    END-READ
            END-PERFORM
            CLOSE futilisateur.

       AJOUT_RESERVATION.
           DISPLAY "Ajouter une réservation"
           DISPLAY "________________________________"
           MOVE 0 TO Wtrouve
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
               DISPLAY "Veuillez saisir le lieu de votre réservation :"
               OPEN INPUT flieu
               ACCEPT Wadresse
               MOVE Wadresse TO fl_adresse
               READ flieu RECORD KEY IS fl_adresse
                   INVALID KEY DISPLAY "Aucunes correspondances avec nos centres."
                                fl_adresse
                   NOT INVALID KEY
                        DISPLAY "Lieu existant"
                        OPEN INPUT fterrain
                        MOVE fl_numlieu TO ft_numlieuT
                        MOVE 1 TO Wfin
                        START fterrain, KEY IS = ft_numlieuT
                        INVALID KEY DISPLAY "Ce lieu ne possède plus de terrain"
                        NOT INVALID KEY
                            DISPLAY " "
                            DISPLAY "________________________________"
                            DISPLAY "Adresse du lieu : "fl_adresse
                            DISPLAY "Identifiant du lieu : " ft_numlieuT
                            DISPLAY "________________________________"
                            DISPLAY " "
                            PERFORM WITH TEST AFTER UNTIL Wfin = 0
                                READ fterrain NEXT 
                                AT END MOVE 0 TO Wfin
                                NOT AT END
                                    IF ft_numlieuT = fl_numlieu
                                       DISPLAY "Numéro de terrain : "ft_numterrain
                                       DISPLAY "Lieu : "ft_numlieuT
                                       DISPLAY "Longueur : "ft_longueur
                                       DISPLAY "Largeur : "ft_largeur
                                       DISPLAY "Type : "ft_type
                                       DISPLAY "Prix : "ft_prix
                                       DISPLAY "Couvert : "ft_couvert
                                       DISPLAY "________________________________"   
                                    END-IF
                                END-READ
                            END-PERFORM
                            MOVE 1 TO Wtrouve  
                        END-START
                END-READ
            END-PERFORM
           OPEN I-O freservation
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 0 
                OPEN I-O fterrain
               PERFORM WITH TEST AFTER UNTIL Wtrouve = 0
                       DISPLAY "Entrez le numéro de terrain :"
                       ACCEPT terrain_saisi
                       MOVE terrain_saisi TO ft_numterrain
                       READ fterrain
                       INVALID KEY  DISPLAY " "
                                    MOVE 1 TO Wtrouve
                                    DISPLAY "Terrain inexistant"
                       NOT INVALID KEY DISPLAY "Terrain existant"
                                       MOVE 0 TO Wtrouve
                       END-READ
               END-PERFORM

               MOVE 0 TO Wtrouve
               PERFORM WITH TEST AFTER UNTIL Wtrouve = 1        
                    DISPLAY "Votre lieu vous propose 8 créneaux : "
                    DISPLAY " -> Créneau 1 : 8h00 à 9h00"
                    DISPLAY " -> Créneau 2 : 9h00 à 10h00"
                    DISPLAY " -> Créneau 3 : 10h00 à 11h00"
                    DISPLAY " -> Créneau 4 : 11h00 à 12h00"
                    DISPLAY " -> Créneau 5 : 14h00 à 15h00"
                    DISPLAY " -> Créneau 6 : 15h00 à 16h00"
                    DISPLAY " -> Créneau 7 : 16h00 à 17h00"
                    DISPLAY " -> Créneau 8 : 17h00 à 18h00"
                    DISPLAY "Entrez le crénaux de la réservation (1-8) :"
                    ACCEPT heure_saisie
                    IF heure_saisie <= 8 and heure_saisie >=1
                    MOVE 1 TO Wtrouve
                    ELSE
                    DISPLAY "Crénaux invalide"
                    END-IF
               END-PERFORM

                MOVE 0 TO Wtrouve
               PERFORM WITH TEST AFTER UNTIL Wtrouve = 1 
                   DISPLAY "Entrez la date de la réservation (JJMM) :"
                   ACCEPT date_saisie
                   DISPLAY date_saisie(1:2)
                   DISPLAY date_saisie(3:2)
                   DIVIDE date_saisie(3:2) BY 2 GIVING jour REMAINDER reste
                   IF jour = 0
                        MOVE 30 TO maxday
                   ELSE 
                        MOVE 31 TO maxday
                   END-IF     
                   IF date_saisie(1:2) IS NUMERIC AND
                      date_saisie(1:2) > 0 AND
                      date_saisie(1:2) <= maxday AND
                      date_saisie(3:2) IS NUMERIC AND
                      date_saisie(3:2) > 0 AND
                      date_saisie(3:2) <= 12 AND 
                      (date_saisie(3:2) > WS-CURRENT-MONTH OR 
                        (date_saisie(3:2) = WS-CURRENT-MONTH 
                            AND date_saisie(1:2) > WS-CURRENT-DAY))             
                      MOVE 1 TO Wtrouve
                   ELSE
                      DISPLAY "Date invalide. Veuillez réessayer."
                   END-IF
               END-PERFORM

               MOVE terrain_saisi TO fr_numterrain
               MOVE heure_saisie TO fr_heure
               MOVE date_saisie TO fr_date 
               READ freservation
                   INVALID KEY  DISPLAY " "
                                    MOVE 0 TO Wtrouve
                   NOT INVALID KEY DISPLAY "La réservation existe déjà, Essayez un autre Crénaux"
                                    MOVE 1 TO Wtrouve
               END-READ 
           END-PERFORM

           IF global_role_user = 1 THEN
               MOVE global_id_user TO fr_numutilisateur
           ELSE
               DISPLAY "Entrez l'id de l'utilisateur de la réservation :"
               ACCEPT id_utilisateur
               MOVE id_utilisateur TO fr_numutilisateur
           END-IF
           DISPLAY "Location de matériel (Oui/Non) :"
           ACCEPT materiel
           MOVE materiel TO fr_materiel
           WRITE tamp_freservation
           IF cr_freservation = "00"
               DISPLAY "Réservation ajoutée avec succès."
           ELSE
               DISPLAY "Erreur lors de l'ajout de la réservation."   
           CLOSE fterrain
           CLOSE flieu 
           CLOSE freservation.

        AJOUT_RESERVATION_LIEU_TYPE.
           DISPLAY "Ajouter une réservation"
           DISPLAY "________________________________"
           MOVE 0 TO Wtrouve
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
               DISPLAY "Veuillez saisir le lieu de votre réservation :"
               OPEN INPUT flieu
               ACCEPT Wadresse
               MOVE Wadresse TO fl_adresse
               READ flieu RECORD KEY IS fl_adresse
                   INVALID KEY DISPLAY "Aucunes correspondances avec nos centres."
                                fl_adresse
                   NOT INVALID KEY
                        DISPLAY "Lieu existant"
                        OPEN INPUT fterrain
                        MOVE fl_numlieu TO ft_numlieuT
                        MOVE 1 TO Wfin
                        START fterrain, KEY IS = ft_numlieuT
                        INVALID KEY DISPLAY "Ce lieu ne possède plus de terrain"
                        NOT INVALID KEY
                            DISPLAY "Veuillez saisir le type de terrain (Falin/Gazon/Synthétique) : "
                            ACCEPT Wtype
                            MOVE Wtype TO ft_type
                            DISPLAY " "
                            DISPLAY "________________________________"
                            DISPLAY "Adresse du lieu : "fl_adresse
                            DISPLAY "Identifiant du lieu : " ft_numlieuT
                            DISPLAY "________________________________"
                            DISPLAY " "
                            PERFORM WITH TEST AFTER UNTIL Wfin = 0
                                READ fterrain NEXT 
                                AT END MOVE 0 TO Wfin
                                NOT AT END
                                    IF ft_numlieuT = fl_numlieu and ft_type = Wtype
                                       DISPLAY "Numéro de terrain : "ft_numterrain
                                       DISPLAY "Lieu : "ft_numlieuT
                                       DISPLAY "Longueur : "ft_longueur
                                       DISPLAY "Largeur : "ft_largeur
                                       DISPLAY "Type : "ft_type
                                       DISPLAY "Prix : "ft_prix
                                       DISPLAY "Couvert : "ft_couvert
                                       DISPLAY "________________________________"   
                                       MOVE 1 TO Wtrouve 
                                    END-IF
                                END-READ
                            END-PERFORM 
                        END-START
                END-READ
            END-PERFORM
           OPEN I-O freservation
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 0 
                OPEN I-O fterrain
               PERFORM WITH TEST AFTER UNTIL Wtrouve = 0
                       DISPLAY "Entrez le numéro de terrain :"
                       ACCEPT terrain_saisi
                       MOVE terrain_saisi TO ft_numterrain
                       READ fterrain
                       INVALID KEY  DISPLAY " "
                                    MOVE 1 TO Wtrouve
                                    DISPLAY "Terrain inexistant"
                       NOT INVALID KEY DISPLAY "Terrain existant"
                                       MOVE 0 TO Wtrouve
                       END-READ
               END-PERFORM

               MOVE 0 TO Wtrouve
               PERFORM WITH TEST AFTER UNTIL Wtrouve = 1        
                    DISPLAY "Entrez le crénaux de la réservation (1-8) :"
                    ACCEPT heure_saisie
                    IF heure_saisie <= 8 and heure_saisie >=1
                    MOVE 1 TO Wtrouve
                    ELSE
                    DISPLAY "Crénaux invalide"
                    END-IF
               END-PERFORM

               MOVE 0 TO Wtrouve
               PERFORM WITH TEST AFTER UNTIL Wtrouve = 1 
                   DISPLAY "Entrez la date de la réservation (JJMM) :"
                   ACCEPT date_saisie
                   DISPLAY date_saisie(1:2)
                   DISPLAY date_saisie(3:2)
                   DIVIDE date_saisie(3:2) BY 2 GIVING jour REMAINDER reste
                   IF jour = 0
                        MOVE 30 TO maxday
                   ELSE 
                        MOVE 31 TO maxday
                   END-IF     
                   IF date_saisie(1:2) IS NUMERIC AND
                      date_saisie(1:2) > 0 AND
                      date_saisie(1:2) <= maxday AND
                      date_saisie(3:2) IS NUMERIC AND
                      date_saisie(3:2) > 0 AND
                      date_saisie(3:2) <= 12 AND 
                      (date_saisie(3:2) > WS-CURRENT-MONTH OR 
                        (date_saisie(3:2) = WS-CURRENT-MONTH 
                            AND date_saisie(1:2) > WS-CURRENT-DAY))             
                      MOVE 1 TO Wtrouve
                   ELSE
                      DISPLAY "Date invalide. Veuillez réessayer."
                   END-IF
               END-PERFORM

               MOVE terrain_saisi TO fr_numterrain
               MOVE heure_saisie TO fr_heure
               MOVE date_saisie TO fr_date 
               READ freservation
                   INVALID KEY  DISPLAY " "
                                    MOVE 0 TO Wtrouve
                   NOT INVALID KEY DISPLAY "La réservation existe déjà, Essayez un autre Crénaux"
                                    MOVE 1 TO Wtrouve
               END-READ 
           END-PERFORM

           IF global_role_user = 1 THEN
               MOVE global_id_user TO fr_numutilisateur
           ELSE
               DISPLAY "Entrez l'id de l'utilisateur de la réservation :"
               ACCEPT id_utilisateur
               MOVE id_utilisateur TO fr_numutilisateur
           END-IF
           DISPLAY "Location de matériel (Oui/Non) :"
           ACCEPT materiel
           MOVE materiel TO fr_materiel
           WRITE tamp_freservation
           IF cr_freservation = "00"
               DISPLAY "Réservation ajoutée avec succès."
           ELSE
               DISPLAY "Erreur lors de l'ajout de la réservation."   
           CLOSE fterrain
           CLOSE flieu 
           CLOSE freservation.

       AFFICHAGE_RESERVATION.
           DISPLAY "Toutes les réservations"
           DISPLAY "________________________________" 
           OPEN INPUT freservation
           MOVE 1 TO Wfin
           PERFORM WITH TEST AFTER UNTIL Wfin=0
                    READ freservation NEXT
                    AT END MOVE 0 TO Wfin
                    NOT AT END
                       DISPLAY "Numéro : "fr_numterrain
                       DISPLAY "Num Utilisateur : "fr_numutilisateur
                       DISPLAY "Date : "fr_date
                       DISPLAY "Crénaux : "fr_heure
                       DISPLAY "Matériel : "fr_materiel
                       DISPLAY "________________________________"
                    END-READ
           END-PERFORM
           CLOSE freservation.

           AFFICHAGE_RESERVATION_GERANT.
           OPEN INPUT flieu
           MOVE global_id_user TO fl_gerant
           READ flieu NEXT
           AT END DISPLAY "Gérant au chomage"
           NOT AT END 
                MOVE fl_numlieu TO Wnumlieu
                OPEN INPUT fterrain
                MOVE Wnumlieu TO ft_numlieuT
                MOVE 1 TO Wfin
                START fterrain, KEY IS = ft_numlieuT
                INVALID KEY DISPLAY "Lieu sans terrain"
                NOT INVALID KEY
                    DISPLAY " "
                    DISPLAY "________________________________"
                    DISPLAY "Adresse du lieu : "fl_adresse
                    DISPLAY "Identifiant du lieu : " ft_numlieuT
                    DISPLAY "________________________________"
                    DISPLAY " "
                    PERFORM WITH TEST AFTER UNTIL Wfin = 0
                        READ fterrain NEXT 
                        AT END MOVE 0 TO Wfin
                        NOT AT END
                            IF ft_numlieuT = fl_numlieu
                                MOVE 1 TO Wfinfin
                                OPEN INPUT freservation
                                MOVE ft_numterrain to fr_numterrain
                                START freservation, KEY IS = fr_numterrain
                                INVALID KEY DISPLAY 'Pas de réservation sur le terrain : ' ft_numterrain
                                NOT INVALID KEY
                                    PERFORM WITH TEST AFTER UNTIL Wfinfin = 0
                                        READ freservation NEXT
                                        AT END DISPLAY " "
                                            MOVE 0 TO Wfinfin
                                        NOT AT END 
                                            IF ft_numterrain = fr_numterrain
                                               DISPLAY "Numéro : "fr_numterrain
                                               DISPLAY "Num Utilisateur : "fr_numutilisateur
                                               DISPLAY "Date : "fr_date
                                               DISPLAY "Crénaux : "fr_heure
                                               DISPLAY "Matériel : "fr_materiel
                                               DISPLAY "________________________________"
                                            END-IF
                                        END-READ
                                    END-PERFORM
                                END-START
                                CLOSE freservation
                            END-IF
                        END-READ
                    END-PERFORM
                END-START
           END-READ
           CLOSE fterrain
           CLOSE flieu.        

        RECHERCHER_RESERVATION.
           OPEN INPUT freservation 
           IF global_role_user = 1 THEN
               MOVE global_id_user TO fr_numutilisateur
               MOVE global_id_user TO id_utilisateur
           ELSE
               DISPLAY "Rechercher une réservation"
               DISPLAY "________________________________"
               DISPLAY "Entrez l'id de l'utilisateur de la réservation :"
               ACCEPT id_utilisateur
               MOVE id_utilisateur TO fr_numutilisateur
           END-IF
           MOVE 1 TO Wfin
           START freservation KEY IS = fr_numutilisateur
           INVALID KEY DISPLAY "Aucunes reservations"
           NOT INVALID KEY
                    PERFORM WITH TEST AFTER UNTIL Wfin=0
                        READ freservation NEXT
                        AT END MOVE 0 TO Wfin
                        NOT AT  END 
                           IF fr_numutilisateur = id_utilisateur
                           DISPLAY "Numéro : "fr_numterrain
                           DISPLAY "Num Utilisateur : "fr_numutilisateur
                           DISPLAY "Date : "fr_date
                           DISPLAY "Crénaux : "fr_heure
                           DISPLAY "Matériel : "fr_materiel
                           DISPLAY "________________________________"
                           END-IF
                        END-READ
                    END-PERFORM
           END-START
           CLOSE freservation.

        SUPPRIMER_RESERVATION.
           DISPLAY "Supprimer une réservation"
           DISPLAY "________________________________"
           MOVE 0 TO Wtrouve
           open I-O freservation
           DISPLAY "Entrez le numéro de terrain :"
               ACCEPT terrain_saisi

               DISPLAY "Entrez l'heure de la réservation :"
               ACCEPT heure_saisie

               DISPLAY "Entrez la date de la réservation :"
               ACCEPT date_saisie

           perform with test after until Wtrouve = 1
               read freservation NEXT
                   at end move 1 to Wtrouve
                   not at end
                       if fr_heure = heure_saisie and
                       fr_date = date_saisie and
                       fr_numterrain = terrain_saisi
                           display "Reservation trouvé"
               display fr_heure " " fr_date " " fr_numterrain
               display "Confirmer suppression la réservation ? (O/N)"
                           accept Wreponse
                           if Wreponse = "O" or Wreponse = "o"
                               delete freservation
                               display "Réservation supprimée"
                           else
                               display "Suppression annulée"
                           end-if
                           move 1 to Wtrouve
                       end-if
               end-read
           end-perform
           close freservation.

        MODIFIER_RESERVATION.
            DISPLAY "Modifier une réservation"
            DISPLAY "________________________________"
            OPEN I-O freservation
            MOVE 0 TO Wtrouve
            PERFORM UNTIL Wtrouve = 1
                DISPLAY "Entrez le numéro de terrain :"
                ACCEPT terrain_saisi

                DISPLAY "Entrez l'heure de la réservation :"
                ACCEPT heure_saisie

                DISPLAY "Entrez la date de la réservation :"
                ACCEPT date_saisie
                MOVE heure_saisie TO fr_heure
                MOVE terrain_saisi TO fr_numterrain
                MOVE date_saisie TO fr_date
                READ freservation
                INVALID KEY DISPLAY "Reservation introuvable"
                    MOVE 0 TO Wtrouve
                NOT INVALID KEY MOVE 1 TO Wtrouve
                    DISPLAY global_id_user
                    DISPLAY "------"
                    DISPLAY fr_numutilisateur
                    DISPLAY global_role_user
                    IF fr_numutilisateur = global_id_user and global_role_user = 1
                        DISPLAY "Réservation trouvée :"
                        DISPLAY "Nouveau matériel : (actuel: " fr_materiel ")"
                        ACCEPT materiel
                        OPEN I-O futilisateur
                       PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
                           DISPLAY "Nouvel Utilisateur : (actuel: " fr_numutilisateur ")"
                           ACCEPT id_utilisateur
                           MOVE id_utilisateur TO fu_numutilisateur
                           READ futilisateur
                           INVALID KEY  DISPLAY "Utilisateur introuvable"
                                        MOVE 0 TO Wtrouve
                           NOT INVALID KEY MOVE 1 TO Wtrouve
                                CLOSE futilisateur
                           END-READ
                       END-PERFORM                 

                        MOVE id_utilisateur TO fr_numutilisateur
                        MOVE terrain_saisi TO fr_numterrain
                        MOVE date_saisie TO fr_date
                        MOVE heure_saisie TO fr_heure
                        MOVE materiel TO fr_materiel
                        DISPLAY fr_materiel

                        REWRITE tamp_freservation FROM W_freservation
                        IF cr_freservation = "00"
                            DISPLAY "Réservation modifiée avec succès."
                        ELSE
                            DISPLAY "Erreur lors de la modification de la réservation."
                        END-IF
                    ELSE IF global_role_user <> 1
                        DISPLAY "Réservation trouvée :"
                        DISPLAY "Nouveau matériel : (actuel: " fr_materiel ")"
                        ACCEPT materiel
                        OPEN I-O futilisateur
                       PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
                           DISPLAY "Nouvel Utilisateur : (actuel: " fr_numutilisateur ")"
                           ACCEPT id_utilisateur
                           MOVE id_utilisateur TO fu_numutilisateur
                           READ futilisateur
                           INVALID KEY  DISPLAY "Utilisateur introuvable"
                                        MOVE 0 TO Wtrouve
                           NOT INVALID KEY MOVE 1 TO Wtrouve
                                CLOSE futilisateur
                           END-READ
                       END-PERFORM                        

                        MOVE id_utilisateur TO fr_numutilisateur
                        MOVE terrain_saisi TO fr_numterrain
                        MOVE date_saisie TO fr_date
                        MOVE heure_saisie TO fr_heure
                        MOVE materiel TO fr_materiel
                        DISPLAY fr_materiel

                        REWRITE tamp_freservation FROM W_freservation
                        IF cr_freservation = "00"
                            DISPLAY "Réservation modifiée avec succès."
                        ELSE
                            DISPLAY "Erreur lors de la modification de la réservation."
                        END-IF
                    ELSE 
                       DISPLAY "Aucunes réservations."         
                END-READ
            END-PERFORM

            CLOSE freservation.


       
       AJOUT_LIEU.
           OPEN I-O flieu
           MOVE 1 TO Wtrouve
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 0
                   DISPLAY "Numero du lieu: "
                   ACCEPT Wnumlieu
                   MOVE Wnumlieu TO fl_numlieu
                   READ flieu
                   INVALID KEY  DISPLAY " "
                                MOVE Wnumlieu TO fl_numlieu
                                MOVE 0 TO Wtrouve
                   NOT INVALID KEY DISPLAY "Numéro déjà utilisé"
                   END-READ
           END-PERFORM

           OPEN INPUT futilisateur
           MOVE 0 TO Wtrouve
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
               DISPLAY "Numéro de l'utilisateur : "
               ACCEPT Wgerant
               MOVE Wgerant TO fu_numutilisateur
               READ futilisateur
               INVALID KEY  DISPLAY "Utilisateur introuvable"
                            MOVE 0 TO Wtrouve
               NOT INVALID KEY 
                            MOVE Wgerant TO fl_gerant
                            MOVE 1 TO Wtrouve         
               END-READ
           END-PERFORM
           CLOSE futilisateur
    

           MOVE 0 TO Wtrouve
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
               DISPLAY "Adresse du lieu : "
               ACCEPT Wadresse
               MOVE Wadresse TO fl_adresse
               READ flieu KEY IS fl_adresse
               INVALID KEY DISPLAY " " 
                       MOVE 1 TO Wtrouve
               NOT INVALID KEY 
                       IF Wadresse = fl_adresse
                            MOVE 1 TO Wtrouve
                       ELSE 
                       DISPLAY "Adresse non libre"     
                       MOVE 0 TO Wtrouve
                       END-IF  
               END-READ
           END-PERFORM     

           DISPLAY "Nombre de terrain pour le lieu : "
           ACCEPT Wterrain_existant

           MOVE Wadresse TO fl_adresse
           MOVE Wterrain_existant TO fl_terrain_existant

           MOVE W_flieu TO tamp_flieu
       
           WRITE tamp_flieu
           END-WRITE
           IF cr_flieu = "00"
               DISPLAY "Lieu ajoutée avec succès."
           ELSE
               DISPLAY "Erreur lors de l'ajout du lieu. Gérant appartenant déja à un lieu"   
           CLOSE flieu.

       AFFICHAGE_LIEU.
           OPEN INPUT flieu
           MOVE 1 TO Wfin
           PERFORM WITH TEST AFTER UNTIL Wfin=0
                    READ flieu NEXT
                    AT END MOVE 0 TO Wfin
                    NOT AT END
                       DISPLAY "Numéro : "fl_numlieu
                       DISPLAY "Gérant : "fl_gerant
                       DISPLAY "Adresse : "fl_adresse
                       DISPLAY "Nombre de terrain : "fl_terrain_existant
                       DISPLAY "________________________________"
                    END-READ
           END-PERFORM
           CLOSE flieu.

       MODIF-LIEU.
           OPEN I-O flieu
           MOVE 0 TO Wtrouve
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
               DISPLAY "Numéro du lieu à modifier : "
               ACCEPT Wnumlieu
               MOVE Wnumlieu TO fl_numlieu
               READ flieu
               INVALID KEY  DISPLAY "Lieu introuvable"
                            MOVE 0 TO Wtrouve
               NOT INVALID KEY MOVE 1 TO Wtrouve
               END-READ
           END-PERFORM
    
           IF Wtrouve = 1

           OPEN I-O futilisateur
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
               DISPLAY "Numéro de l'utilisateur : "
               ACCEPT Wgerant
               MOVE Wgerant TO fu_numutilisateur
               READ futilisateur
               INVALID KEY  DISPLAY "Utilisateur introuvable"
                            MOVE 0 TO Wtrouve
               NOT INVALID KEY MOVE 1 TO Wtrouve
               END-READ
           END-PERFORM
           CLOSE futilisateur
           
           MOVE 0 TO Wtrouve
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
               DISPLAY "Nouvelle adresse (ville) : ( actuel "fl_adresse" )"
               ACCEPT Wadresse
               MOVE Wadresse TO fl_adresse
               READ flieu KEY IS fl_adresse
               INVALID KEY DISPLAY " " 
                       MOVE 1 TO Wtrouve
               NOT INVALID KEY 
                       IF Wadresse = fl_adresse
                            MOVE 1 TO Wtrouve
                       ELSE 
                       DISPLAY "Adresse non libre"     
                       MOVE 0 TO Wtrouve
                       END-IF  
               END-READ
           END-PERFORM    
           DISPLAY "Nouveau nombre de terrain : ( actuel "
           fl_terrain_existant" )"
           ACCEPT Wterrain_existant

           MOVE Wgerant TO fl_gerant
           Move Wadresse TO fl_adresse
           MOVE Wterrain_existant TO fl_terrain_existant

           REWRITE tamp_flieu FROM W_flieu
           IF cr_flieu = "00"
           DISPLAY "Lieu modifié avec succès."
           ELSE
               DISPLAY "Erreur lors de la mise à jour du lieu. Gérant appartenant déja à un lieu ou "   
           END-IF
           CLOSE flieu.

       SUPPRIMER_LIEU.
           open I-O flieu
           display "Suppression d'un lieu"
           accept Wnumlieu
           MOVE Wnumlieu TO fl_numlieu
           MOVE 0 TO Wtrouve
           perform with test after until Wtrouve = 1
               read flieu
                   INVALID KEY move 1 to Wtrouve
                        display "Lieu introuvable"
                   NOT INVALID KEY
                       display "Lieu trouvé"
                       display fl_gerant " " fl_adresse " " fl_terrain_existant
                       display "Confirmer suppression lieu ? (O/N)"
                       accept Wreponse
                       if Wreponse = "O" or Wreponse = "o"
                            OPEN I-O fterrain
                            MOVE 0 TO Wfin
                            MOVE fl_numlieu TO ft_numlieuT
                            START fterrain,KEY IS = ft_numlieuT
                            INVALID KEY DISPLAY "PAS DE TERRAINS"
                            NOT INVALID KEY
                                perform with test after until Wfin = 1
                                   read fterrain NEXT
                                    AT END move 1 to Wfin
                                        display "Aucuns terrains associés"
                                    NOT AT END         
                                        display "Terrain trouvé"
                                           OPEN I-O freservation
                                                MOVE ft_numterrain to fr_numterrain
                                                START freservation, KEY IS = fr_numterrain
                                                INVALID KEY DISPLAY 'Pas de réservation sur le terrain : ' ft_numterrain
                                                NOT INVALID KEY
                                                    PERFORM WITH TEST AFTER UNTIL Wfinfin = 0
                                                        READ freservation NEXT
                                                        AT END DISPLAY " "
                                                            MOVE 0 TO Wfinfin
                                                        NOT AT END 
                                                            IF ft_numterrain = fr_numterrain
                                                               DISPLAY "Numéro : "fr_numterrain
                                                               DISPLAY "Num Utilisateur : "fr_numutilisateur
                                                               DISPLAY "Date : "fr_date
                                                               DISPLAY "Crénaux : "fr_heure
                                                               DISPLAY "Matériel : "fr_materiel
                                                               DISPLAY "________________________________"
                                                               delete freservation
                                                            END-IF
                                                        END-READ
                                                    END-PERFORM
                                                END-START
                                                CLOSE freservation
                                           delete fterrain
                                           display "Terrain et reservation supprimés "
                                       move 1 to Wtrouve
                                   end-read
                                END-PERFORM
                            END-START
                            close fterrain   
                           delete flieu
                           display "Lieu supprimé"
                       else
                           display "Suppression annulée"
                       end-if
                       move 1 to Wtrouve
               end-read
           end-perform
           close flieu.

       AJOUT_TERRAIN.
           OPEN I-O fterrain
           IF global_role_user = 2
           OPEN INPUT flieu
           MOVE global_id_user TO fl_gerant
           READ flieu NEXT
           AT END DISPLAY "Gérant au chomage"
           NOT AT END 
                MOVE fl_numlieu TO WnumlieuT
                MOVE WnumlieuT TO ft_numlieuT
           END-READ
           CLOSE flieu
           ELSE
           OPEN I-O flieu
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
               DISPLAY "Numéro de lieu : "
               ACCEPT WnumlieuT
               MOVE WnumlieuT TO fl_numlieu
               READ flieu
               INVALID KEY  DISPLAY "Lieu introuvable"
                            MOVE 0 TO Wtrouve
               NOT INVALID KEY MOVE 1 TO Wtrouve
                           MOVE WnumlieuT TO ft_numlieuT
               END-READ
           END-PERFORM
           CLOSE flieu
           END-IF
     
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 0
                   DISPLAY "Numero du terrain :"
                   ACCEPT Wnumterrain
                   MOVE Wnumterrain TO ft_numterrain
                   READ fterrain
                   INVALID KEY  DISPLAY " "
                                MOVE 0 TO Wtrouve
                   NOT INVALID KEY DISPLAY "Numéro déjà utilisé"
                                   MOVE 1 TO Wtrouve
                   END-READ
           END-PERFORM

        
           DISPLAY "Longueur : "
           ACCEPT Wlongueur
           DISPLAY "Largeur : "
           ACCEPT Wlargeur
           DISPLAY "Type : "
           ACCEPT Wtype
           DISPLAY "Prix : "
           ACCEPT Wprix
           DISPLAY "Couvert (O/N) : "
           ACCEPT Wcouvert

           MOVE Wlongueur to ft_longueur
           MOVE Wlargeur to ft_largeur
           MOVE Wtype to ft_type
           MOVE Wprix to ft_prix
           MOVE Wcouvert to ft_couvert 
       
           MOVE W_fterrain to tamp_fterrain
           WRITE tamp_fterrain
           END-WRITE
           CLOSE fterrain.


       AFFICHAGE_TERRAIN.
           OPEN INPUT fterrain
           MOVE 1 TO Wfin
           PERFORM WITH TEST AFTER UNTIL Wfin=0
                   READ fterrain NEXT
                   AT END MOVE 0 TO Wfin
                   NOT AT END
                       DISPLAY "Numéro : "ft_numterrain
                       DISPLAY "Lieu : "ft_numlieuT
                       DISPLAY "Longueur : "ft_longueur
                       DISPLAY "Largeur : "ft_largeur
                       DISPLAY "Type : "ft_type
                       DISPLAY "Prix : "ft_prix
                       DISPLAY "Couvert : "ft_couvert
                       DISPLAY "________________________________"
                   END-READ
           END-PERFORM
           CLOSE fterrain.

       AFFICHAGE_TERRAIN_GERANT.
       OPEN INPUT flieu
           MOVE global_id_user TO fl_gerant
           READ flieu NEXT
           AT END DISPLAY "Gérant au chomage"
           NOT AT END 
                MOVE fl_numlieu TO Wnumlieu
                OPEN INPUT fterrain
                MOVE Wnumlieu TO ft_numlieuT
                MOVE 1 TO Wfin
                START fterrain, KEY IS = ft_numlieuT
                INVALID KEY DISPLAY "Le gérant est associé à un lieu sans terrain"
                NOT INVALID KEY
                    DISPLAY " "
                    DISPLAY "________________________________"
                    DISPLAY "Adresse du lieu : "fl_adresse
                    DISPLAY "Identifiant du lieu : " ft_numlieuT
                    DISPLAY "________________________________"
                    DISPLAY " "
                    PERFORM WITH TEST AFTER UNTIL Wfin = 0
                        READ fterrain NEXT 
                        AT END MOVE 0 TO Wfin
                        NOT AT END
                            IF ft_numlieuT = fl_numlieu
                               DISPLAY "Numéro : "ft_numterrain
                               DISPLAY "Lieu : "ft_numlieuT
                               DISPLAY "Longueur : "ft_longueur
                               DISPLAY "Largeur : "ft_largeur
                               DISPLAY "Type : "ft_type
                               DISPLAY "Prix : "ft_prix
                               DISPLAY "Couvert : "ft_couvert
                               DISPLAY "________________________________"   
                            END-IF
                        END-READ
                    END-PERFORM
                END-START
           END-READ
           CLOSE fterrain
           CLOSE flieu. 

       MODIFIER_TERRAIN.
           OPEN I-O fterrain
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
               DISPLAY "Numéro du terrain à modifier : "
               ACCEPT Wnumterrain
               MOVE Wnumterrain TO ft_numterrain
               READ fterrain
               INVALID KEY  DISPLAY "Terrain introuvable"
                            MOVE 0 TO Wtrouve
               NOT INVALID KEY MOVE 1 TO Wtrouve
               END-READ
           END-PERFORM
    
           IF Wtrouve = 1
           OPEN I-O flieu
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
               DISPLAY "Numéro de lieu : "
               ACCEPT WnumlieuT
               MOVE WnumlieuT TO fl_numlieu
               READ flieu
               INVALID KEY  DISPLAY "Lieu introuvable"
                            MOVE 0 TO Wtrouve
               NOT INVALID KEY MOVE 1 TO Wtrouve
                           MOVE WnumlieuT TO ft_numlieuT
               END-READ
           END-PERFORM
           CLOSE flieu
            
           DISPLAY "Nouvelle longueur : (actuelle " ft_longueur")"
           ACCEPT Wlongueur
           DISPLAY "Nouvelle largeur : (actuelle " ft_largeur")"
           ACCEPT Wlargeur
           DISPLAY "Nouveau type : (actuel " ft_type")"
           ACCEPT Wtype
           DISPLAY "Nouveau prix : (actuel " ft_prix")"
           ACCEPT Wprix
           DISPLAY "Couvert (O/N) : (actuel " ft_couvert")"
           ACCEPT Wcouvert

           MOVE Wlongueur TO ft_longueur
           MOVE Wlargeur TO ft_largeur
           MOVE Wtype TO ft_type
           Move Wprix TO ft_prix
           MOVE Wcouvert TO ft_couvert
    
           REWRITE tamp_fterrain FROM W_fterrain
           DISPLAY "Terrain modifié avec succès"
           END-IF
           CLOSE fterrain.

       SUPPRIMER_TERRAIN.
           open I-O fterrain
           display "Suppression d'un terrain"
           DISPLAY "Numéro de terrain : "
           accept Wnumterrain
           MOVE 0 TO Wtrouve
           MOVE Wnumterrain TO ft_numterrain
           perform with test after until Wtrouve = 1
               read fterrain
                    INVALID KEY move 1 to Wtrouve
                        display "Terrain incorrect"
                    NOT INVALID KEY         
                        display "Terrain trouvé"
                        display "Confirmer suppression terrain ? (O/N)"
                        accept Wreponse
                        if Wreponse = "O" or Wreponse = "o"
                           OPEN I-O freservation
                                MOVE Wnumterrain to fr_numterrain
                                START freservation, KEY IS = fr_numterrain
                                INVALID KEY DISPLAY 'Pas de réservation sur le terrain : ' ft_numterrain
                                NOT INVALID KEY
                                    PERFORM WITH TEST AFTER UNTIL Wfinfin = 0
                                        READ freservation NEXT
                                        AT END DISPLAY " "
                                            MOVE 0 TO Wfinfin
                                        NOT AT END 
                                            IF ft_numterrain = fr_numterrain
                                               DISPLAY "Numéro : "fr_numterrain
                                               DISPLAY "Num Utilisateur : "fr_numutilisateur
                                               DISPLAY "Date : "fr_date
                                               DISPLAY "Crénaux : "fr_heure
                                               DISPLAY "Matériel : "fr_materiel
                                               DISPLAY "________________________________"
                                               delete freservation
                                            END-IF
                                        END-READ
                                    END-PERFORM
                                END-START
                                CLOSE freservation
                           delete fterrain
                           display "Terrain et reservation supprimés "
                       else
                           display "Suppression annulée"
                       end-if
                       move 1 to Wtrouve
               end-read
           end-perform
           close fterrain.

           COLLECTER_STATISTIQUES.
           OPEN I-O fstat
           DISPLAY "Rechercher des statistiques"
           DISPLAY "________________________________"
           
           MOVE 0 TO Wtrouve
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 1 
               DISPLAY "Entrez le mois :"
               ACCEPT WmoisS   
               IF WmoisS(1:2) IS NUMERIC AND
                  WmoisS(1:2) > 0 AND
                  WmoisS(1:2) <= 12 AND 
                  WmoisS(1:2) < WS-CURRENT-MONTH            
                  MOVE 1 TO Wtrouve
               ELSE
                  DISPLAY "Mois invalide. Veuillez réessayer."
               END-IF
           END-PERFORM
       
           MOVE 0 TO Wtrouve
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
               DISPLAY "Veuillez saisir l'adresse du lieu :"
               OPEN INPUT flieu
               ACCEPT WlieuS
               MOVE WlieuS TO fl_adresse
               READ flieu RECORD KEY IS fl_adresse
               INVALID KEY DISPLAY "Aucunes correspondances avec nos centres."                          
               NOT INVALID KEY
                    MOVE 1 TO Wtrouve
               END-READ
           END-PERFORM   
           
           OPEN INPUT fterrain
           OPEN INPUT flieu
           MOVE 0 TO Wnb_reservationS
           MOVE 0 TO Wreservation_gazon
           MOVE 0 TO Wreservation_synthetique
           MOVE 0 TO Wreservation_falin
           MOVE 0 TO Wnb_reservation_materiel
               
           MOVE WlieuS TO fl_adresse
           READ flieu RECORD KEY IS fl_adresse
           
           NOT INVALID KEY
               MOVE fl_numlieu TO ft_numlieuT
               START fterrain, KEY IS = ft_numlieuT
               
               NOT INVALID KEY
                   MOVE 1 TO Wfinfin  
                   PERFORM WITH TEST AFTER UNTIL Wfinfin = 0
                   READ fterrain NEXT 
                   AT END
                   MOVE 0 TO Wfinfin
                   NOT AT END
                       MOVE 1 TO WfinfinS 
                       OPEN INPUT freservation
                       PERFORM WITH TEST AFTER UNTIL WfinfinS = 0
                       READ freservation NEXT 
                       AT END
                       MOVE 0 TO WfinfinS   
                       NOT AT END
                           IF fr_date(3:2) = WmoisS AND fl_adresse = WlieuS AND fr_numterrain = ft_numterrain
                               ADD 1 TO Wnb_reservationS
                               IF fr_materiel = "Oui" OR fr_materiel = "oui" OR fr_materiel = "OUI"
                                   ADD 1 TO Wnb_reservation_materiel
                               END-IF
                               IF ft_type = "gazon" OR ft_type = "GAZON" 
                                   ADD 1 TO Wreservation_gazon
                               ELSE IF ft_type = "synthétique" OR ft_type = "SYNTHÉTIQUE"
                                   ADD 1 TO Wreservation_synthetique
                               ELSE IF ft_type = "falin" OR ft_type = "FALIN"
                                   ADD 1 TO Wreservation_falin
                               END-IF
                            END-IF
                          END-READ
                        END-PERFORM
                        CLOSE freservation  
                   END-READ
                   END-PERFORM
                   END-START     
           END-READ  
           
           CLOSE fterrain
           CLOSE flieu      
       
           MoVE WlieuS TO fs_lieu
           MOVE WmoisS TO fs_mois
           MOVE Wnb_reservationS TO fs_nb_reservation
           MOVE Wreservation_gazon TO fs_type_reservation_gazon
           MOVE Wreservation_synthetique TO fs_type_reservation_synthetique    
           MOVE Wreservation_falin TO fs_type_reservation_falin
           MOVE Wnb_reservation_materiel TO fs_nb_reservation_materiel

           REWRITE tamp_fstat FROM W_fstat 
           READ fstat RECORD KEY IS fs_cle
           INVALID KEY
               WRITE tamp_fstat FROM W_fstat
               INVALID KEY DISPLAY "Erreur écriture des statistiques"
           NOT INVALID KEY
           REWRITE tamp_freservation FROM W_freservation
               REWRITE tamp_fstat FROM W_fstat
               INVALID KEY DISPLAY "Erreur mise à jour des statistiques"
           END-READ       
           
           READ fstat
           INVALID KEY DISPLAY "Aucunes statistiques disponibles"
           NOT INVALID KEY       
           IF fs_mois = WmoisS AND fs_lieu  = WlieuS
           DISPLAY "________________________________"
           DISPLAY "Affichage des statistiques pour le mois et le lieu choisi :"
           DISPLAY "Lieu : "fs_lieu
           DISPLAY "Mois : "fs_mois
           DISPLAY "Nombre de réservation sur le mois : "fs_nb_reservation
           DISPLAY "Nombre de réservation avec type gazon : "fs_type_reservation_gazon
           DISPLAY "Nombre de réservation avec type synthétique : "fs_type_reservation_synthetique
           DISPLAY "Nombre de réservation avec type falin : "fs_type_reservation_falin
           DISPLAY "Nombre de réservation avec matériel : "fs_nb_reservation_materiel
           DISPLAY "________________________________"
           END-IF
           END-READ
           CLOSE fstat.

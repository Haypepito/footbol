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
           ALTERNATE RECORD KEY IS fu_mail
           FILE STATUS IS cr_futilisateur.
       
           SELECT flieu ASSIGN TO "lieux.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS fl_numlieu
           ALTERNATE RECORD KEY IS fl_adresse WITH DUPLICATES
           ALTERNATE RECORD KEY IS fl_gerant WITH DUPLICATES
           FILE STATUS IS cr_flieu.
       
           SELECT freservation ASSIGN TO "reservations.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS fr_cleres
           ALTERNATE RECORD KEY IS fr_numutilisateur WITH DUPLICATES
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
             02 fu_mail PIC A(50).
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
                03 fr_date PIC A(10).
             02 fr_numutilisateur PIC 9(10).
             02 fr_materiel PIC A(3).
       
       FD fterrain.
          01 tamp_fterrain.
             02 ft_numterrain PIC 9(9).
             02 ft_lieu PIC A(50).
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
             02 fs_type_reservation_gazon PIC 9(9).
             02 fs_type_reservation_synthetique PIC 9(9).
             02 fs_type_reservation_falin PIC 9(9).
             02 fs_materiel PIC 9(9).
       
       WORKING-STORAGE SECTION.

              77 cr_futilisateur PIC 9(2).
              77 cr_flieu PIC 9(2).
              77 cr_fterrain PIC 9(2).
              77 cr_fstat PIC 9(2).
              77 cr_freservation PIC 9(2).
              77 Wtrouve PIC 9(1).
              77 Wfin PIC 9(1).
              77 Wmail_valide PIC 9(1).
              77 Wreponse PIC A(1).
              77 attempts PIC 9(1).
              01 W_freservation.
                02 W_fr_cle.
                  03 terrain_saisi PIC 9(9).
                  03 heure_saisie PIC 9(2).
                  03 date_saisie PIC A(10).
                02 id_utilisateur PIC 9(10).  
                02 materiel PIC A(3).
               01 W_futilisateur.
                02 Wnumutilisateur PIC 9(9).
                02 Wnom PIC A(30).
                02 Wprenom PIC A(30).
                02 Wmail PIC A(50).
                02 Wmdp PIC 9(20).
                02 Wrole PIC 9(1).
                02 Wconnecte PIC A.
               01 W_flieu.
                02 Wnumlieu PIC 9(9).
                02 Wgerant PIC 9(9).
                02 Wadresse PIC A(50).
                02 Wterrain_existant PIC 9(2).
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

                PERFORM AFFICHAGE_UTILISATEUR.

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
                       DISPLAY "Entrez votre email :"
                       ACCEPT Wmail
                       DISPLAY "Entrez votre mot de passe :"
                       ACCEPT Wmdp
                       SET Wtrouve TO 0
                       SET attempts TO 0
                       MOVE Wmail TO fu_mail
                       PERFORM UNTIL Wtrouve = 1 OR attempts = 3
                           READ futilisateur RECORD KEY IS fu_mail
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
                                       DISPLAY "Mot de passe ou mail incorrect. Essayez encore."
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
                PERFORM UNTIL exitmenu = 'U'
                    DISPLAY "________________________________________________________________"
                    DISPLAY "         Bienvenue dans le menu amateur de FootBol "
                    DISPLAY "________________________________________________________________"
                    DISPLAY "1. Modifier un utilisateur   "
                    DISPLAY "2. Ajout d'une réservation   "
                    DISPLAY "3. Modifier une réservation   "
                    DISPLAY "4. Rerchercher une réservation"
                    DISPLAY "5. Se déconnecter "
                    DISPLAY "0. Quitter"
                    DISPLAY "Entrez votre choix (0-5):"
                    ACCEPT choice

                    EVALUATE choice
                        WHEN '1'
                            PERFORM MODIF_UTILISATEUR
                        WHEN '2'
                            PERFORM AJOUT_RESERVATION
                        WHEN '3'
                            PERFORM MODIFIER_RESERVATION
                        WHEN '4'
                            PERFORM RECHERCHER_RESERVATION    
                        WHEN '5'
                            DISPLAY "Vous êtes bien déconnecté"
                            PERFORM DECONNECTER_UTILISATEUR
                            PERFORM CONNEXION_UTILISATEUR
                        WHEN '0'
                            MOVE 'U' TO exitmenu
                        WHEN OTHER
                            DISPLAY "Choix invalide. Veuillez réessayer."
                    END-EVALUATE
                END-PERFORM.

        MENU_GERANT.
            PERFORM UNTIL exitmenu = 'R'
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
                        MOVE 'R' TO exitmenu
                    WHEN OTHER
                        DISPLAY "Choix invalide. Veuillez réessayer."
                END-EVALUATE
            END-PERFORM. 

        MENU_ADMIN.
            PERFORM UNTIL exitmenu = 'R'
                DISPLAY "________________________________________________________________"
                DISPLAY "       Bienvenue dans le menu Administrateur de FootBol "
                DISPLAY "________________________________________________________________"
                DISPLAY "1. Utilisateur"
                DISPLAY "2. Réservation"
                DISPLAY "3. Terrain"
                DISPLAY "4. Lieu"
                DISPLAY "5. Se déconnecter"
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
                        PERFORM MENU_LIEU
                    WHEN '5'
                        DISPLAY "Vous êtes bien déconnecté"
                        PERFORM DECONNECTER_UTILISATEUR
                        PERFORM CONNEXION_UTILISATEUR
                    WHEN '0'
                        MOVE 'R' TO exitmenu
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
                PERFORM UNTIL exitmenu = 'U'
                    DISPLAY "________________________________________________________________"
                    DISPLAY "       Bienvenue dans le menu reservation de FootBol "
                    DISPLAY "________________________________________________________________"
                    DISPLAY "1. Affichage des réservation"
                    DISPLAY "2. Ajout d'une réservation"
                    DISPLAY "3. Modifier une réservation   "
                    DISPLAY "4. Rerchercher une réservation"
                    DISPLAY "5. Supprimer un utilisateur   "
                    DISPLAY "0. Retour"
                    DISPLAY "Entrez votre choix (0-5):"
                    ACCEPT choice

                    EVALUATE choice
                        WHEN '1'
                            PERFORM AFFICHAGE_RESERVATION
                        WHEN '2'
                            PERFORM AJOUT_RESERVATION
                        WHEN '3'
                            PERFORM MODIFIER_RESERVATION
                        WHEN '4'
                            PERFORM RECHERCHER_RESERVATION
                        WHEN '5'
                            PERFORM SUPPRIMER_RESERVATION
                        WHEN '0'
                            PERFORM MENU_GERANT
                        WHEN OTHER
                            DISPLAY "Choix invalide. Veuillez réessayer."
                    END-EVALUATE
                END-PERFORM.

           MENU_TERRAIN.
                PERFORM UNTIL exitmenu = 'U'
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
                            PERFORM AFFICHAGE_TERRAIN
                        WHEN '2'
                            PERFORM AJOUT_TERRAIN
                        WHEN '3'
                            PERFORM MODIFIER_TERRAIN
                        WHEN '4'
                            PERFORM SUPPRIMER_TERRAIN
                        WHEN '0'
                            PERFORM MENU_GERANT
                        WHEN OTHER
                            DISPLAY "Choix invalide. Veuillez réessayer."
                    END-EVALUATE
                END-PERFORM.

           MENU_LIEU.
                PERFORM UNTIL exitmenu = 'U'
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

           DISPLAY "Entrez votre email :"
           ACCEPT Wmail

           PERFORM WITH TEST AFTER UNTIL Wtrouve = 0
           READ futilisateur
               AT END MOVE 0 TO Wtrouve
               NOT AT END
                   IF fu_mail = Wmail THEN
                       MOVE 1 TO Wtrouve
                       DISPLAY "L'email est déjà utilisé."
                       DISPLAY "Entrez votre email :"
                       ACCEPT Wmail
                   END-IF
           END-READ
           END-PERFORM

           DISPLAY "Rôle : "
           ACCEPT Wrole
           DISPLAY "Mdp : "
           ACCEPT Wmdp
           MOVE 'N' TO Wconnecte 
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
               MOVE fu_role TO Wrole
               
               DISPLAY "Nouveau nom : ( actuel " fu_nom" )"
               ACCEPT Wnom
               DISPLAY "Nouveau prénom : ( actuel " fu_prenom" )"
               ACCEPT Wprenom
               DISPLAY "Nouveau mail : ( actuel " fu_mail" )"
               ACCEPT Wmail
               DISPLAY "Nouveau mdp : ( actuel " fu_mdp" )"
               ACCEPT Wmdp

               MOVE Wnom TO fu_nom
               Move Wprenom TO fu_prenom
               MOVE Wmail TO fu_mail
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
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
               MOVE global_id_user TO fu_numutilisateur
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
               MOVE fu_mail TO Wmail
               
               DISPLAY "Nouveau rôle : ( actuel " fu_role" )"
               ACCEPT Wrole

               MOVE Wnom TO fu_nom
               Move Wprenom TO fu_prenom
               MOVE Wmail TO fu_mail
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
           read futilisateur
               at end move 1 to Wtrouve
               not at end
                   if fu_nom = Wnom and fu_prenom = Wprenom
                       display "Utilisateur trouvé"
           display fu_nom " " fu_prenom " " fu_mail" "fu_role" "fu_mdp
           display "Confirmer suppression utilisateur ? (O/N)"
                       accept Wreponse
                       if Wreponse = "O" or Wreponse = "o"
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
                    READ futilisateur
                    AT END MOVE 0 TO Wfin
                    NOT AT END
                       DISPLAY "Numéro : ["fu_numutilisateur"]"
                       DISPLAY "Prénom : [" fu_prenom"]"
                       DISPLAY "Nom : ["fu_nom "]"
                       DISPLAY "Mail : ["fu_mail "]"
                       DISPLAY "Role : ["fu_role "]"
                       DISPLAY "Connecté : ["fu_connecte "]"
                       DISPLAY "Mdp : ["fu_mdp "]"
                       DISPLAY "________________________________"
                    END-READ
            END-PERFORM
            CLOSE futilisateur.

       AJOUT_RESERVATION.
           DISPLAY "Ajouter une réservation"
           DISPLAY "________________________________"
           OPEN I-O freservation
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 0 
               DISPLAY "Entrez le numéro de terrain :"
               ACCEPT terrain_saisi

               DISPLAY "Entrez l'heure de la réservation :"
               ACCEPT heure_saisie

               DISPLAY "Entrez la date de la réservation :"
               ACCEPT date_saisie

               MOVE terrain_saisi TO fr_numterrain
               MOVE heure_saisie TO fr_heure
               MOVE date_saisie TO fr_date 
               READ freservation
                   INVALID KEY  DISPLAY " "
                                    MOVE 0 TO Wtrouve
                       NOT INVALID KEY DISPLAY "La réservation existe déjà."
                                       MOVE 1 TO Wtrouve
               END-READ 
           END-PERFORM

           DISPLAY "Entrez l'id de l'utilisateur de la réservation :"
           ACCEPT id_utilisateur
           MOVE id_utilisateur TO fr_numutilisateur
           DISPLAY "Location de matériel (Oui/Non) :"
           ACCEPT materiel
           MOVE materiel TO fr_materiel
           WRITE tamp_freservation
           IF cr_freservation = "00"
               DISPLAY "Réservation ajoutée avec succès."
           ELSE
               DISPLAY "Erreur lors de l'ajout de la réservation."   
           CLOSE freservation.

       AFFICHAGE_RESERVATION.
           DISPLAY "Toutes les réservations"
           DISPLAY "________________________________" 
           OPEN INPUT freservation
           MOVE 1 TO Wfin
           PERFORM WITH TEST AFTER UNTIL Wfin=0
                    READ freservation
                    AT END MOVE 0 TO Wfin
                    NOT AT END
                       DISPLAY "Numéro : ["fr_numterrain"]"
                       DISPLAY "Num Utilisateur : ["fr_numutilisateur"]"
                       DISPLAY "Date : ["fr_date"]"
                       DISPLAY "Crénaux : ["fr_heure"]"
                       DISPLAY "Matériel : ["fr_materiel"]"
                       DISPLAY "________________________________"
                    END-READ
           END-PERFORM
           CLOSE freservation.

        RECHERCHER_RESERVATION.
           OPEN INPUT freservation 
           DISPLAY "Rechercher une réservation"
           DISPLAY "________________________________" 
           DISPLAY "Entrez l'id de l'utilisateur de la réservation :"
           ACCEPT id_utilisateur
           MOVE id_utilisateur TO fr_numutilisateur
           MOVE 1 TO Wfin
           START freservation KEY IS = fr_numutilisateur
           INVALID KEY DISPLAY "Aucunes reservations"
           NOT INVALID KEY
                    PERFORM WITH TEST AFTER UNTIL Wfin=0
                        READ freservation NEXT
                        AT END MOVE 0 TO Wfin
                        NOT AT  END 
                           DISPLAY "Numéro : ["fr_numterrain"]"
                           DISPLAY "Num Utilisateur : ["fr_numutilisateur"]"
                           DISPLAY "Date : ["fr_date"]"
                           DISPLAY "Crénaux : ["fr_heure"]"
                           DISPLAY "Matériel : ["fr_materiel"]"
                           DISPLAY "________________________________"
                        END-READ
                    END-PERFORM
           END-START
           CLOSE freservation.

        SUPPRIMER_RESERVATION.
           DISPLAY "Supprimer une réservation"
           DISPLAY "________________________________"
           open I-O freservation
           DISPLAY "Entrez le numéro de terrain :"
               ACCEPT terrain_saisi

               DISPLAY "Entrez l'heure de la réservation :"
               ACCEPT heure_saisie

               DISPLAY "Entrez la date de la réservation :"
               ACCEPT date_saisie

           perform with test after until Wtrouve = 1
               read freservation
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
            PERFORM UNTIL Wtrouve = 1
                DISPLAY "Entrez le numéro de terrain :"
                ACCEPT terrain_saisi

                DISPLAY "Entrez l'heure de la réservation :"
                ACCEPT heure_saisie

                DISPLAY "Entrez la date de la réservation :"
                ACCEPT date_saisie
                MOVE id_utilisateur TO fr_numutilisateur
                MOVE terrain_saisi TO fr_numterrain
                MOVE date_saisie TO fr_date
                READ freservation
                INVALID KEY DISPLAY "Utilisateur introuvable"
                    MOVE 0 TO Wtrouve
                NOT INVALID KEY MOVE 1 TO Wtrouve
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
                END-READ
            END-PERFORM

            CLOSE freservation.


       
       AJOUT_LIEU.
           OPEN I-O flieu
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 0
                   DISPLAY "Numero du lieu: "
                   ACCEPT Wnumlieu
                   MOVE Wnumlieu TO fl_numlieu
                   READ flieu
                   INVALID KEY  DISPLAY " "
                                MOVE 0 TO Wtrouve
                   NOT INVALID KEY DISPLAY "Numéro déjà utilisé"
                                   MOVE 1 TO Wtrouve
                   END-READ
           END-PERFORM

           OPEN I-O futilisateur
           PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
               DISPLAY "Numéro de l'utilisateur : "
               ACCEPT Wgerant
               MOVE Wgerant TO fu_numutilisateur
               READ futilisateur
               INVALID KEY  DISPLAY "Utilisateur introuvable"
                            MOVE 0 TO Wtrouve
               NOT INVALID KEY MOVE 1 TO Wtrouve
                           MOVE Wgerant TO fl_gerant
               END-READ
           END-PERFORM
           CLOSE futilisateur
    
           DISPLAY "Adresse du lieu : "
           ACCEPT Wadresse

           DISPLAY "Nombre de terrain pour le lieu : "
           ACCEPT Wterrain_existant
       
           MOVE W_flieu to tamp_flieu
           WRITE tamp_flieu
           END-WRITE
           CLOSE flieu.

       AFFICHAGE_LIEU.
           OPEN INPUT flieu
           MOVE 1 TO Wfin
           PERFORM WITH TEST AFTER UNTIL Wfin=0
                    READ flieu
                    AT END MOVE 0 TO Wfin
                    NOT AT END
                       DISPLAY "Numéro : ["fl_numlieu"]"
                       DISPLAY "Gérant : ["fl_gerant"]"
                       DISPLAY "Adresse : ["fl_adresse"]"
                       DISPLAY "Nombre de terrain : ["
                       fl_terrain_existant"]"
                       DISPLAY "________________________________"
                    END-READ
           END-PERFORM
           CLOSE flieu.

       MODIF-LIEU.
           OPEN I-O flieu
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
               
               DISPLAY "Nouvelle adresse : ( actuel "fl_adresse" )"
               ACCEPT Wadresse
               DISPLAY "Nouveau nombre de terrain : ( actuel "
               fl_terrain_existant" )"
               ACCEPT Wterrain_existant

               MOVE Wgerant TO fl_gerant
               Move Wadresse TO fl_adresse
               MOVE Wterrain_existant TO fl_terrain_existant
    
               REWRITE tamp_flieu FROM W_flieu
               DISPLAY "Lieu modifié avec succès"
           END-IF
           CLOSE flieu.

       SUPPRIMER_LIEU.
           open I-O flieu
           display "Suppression d'un lieu"
           accept Wnumlieu
           perform with test after until Wtrouve = 1
               read flieu
                   at end move 1 to Wtrouve
                   not at end
                       if fl_numlieu = Wnumlieu
                           display "Lieu trouvé"
               display fl_gerant " " fl_adresse " " fl_terrain_existant
               display "Confirmer suppression lieu ? (O/N)"
                           accept Wreponse
                           if Wreponse = "O" or Wreponse = "o"
                               delete flieu
                               display "Lieu supprimé"
                           else
                               display "Suppression annulée"
                           end-if
                           move 1 to Wtrouve
                       end-if
               end-read
           end-perform
           close flieu.

       AJOUT_TERRAIN.
           OPEN I-O fterrain
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
       
           MOVE W_fterrain to tamp_fterrain
           WRITE tamp_fterrain
           END-WRITE
           CLOSE fterrain.

       AFFICHAGE_TERRAIN.
           OPEN INPUT fterrain
           MOVE 1 TO Wfin
           PERFORM WITH TEST AFTER UNTIL Wfin=0
                   READ fterrain
                   AT END MOVE 0 TO Wfin
                   NOT AT END
                       DISPLAY "Numéro : ["ft_numterrain"]"
                       DISPLAY "Lieu : ["ft_numlieuT"]"
                       DISPLAY "Longueur : ["ft_longueur"]"
                       DISPLAY "Largeur : ["ft_largeur"]"
                       DISPLAY "Type : ["ft_type "]"
                       DISPLAY "Prix : ["ft_prix "]"
                       DISPLAY "Couvert : ["ft_couvert "]"
                       DISPLAY "________________________________"
                   END-READ
           END-PERFORM
           CLOSE fterrain.

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
           DISPLAY "Numéro de lieu : "
           accept WnumlieuT
           perform with test after until Wtrouve = 1
               read fterrain
                   at end move 1 to Wtrouve
                   not at end
                       if ft_numterrain = Wnumterrain and ft_numlieuT =
                       WnumlieuT
                           display "Terrain trouvé"
               display "Confirmer suppression terrain ? (O/N)"
                           accept Wreponse
                           if Wreponse = "O" or Wreponse = "o"
                               delete fterrain
                               display "Terrain supprimé"
                           else
                               display "Suppression annulée"
                           end-if
                           move 1 to Wtrouve
                       end-if
               end-read
           end-perform
           close fterrain.

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
           ALTERNATE RECORD KEY IS fu_mail WITH DUPLICATES
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
           ALTERNATE RECORD KEY IS ft_lieu WITH DUPLICATES
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
             02 fu_role PIC 9(2).
       
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
             02 fr_materiel PIC A.
       
       FD fterrain.
          01 tamp_fterrain.
             02 ft_numterrain PIC 9(9).
             02 ft_lieu PIC A(50).
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
              77 Wancienrole PIC 9(2).
              01 lieu_saisi PIC A(50).
              01 heure_saisie PIC 9(2).
              01 date_saisie PIC A(10).
              01 id_utilisateur PIC 9(10).
               01 W_futilisateur.
                02 Wnumutilisateur PIC 9(9).
                02 Wnom PIC A(30).
                02 Wprenom PIC A(30).
                02 Wmail PIC A(50).
                02 Wmdp PIC 9(20).
                02 Wrole PIC 9(2).
               01 W_flieu.
                02 Wnumlieu PIC 9(9).
                02 Wgerant PIC 9(9).
                02 Wadresse PIC A(50).
                02 Wterrain_existant PIC 9(2).     

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
              
                PERFORM AFFICHAGE_LIEU.
                PERFORM SUPPRIMER_LIEU.
                PERFORM AFFICHAGE_LIEU.

        STOP RUN.
        
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
               DISPLAY "Mail : "
               ACCEPT Wmail
               PERFORM WITH TEST AFTER UNTIL Wmail_valide = 1
                   MOVE 0 TO Wtrouve
                   READ futilisateur
                   AT END
                       MOVE 1 TO Wmail_valide
                   NOT AT END
                       IF fu_mail = Wmail
                           DISPLAY "Mail déjà existant"
                           MOVE 1 TO Wtrouve
                       END-IF
                   END-READ
               END-PERFORM
           END-PERFORM
    
           DISPLAY "Rôle : "
           ACCEPT Wrole
       
           DISPLAY "Mdp : "
           ACCEPT Wmdp
       
           MOVE W_futilisateur to tamp_futilisateur
           WRITE tamp_futilisateur
           END-WRITE
           CLOSE futilisateur.

       MODIF-UTILISATEUR.
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
               DISPLAY "Utilisateur modifié avec succès"
           END-IF
           CLOSE futilisateur.

       MODIF-DROIT.
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
               DISPLAY "Rôle modifié avec succès"
           END-IF
           CLOSE futilisateur.
       
       SUPPRIMER_UTILISATEUR.
           open I-O futilisateur
       display "Suppression d'un utilisateur"
       accept Wnom
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
                       DISPLAY "Mdp : ["fu_mdp "]"
                       DISPLAY "________________"
                    END-READ
            END-PERFORM
            CLOSE futilisateur.

       AJOUT_RESERVATION.
       OPEN I-O freservation

       DISPLAY "Entrez le lieu de la réservation :"
       ACCEPT lieu_saisi FROM CONSOLE.

       DISPLAY "Entrez l'heure de la réservation :"
       ACCEPT heure_saisie FROM CONSOLE.

       DISPLAY "Entrez la date de la réservation :"
       ACCEPT date_saisie FROM CONSOLE.

       DISPLAY "Entrez l'id de l'utilisateur de la réservation :"
       ACCEPT id_utilisateur FROM CONSOLE.

       READ freservation
           INVALID KEY
               MOVE lieu_saisi TO ft_lieu
               MOVE heure_saisie TO fr_heure
               MOVE date_saisie TO fr_date
               MOVE id_utilisateur TO fr_numutilisateur
               MOVE "M" TO fr_materiel

               WRITE tamp_freservation
               IF cr_freservation = "00"
                   DISPLAY "Réservation ajoutée avec succès."
               ELSE
                   DISPLAY "Erreur lors de l'ajout de la réservation."
               END-IF
           NOT INVALID KEY
               DISPLAY "La réservation existe déjà."
       END-READ
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
                       DISPLAY "________________"
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

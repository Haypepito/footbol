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
             02 fu_mdp PIC A(20).
             02 fu_role PIC 9(2).
       
       FD flieu.
          01 tamp_flieu.
             02 fl_numlieu PIC 9(9).
             02 fl_gerant PIC 9(9).
             02 fl_adresse PIC A(50).
             02 fl_terrain_existant PIC 9(9).
       
       FD freservation.
          01 tamp_freservation.
             02 fr_cleres.
                03 fr_numterrain PIC 9(9).
                03 fr_heure PIC 9(2).
                03 fr_date PIC A(10).
             02 fr_numutilisateur PIC A(50).
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

                PERFORM AJOUT_LIEU
                PERFORM MODIF_LIEU
                PERFORM SUPPRIMER_LIEU

        STOP RUN.
        
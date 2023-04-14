IDENTIFICATION DIVISION.
PROGRAM-ID. biathlon.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT fcompetitions assign to "competitions.dat"
        organization sequential
        access mode is sequential
        file status is cr_fcompetitions.
        
        SELECT fathletes assign to "athletes.dat"
        organization indexed
        access mode is dynamic
        file status is cr_fathletes
        record key is fco_numA
        
        SELECT fcourses assign to "courses.dat"
        organization indexed
        access mode is dynamic
        file status is cr_fcourses
        record key is fc_numCo
        
        SELECT fparticipants assign to "participants.dat"
        organization indexed
        access mode is dynamic
        file status is cr_participants
        record key is fp_cle
        alternate record key is fp_numCo WITH DUPLICATES
        alternate record key is fp_numA WITH DUPLICATES.
        

DATA DIVISION.

FILE SECTION.
FD fcompetitions.
        01 Tcompetition.
            02 fco_ville PIC X(30).
            02 fco_pays PIC X(15).
            02 fco_semaine PIC 9(2).
            02 fco_nbj PIC 9.

FD fathletes.
        01 Tathlete.
            02 fa_numA PIC 9(3).
            02 fa_nom PIC X(30).
            02 fa_prenom PIC X(30).
            02 fa_pays PIC X(30).
            02 fa_annee PIC 9(4).
            02 fa_classementP PIC 9(3).
                
FD fcourses.
        01 Tcourses.
            02 fc_numCo PIC 9(2).
            02 fc_villeCompet PIC X(30).
            02 fc_typeCo PIC X(15).
            02 fc_nbpassage PIC 9.
            02 fc_nbkms PIC 9(2).
                
FD fparticipants.
        01 Tparticipant.
            02 fp_cle.
                03 fp_numCo PIC 9(2).
                03 fp_numA PIC 9(3).
            02 fp_classement PIC 9(3).
            02 fp_temps PIC 9(4).
            02 fp_penalites PIC 9(2).
            02 fp_points PIC 9(2).

WORKING-STORAGE SECTION.
        01 new_competition.
            02 fco_ville_new PIC X(30).
            02 fco_pays_new PIC X(15).
            02 fco_semaine_new PIC 9(2).
            02 fco_nbj_new PIC 9.
            01 end_of_file PIC X VALUE "N".
        01 file_status PIC XX.
        01 duplicate_city PIC X VALUE "N".
        01 duplicate_week PIC X VALUE "N".
        
        01 end_of_file_flag PIC X VALUE 'N'.
        01 detail.
            02 filler PIC X(10) VALUE "Ville : ".
            02 fco_ville PIC X(30).
            02 filler PIC X(5) VALUE "Pays : ".
            02 fco_pays PIC X(15).
            02 filler PIC X(7) VALUE "Semaine : ".
            02 fco_semaine PIC 9(2).
            02 filler PIC X(5) VALUE "Jours : ".
            02 fco_nbj PIC 9.
                
        01 recherche_semaine.
            02 sem_recherchee PIC 9(2).
                
        01 ajout_athlete.
            02 fa_numA_new PIC 9(3).
            02 fa_nom_new PIC X(30).
            02 fa_prenom_new PIC X(30).
            02 fa_pays_new PIC X(30).
            02 fa_annee_new PIC 9(4).
            02 fa_classementP_new PIC 9(3).
            
        01 ws_annee_compet PIC 9(4).
        01 ws_fin_lecture PIC X VALUE 'N'.
        
        01 recherche_athlete.
            02 athlete_recherche PIC X VALUE 'N'.
            02 nom_recherche PIC X(30).
            02 prenom_recherche PIC X(30).
            
        01 ws_country_to_find PIC X(20).
        01 ws_found_switch PIC X VALUE 'N'.
        
        01 ajout_course.
            02 typeCo_found PIC X VALUE 'N'.
            01 ws_fc_key PIC X(15).
            01 WS_FC-VARIABLES PIC X(80).
            01 WS_EOF PIC X VALUE 'N'.
            01 ws_answer PIC X.
            02 ws_fc_numCo PIC 9.
            02 ws_fc_villeCompet PIC X(30).
            02 ws_fc_typeCo PIC X(15).
            02 ws_fc_nbpassage PIC 9.
            02 ws_fc_nbkms PIC 9(2).

PROCEDURE DIVISION.

FD ajout_competition.
        DISPLAY "Ajout d'une nouvelle compétition"
        DISPLAY "================================"
        DISPLAY "Ville de la compétition : "
        ACCEPT fco_ville_new
        DISPLAY "Pays de la compétition : "
        ACCEPT fco_pays_new
        DISPLAY "Numéro de semaine de la compétition : "
        ACCEPT fco_semaine_new
        DISPLAY "Nombre de jours de la compétition : "
        ACCEPT fco_nbj_new
        
        PEN EXTEND fcompetitions
        PERFORM UNTIL end_of_file = "Y" OR duplicate_city = "Y" 
        OR duplicate_week = "Y"
        READ fcompetitions
            AT END
                SET end_of_file TO "Y"
            NOT AT END
                IF fco_ville_new = fco_ville
                    SET duplicate_city TO "Y"
                END-IF
                IF fco_semaine_new = fco_semaine
                    SET duplicate_week TO "Y"
                END-IF
        END-READ
        END-PERFORM

        IF duplicate_city = "Y" OR duplicate_week = "Y"
            DISPLAY "Impossible d'ajouter la compétition : "
            DISPLAY "une compétition existe déjà pour cette ville "
            DISPLAY "ou cette semaine."
        ELSE
            MOVE fco_ville_new TO fco_ville
            MOVE fco_pays_new TO fco_pays
            MOVE fco_semaine_new TO fco_semaine
            MOVE fco_nbj_new TO fco_nbj
            WRITE Tcompetition
            
            DISPLAY "La compétition a été ajoutée avec succès."
            
        END-IF
        CLOSE fcompetitions
        STOP RUN.
        
FD affichage_competititon
        DISPLAY "Liste des compétitions"
        DISPLAY "========================"
        OPEN INPUT fcompetitions
        PERFORM UNTIL end_of_file-flag = 'Y'
                READ fcompetitions
                    AT END
                        SET end_of_file-flag TO 'Y'
                    NOT AT END
                        MOVE Tcompetition TO detail
                        DISPLAY detail
                END-READ
        END-PERFORM
        CLOSE fcompetitions
        STOP RUN.
        
FD rechercher_semaine
        DISPLAY "Recherche d'une compétition à une semaine donnée"
        DISPLAY "=================================================="
        DISPLAY "Veuillez entrer la semaine recherchée :"
        ACCEPT sem_recherchee

        OPEN INPUT fcompetitions
        PERFORM UNTIL END-OF-FILE
            READ fcompetitions INTO Tcompetition
            IF fco_semaine = sem_recherchee
                DISPLAY "La compétition organisée dans la semaine "
                DISPLAY sem_recherchee,
                DISPLAY " a lieu à ", fco_ville, " (", fco_pays, ")"
                EXIT PERFORM
            END-IF
        END-PERFORM

        IF NOT END-OF-FILE
            CLOSE fcompetitions
        ELSE
            DISPLAY "La semaine ", sem_recherchee, " ne correspond "
            DISPLAY "à aucune compétition dans le fichier."
        END-IF

STOP RUN.

FD ajout_athlete.
        DISPLAY "Ajout d'un nouvel athlète"
        DISPLAY "==========================="
        DISPLAY "Numéro de l'athlète :"
        ACCEPT fa_numA_new
        DISPLAY "Nom de l'athlète :"
        ACCEPT fa_nom_new
        DISPLAY "Prénom de l'athlète :"
        ACCEPT fa_prenom_new
        DISPLAY "Pays de l'athlète :"
        ACCEPT fa_pays_new
        DISPLAY "Année de naissance de l'athlète :"
        ACCEPT fa_annee_new
        
        MOVE 0 TO cr_fathletes
        READ fathletes NEXT
            AT END
                MOVE fa_numA_new TO Tathlete.fa_numA
                MOVE fa_nom_new TO Tathlete.fa_nom
                MOVE fa_prenom_new TO Tathlete.fa_prenom
                MOVE fa_pays_new TO Tathlete.fa_pays
                MOVE fa_annee_new TO Tathlete.fa_annee
                MOVE 0 TO Tathlete.fa_classementP
                WRITE Tathlete
            NOT AT END
                IF fa_nom_new = Tathlete.fa_nom 
                AND fa_prenom_new = Tathlete.fa_prenom
                    DISPLAY "Un athlète avec ce nom et prénom "
                    DISPLAY "existe déjà"
                ELSE
                    IF fa_numA_new > Tathlete.fa_numA
                        READ fathletes NEXT
                    ELSE
                        IF fa_numA_new = Tathlete.fa_numA
                            DISPLAY "Un athlète avec ce numéro "
                            DISPLAY "exite déja"
                        ELSE
                            MOVE fa_numA_new TO Tathlete.fa_numA
                            MOVE fa_nom_new TO Tathlete.fa_nom
                            MOVE fa_prenom_new TO Tathlete.fa_prenom
                            MOVE fa_pays_new TO Tathlete.fa_pays
                            MOVE fa_annee_new TO Tathlete.fa_annee
                            MOVE 0 TO Tathlete.fa_classementP
                            REWRITE Tathlete
                            DISPLAY "L'athlète a été ajouté"
                        END-IF
                    END-IF
                END-IF
        END-READ

        CLOSE fathletes
        
FD affichage_athlete.
        DISPLAY "Affichage des athlètes en compétition cette année :"
        DISPLAY "===================================================="
        DISPLAY "Sur quelle année souhaitez vous faire un tri ? "
        ACCEPT ws_annee_compet
        MOVE "N" TO ws_fin_lecture
    
        OPEN INPUT fathletes
        READ fathletes NEXT
        AT END
            MOVE "Y" TO ws_fin_lecture
        END-READ

        PERFORM UNTIL ws_fin_lecture = "Y"
            IF fa_annee = ws_annee_compet
                DISPLAY "Numéro d'athlète : " fa_numA
                DISPLAY "Nom : " fa_nom
                DISPLAY "Prénom : " fa_prenom
                DISPLAY "Pays : " fa_pays
                DISPLAY "Année de naissance : " fa_annee
                DISPLAY "Classement Points : " fa_classementP
                DISPLAY "----------------------------------------------"
            END-IF

            READ fathletes NEXT
            AT END
                MOVE "Y" TO ws_fin_lecture
            END-READ
        END-PERFORM

        CLOSE fathletes
        STOP RUN.
        
FD recherche_pays.
        DISPLAY "Recherche d'un athlète"
        DISPLAY "======================"
        DISPLAY "Nom de l'athlète : "
        ACCEPT nom_recherche
        DISPLAY "Prénom de l'athlète : "
        ACCEPT prenom_recherche

        OPEN INPUT fathletes

        SETLL nom_recherche OF fathletes
        READ NEXT fathletes
        PERFORM UNTIL (athlete_recherche = "Y") 
        OR (fathletes NOT = "00" AND fathletes NOT = "10")
            IF fa_nom NOT = nom_recherche
                SETLL nom_recherche OF fathletes
                READ NEXT fathletes
                CONTINUE
            END-IF

            PERFORM UNTIL (athlete_recherche = "Y") 
            OR (fathletes NOT = "00" AND fathletes NOT = "10")
                IF fa_prenom = prenom_recherche
                    DISPLAY "L'athlète ", ws-nom-recherche, " ",
                    DISPLAY ws-prenom-recherche, 
                    DISPLAY "a pour identifiant : ", fa_numA
                    SET athlete_recherche TO "Y"
                END-IF

                READ NEXT fathletes
            END-PERFORM

            READ NEXT fathletes
        END-PERFORM

        IF athlete_recherche = "N"
            DISPLAY "Aucun athlète trouvé"
        END-IF

        CLOSE fathletes
        STOP RUN.
        
FD recherche_pays.
        DISPLAY "Entrez le pays que vous voulez chercher : ".
        ACCEPT ws_country_to_find.
        
        READ fathletes
        PERFORM UNTIL ws_found_switch = 'Y'
            IF fa_pays = ws_country_to_find
                DISPLAY "Athlètes : "
                DISPLAY "============"
                DISPLAY "Prénom : "fa_prenom, " Nom : ", fa_nom,
                DISPLAY "année de naissance : ", fa_annee
                DISPLAY "classement mondial : " fa_classementP
                MOVE "Y" TO ws_found_switch
            END-IF
            READ fathletes
                AT END
                    MOVE "Y" TO ws_found_switch
        END-PERFORM.

        CLOSE fathletes
        STOP RUN.
        
FD ajout_course
        DISPLAY "Ajouter une nouvelle course"
        DISPLAY "============================"
        
        READ fcourses
            DISPLAY "Entrez le numéro de course (2 chiffres) : "
            ACCEPT ws_fc_numCo
            DISPLAY "Entrez la ville de la compétition : "
            ACCEPT ws_fc_villeCompet
            DISPLAY "Entrez le type de course : "
            ACCEPT ws_fc_typeCo
            DISPLAY "Entrez le nombre de passages : "
            ACCEPT ws_fc_nbpassage
            DISPLAY "Entrez le nombre de kilomètres : "
            ACCEPT ws_fc_nbkms
            
            MOVE fc_typeCo TO ws_fc_key
            START Fcourses KEY IS  ws_fc_key
                INVALID KEY
                    MOVE "N" TO typeCo_found
                NOT INVALID KEY
                    MOVE "O" TO typeCo_found
            END-START
            IF typeCo_found = "N" OR ","
                DISPLAY "Le type de course n'existe pas, "
                DISPLAy "voulez-vous l'ajouter ? (O/N)"
                ACCEPT ws_answer
                IF ws_answer = "O" OR "o"
                    MOVE fc_typeCo TO ws_fc_typeCo
                    WRITE FC_RECORD INVALID KEY
                        DISPLAY "Erreur lors de l'ajout de la course : "
                    END-WRITE
                    DISPLAY "Course ajoutée avec succès."
                ELSE
                    DISPLAY "Opération annulée."
                END-IF
            ELSE
                DISPLAY "Le type de course existe déjà."
                MOVE WS_FC-VARIABLES TO WS_TEMP-RECORD
                ADD 1 TO WS_FC-NBPASSAGE
                ADD FC_NBKMS TO WS_FC-NBKMS
                REWRITE FC_RECORD INVALID KEY
                    DISPLAY "Erreur lors de la modification de la course : "
                    DISPLAY WS_FC-STATUS
                    DISPLAY WS_FC-ERROR-MSG
                END-REWRITE

OPEN I-O fcompetitions
IF cr_fcompetitions=35 THEN
        OPEN OUTPUT fcompetition
END-IF
CLOSE fcompetitions

OPEN I-O fparticipants
IF cr_fparticipants=35 THEN
        OPEN OUTPUT fparticipants
END-IF
CLOSE fparticipants

OPEN I-O fathletes
IF cr_fathletes=35 THEN
        OPEN OUTPUT fathletes
END-IF
CLOSE fathletes

OPEN I-O fcourses
IF cr_fcourses=35 THEN
        OPEN OUTPUT fcourses
END-IF
CLOSE fcourses

      SUBROUTINE ULOPEN ( UNIT, FICHIE, NAME, ACCES, AUTOR )
      IMPLICIT   NONE
      INTEGER             UNIT
      CHARACTER*(*)             FICHIE, NAME, ACCES, AUTOR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 11/05/2005   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY  
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY  
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR     
C (AT YOUR OPTION) ANY LATER VERSION.                                   
C                                                                       
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT   
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF            
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU      
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                              
C                                                                       
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE     
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,         
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE D6BHHJP J.P.LEFEBVRE
C TOLE CRP_6
C
C     OUVERTURE DE L'UNITE LOGIQUE ASSOCIE AU FICHIER DE NOM FICHIE DE
C     TYPE ASCII, SI LE NOM EST VIDE, IL EST AFFECTE A fort.UNIT
C     (ENTREES/SORTIES DE TYPE FORMATE)
C
C IN  : UNIT   : NUMERO D'UNITE LOGIQUE
C IN  : FICHIE : NOM DU FICHIER ASSOCIE AU NUMERO D'UNITE LOGIQUE UNIT
C IN  : NAME   : NOM LOCAL ASSOCIE AU NUMERO D'UNITE LOGIQUE UNIT
C IN  : ACCES  : TYPE D'ACCES  N -> NEW, O -> OLD, A -> APPEND 
C IN  : AUTOR  : O -> AUTORISE LA MODIFICATION
C                N -> N'AUTORISE PAS LA MODIFICATION
C                R -> RESERVE L'UNITE SANS OUVRIR LE FICHIER
C     ATTENTION ecriture du NAME en minuscules.
C     ------------------------------------------------------------------
C
      INTEGER          MXF
      PARAMETER       (MXF=100)
      CHARACTER*1      TYPEFI(MXF),ACCEFI(MXF),ETATFI(MXF),MODIFI(MXF)
      CHARACTER*16     DDNAME(MXF)
      CHARACTER*255    NAMEFI(MXF)
      INTEGER          FIRST, UNITFI(MXF) , NBFILE
      COMMON/ ASGFI1 / FIRST, UNITFI         , NBFILE
      COMMON/ ASGFI2 / NAMEFI,DDNAME,TYPEFI,ACCEFI,ETATFI,MODIFI
C
      CHARACTER*255 NAMELL
      CHARACTER*16  NAME16
      CHARACTER*8   K8B
      CHARACTER*4   K4B
      CHARACTER*1   K1ACCE,K1AUT
      INTEGER       I,K,IERR,IER1,IER2,IFILE
      LOGICAL       V11
C     ------------------------------------------------------------------
      INTEGER       MXIMPR
      PARAMETER   ( MXIMPR = 5)
C     ------------------------------------------------------------------
C     CONSERVER LA COHERENCE AVEC IBIMPR
      CHARACTER*16  NOMPR (MXIMPR)
      CHARACTER*1   TYPPR (MXIMPR) , AUTPR(MXIMPR)
      INTEGER       UNITPR (MXIMPR)   , PRESPR(MXIMPR)
      DATA          NOMPR  /'VIGILE'  , 'MESSAGE'   , 'RESULTAT',
     +                      'ERREUR'  ,  'MED'      /
      DATA          UNITPR /    0     ,      6      ,     8     ,
     +                          9     ,    80       /
C     ------------------------------------------------------------------
C
      NAME16 = NAME
      NAMELL = FICHIE
      K1ACCE = ACCES
      K1AUT  = AUTOR
C     
      IF ( UNIT .GT. 0 ) THEN
C
C       VALEUR PAR DEFAUT POUR LES NOMS INTERNES
        IF ( NAME16.EQ.' ' ) THEN
          DO 50 I = 1, MXIMPR
            IF( UNIT .EQ. UNITPR(I) ) THEN
              NAME16 = NOMPR(I)
              GOTO 59
            ENDIF
  50      CONTINUE
  59      CONTINUE
        ENDIF
C
        WRITE(K4B,'(I3)') UNIT
        IF ( FICHIE(1:1) .EQ. ' ' ) THEN
           CALL CODENT ( UNIT, 'G', K8B )
           NAMELL = 'fort.'//K8B
        ELSE
           NAMELL = FICHIE
        ENDIF
C       
        DO 10 I = 1 , NBFILE
          IF ( UNITFI(I) .EQ. UNIT ) THEN
C
C     --- L'UNITE EST DEJA RESERVEE DANS LA SD ---
C
            IF ( NAMEFI(I) .EQ. NAMELL ) THEN
              IF ( TYPEFI(I) .EQ. 'A' ) THEN
                IF ((ETATFI(I).EQ.'O').OR.(ETATFI(I).EQ.'R')) THEN
                  IF ( ACCEFI(I).EQ. K1ACCE ) THEN
                    IF ( DDNAME(I).EQ.NAME16 .OR. NAME16.EQ.' ') THEN
                      GOTO 9999
                    ENDIF
                    CALL UTMESS('E','ULOPEN01','UNITE LOGIQUE '//K4B//
     &               ' ASSOCIEE AU NOM '//DDNAME(I)// 
     &               ' ET AU FICHIER '//NAMEFI(I)(1:80))          
                    CALL UTMESS('F','ULOPEN01','VOUS DEVEZ D''ABORD '
     &                  //'LE FERMER POUR L''ASSOCIER AU NOM '//NAME16)
                  ELSE
                    CALL UTMESS('E','ULOPEN02','UNITE LOGIQUE '//K4B//
     &               ' DEJA UTILISEE EN ACCES '//ACCEFI(I)// 
     &               ' PAR LE FICHIER '//NAMEFI(I)(1:80))         
                    CALL UTMESS('F','ULOPEN02','VOUS DEVEZ D''ABORD '
     &                        //'LE FERMER')        
                  ENDIF
                ENDIF
              ELSE
                CALL UTMESS('E','ULOPEN03','UNITE LOGIQUE '//K4B//
     &               ' DEJA UTILISEE EN MODE BINAIRE PAR LE FICHIER '
     &               //NAMEFI(I)(1:80))           
                CALL UTMESS('F','ULOPEN03','VOUS DEVEZ D''ABORD'
     &                    //' FERMER LE FICHIER ASSOCIE')           
              ENDIF
            ELSE
              CALL UTMESS('F','ULOPEN04','UNITE LOGIQUE '//K4B//
     &                    ' DEJA UTILISEE PAR LE FICHIER '//
     &           NAMEFI(I)(1:80)//' ASSOCIEE AU NOM '//DDNAME(I))
            ENDIF
          ENDIF
 10     CONTINUE
C
C     --- VERIFICATION DE L'OUVERTURE DU FICHIER ---
C
        IF ( NAME16 .NE. ' ' ) THEN
          DO 11 I = 1 , NBFILE
            IF ( DDNAME(I) .EQ. NAME16 )  DDNAME(I) = ' '    
 11       CONTINUE
        ENDIF
        INQUIRE ( UNIT=UNIT, OPENED=V11, IOSTAT=IER1)
        IF ( IER1 .EQ. 0 ) THEN
          IF ( .NOT. V11 ) THEN
            OPEN ( UNIT=UNIT, FILE=NAMELL, IOSTAT=IER2 )

            IF ( IER2 .NE. 0 ) THEN
              CALL UTMESS('F','ULOPEN05','UNITE LOGIQUE '//K4B//
     &               ', PROBLEME LORS DE L''OPEN '//NAMELL(1:80))
            ENDIF     
            CALL ULPOSI ( UNIT, K1ACCE, IERR)
            IF ( IERR .GT. 0 ) THEN
              CALL UTMESS('F','ULOPEN06','UNITE LOGIQUE '//K4B//
     &                    ', PROBLEME LORS DU POSITIONNEMENT') 
            ENDIF
          ENDIF
        ELSE
            CALL UTMESS('F','ULOPEN07','UNITE LOGIQUE '//K4B//
     &                 ', PROBLEME LORS DE L''INQUIRE')             
        ENDIF
C
C     --- ON STOCKE DANS LE COMMON ---
C
        DO 15 I = 1 , NBFILE
          IF ( UNITFI(I) .EQ. 0 ) THEN
            IFILE=I
            GOTO 16
          ENDIF
 15     CONTINUE
        NBFILE = NBFILE + 1
        IF ( NBFILE .GT. MXF ) THEN
          WRITE(K4B,'(I4)') MXF  
          CALL UTMESS('F','ULOPEN08','NOMBRE D''UNITES LOGIQUES'
     &              //' OUVERTES SUPERIEUR A //K4B')
        ENDIF
        IFILE=NBFILE
 16     CONTINUE
        NAMEFI(IFILE) = NAMELL
        DDNAME(IFILE) = NAME16
        UNITFI(IFILE) = UNIT
        TYPEFI(IFILE) = 'A'
        ACCEFI(IFILE) = K1ACCE
        ETATFI(IFILE) = 'O'
        MODIFI(IFILE) = K1AUT
C       POUR UNE RÉSERVATION, ON FERME LE FICHIER, SON ÉTAT PASSE À 'R'
        IF ( K1AUT .EQ. 'R' ) THEN
           MODIFI(IFILE) = 'O'
           ETATFI(IFILE) = 'R'
           CLOSE (UNIT=UNIT, IOSTAT=IERR)
           IF ( IERR .GT. 0 ) THEN
               WRITE(K4B,'(I4)') UNIT
               CALL UTMESS('F','ULOPEN09','UNITE LOGIQUE '//K4B
     &         //', PROBLEME LORS DU CLOSE DE LA RESERVATION.')
           ENDIF
        ENDIF
C       
      ELSE IF ( UNIT .LT. 0 ) THEN
        WRITE(K4B,'(I4)') -UNIT
        DO 20 I = 1 , NBFILE
          IF ( UNITFI(I) .EQ. -UNIT ) THEN
            IF ( MODIFI(I) .EQ. 'O' ) THEN
              IF ( TYPEFI(I) .EQ. 'A' ) THEN
                IF ( ETATFI(I) .EQ. 'O' ) THEN
                  CLOSE (UNIT=-UNIT, IOSTAT=IERR)
                  IF ( IERR .GT. 0 ) THEN
                    CALL UTMESS('F','ULOPEN20','UNITE LOGIQUE '//K4B
     &                    //', PROBLEME LORS DU CLOSE ')         
                  ENDIF
                ENDIF
                NAMEFI(I) = ' '
                DDNAME(I) = ' '
                UNITFI(I) = 0
                TYPEFI(I) = '?'
                ACCEFI(I) = '?'
                ETATFI(I) = 'F'
                MODIFI(I) = ' '
                GOTO 9999
              ELSE
                CALL UTMESS('F','ULOPEN21','LE FICHIER ASSOCIE'
     &                  //'A L''UNITE LOGIQUE '//K4B//' N''EST PAS DE '
     &                  //'TYPE ASCII')
              ENDIF
            ELSE
              CALL UTMESS('F','ULOPEN23','LA REDEFINITION DE L''UNITE '
     &                //'LOGIQUE '//K4B//' N''EST PAS AUTORISEE')
            ENDIF
          ENDIF
20      CONTINUE
        CALL UTMESS('F','ULOPEN12','L''UNITE LOGIQUE '//K4B
     &            //', N''EST PAS TROUVEE, ERREUR LORS DU CLOSE')
      ENDIF
C
 9999 CONTINUE
      END

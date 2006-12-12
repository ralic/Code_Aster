      SUBROUTINE ULOPEN ( UNIT, FICHIE, NAME, ACCES, AUTOR )
      IMPLICIT   NONE
      INTEGER             UNIT
      CHARACTER*(*)             FICHIE, NAME, ACCES, AUTOR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 13/12/2006   AUTEUR PELLET J.PELLET 
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
      CHARACTER*24 VALK(3)
      INTEGER       UNITPR (MXIMPR)   , PRESPR(MXIMPR)
      DATA          NOMPR  /'VIGILE'  , 'MESSAGE'   , 'RESULTAT',
     &                      'ERREUR'  ,  'MED'      /
      DATA          UNITPR /    0     ,      6      ,     8     ,
     &                          9     ,    80       /
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
                     VALK(1) = K4B
                     VALK(2) = DDNAME(I)
                     VALK(3) = NAMEFI(I)(1:80)
                     CALL U2MESK('E','UTILITAI5_11', 3 ,VALK)
                    CALL U2MESK('F','UTILITAI5_12',1,NAME16)
                  ELSE
                     VALK(1) = K4B
                     VALK(2) = ACCEFI(I)
                     VALK(3) = NAMEFI(I)(1:80)
                     CALL U2MESK('E','UTILITAI5_13', 3 ,VALK)
                    CALL U2MESS('F','UTILITAI5_14')
                  ENDIF
                ENDIF
              ELSE
                 VALK(1) = K4B
                 VALK(2) = NAMEFI(I)(1:80)
                 CALL U2MESK('E','UTILITAI5_15', 2 ,VALK)
                CALL U2MESS('F','UTILITAI5_16')
              ENDIF
            ELSE
               VALK(1) = K4B
               VALK(2) = NAMEFI(I)(1:80)
               VALK(3) = DDNAME(I)
               CALL U2MESK('F','UTILITAI5_17', 3 ,VALK)
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
               VALK(1) = K4B
               VALK(2) = NAMELL(1:80)
               CALL U2MESK('F','UTILITAI5_18', 2 ,VALK)
            ENDIF
            CALL ULPOSI ( UNIT, K1ACCE, IERR)
            IF ( IERR .GT. 0 ) THEN
              CALL U2MESK('F','UTILITAI5_19',1,K4B)
            ENDIF
          ENDIF
        ELSE
            CALL U2MESK('F','UTILITAI5_20',1,K4B)
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
          CALL U2MESS('F','UTILITAI5_21')
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
               CALL U2MESK('F','UTILITAI5_22',1,K4B)
           ENDIF
        ENDIF
C
      ELSE IF ( UNIT .LT. 0 ) THEN
        WRITE(K4B,'(I4)') -UNIT
        DO 20 I = 1 , NBFILE
          IF ( UNITFI(I) .EQ. -UNIT ) THEN
            IF ( MODIFI(I) .EQ. 'O' ) THEN
C              IF ( TYPEFI(I) .EQ. 'A' ) THEN
                IF ( ETATFI(I) .EQ. 'O' ) THEN
                  CLOSE (UNIT=-UNIT, IOSTAT=IERR)
                  IF ( IERR .GT. 0 ) THEN
                    CALL U2MESK('F','UTILITAI_77',1,K4B)
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
              CALL U2MESK('F','UTILITAI5_23',1,K4B)
            ENDIF
          ENDIF
20      CONTINUE
      ENDIF
C
 9999 CONTINUE
      END

      SUBROUTINE PRESUP
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF STBTRIAS  DATE 10/12/2001   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C     =================
CA PRESUPER
C  =============================================================
C  !                                                           !
C  !  FONCTION : INTERFACE ENTRE SUPERTAB I-DEAS(4.0) - ASTER  !
C  !                             SUPERTAB I-DEAS(6.0) - ASTER  !
C  !                             SUPERTAB I-DEAS(7.0) - ASTER  !
C  !                                                           !
C  !  DANS CETTE INTERFACE NE SONT RETENUS QUE LES DATASETS    !
C  !  SUIVANTS :                                               !
C  !                                                           !
C  !  DATASET N  151 (SUPERTAB 4,6,7)  ---> TITRE              !
C  !  DATASET N   15 (SUPERTAB 4)      ---> COORDONNEES DES    !
C  !          N  781 (SUPERTAB 6)      --->   NOEUDS           !
C  !          N 2411 (SUPERTAB 7)      --->                    !
C  !  DATASET N   71 (SUPERTAB 4)      ---> DESCRIPTION DES    !
C  !          N  780 (SUPERTAB 6)      --->   ELEMENTS         !
C  !          N 2412 (SUPERTAB 7)      --->                    !
C  !  DATASET N  752 (SUPERTAB 4 & 6)  ---> GROUPES DE NOEUDS  !
C  !          N 2417 (SUPERTAB 7)      --->   OU MAILLES       !
C  !          N 2429 (MASTER SERIES 3) --->   OU MAILLES       !
C  !  DATASET N  735 (SUPERTAB 4 & 6)  ---> DESCRIPTION        !
C  !              ??                        GEOMETRIQUE        !
C  =============================================================
C  !                                                           !
C  !  ROUTINES APPELEES :                                      !
C  !                          : IUNIFI (FONCTION)              !
C  !                          : INISTB                         !
C  !                          : SLETIT                         !
C  !                          : SLENEU                         !
C  !                          : ECRNEU                         !
C  !                          : SLEELT                         !
C  !                          : ECRELT                         !
C  !                          : SLEGRO                         !
C  !                          : SLEGEO                         !
C  !                                                           !
C  =============================================================
C
C --> DECLARATIONS DES VARIABLES
C
      PARAMETER (MXTYMA=99,MAXNOD=32,MXPERM=MAXNOD*MXTYMA)
      PARAMETER (MAXFA=6,MXPERF=MAXFA*MXTYMA)
      CHARACTER*6 CHAR,MOINS1
      CHARACTER*8 NOMAIL(MXTYMA),RQUOI
      INTEGER LIMAIL(MXTYMA),NBMTOT
      INTEGER DATSET,NBNODE,NBMAIL(MXTYMA),INDIC(MXTYMA),PERMUT(MXPERM)
      INTEGER INDICF(MXTYMA),PERMUF(MXPERF)
      INTEGER MINT(MXTYMA),MANT(MXTYMA)
      REAL*8 AMA,AMI,BMA,BMI,CMA,CMI
      INTEGER MIN,MAN
      INTEGER IITEST
      LOGICAL LARRET
C
C --> INITIALISATIONS
C
      MOINS1 = '    -1'
      RQUOI = '????????'
      LARRET=.TRUE.
C
C  -->N  D'UNITE LOGIQUE DES FICHIERS
C
      IMES = IUNIFI('MESSAGE')
      IUNV = IUNIFI('UNIVERSEL')
C
      DO 1234 I = 1,MXTYMA
         NBMAIL(I) = 0
         NOMAIL(I) = RQUOI
 1234 CONTINUE
C
      CALL INISTB(MAXNOD,NBTYMA,NOMAIL,INDIC,PERMUT,LIMAIL,
     &            INDICF,PERMUF,MAXFA)
C
C     RECHERCHE DU PREMIER '    -1'
C
 1000 CONTINUE
      READ (IUNV,'(A)',END=9999) CHAR
      IF (CHAR.NE.MOINS1) GO TO 1000
C
    1 CONTINUE
      READ (IUNV,'(I6)',END=9999) DATSET
      IF (DATSET.EQ.-1) THEN
C
C  -->   FIN DE DATASET
C
      ELSE IF (DATSET.EQ.151) THEN
C
C  -->   LECTURE ET ECRITURE DU  TITRE
         CALL SLETIT
         LARRET=.FALSE.
C
      ELSE IF (DATSET.EQ.15.OR.DATSET.EQ.781
     &         .OR.DATSET.EQ.2411) THEN
C
C  -->   LECTURE ET ECRITURE DES  NOEUDS
         CALL SLENEU(NBNODE,AMA,BMA,CMA,AMI,BMI,CMI,MIN,MAN,ITES,
     &               DATSET)
         LARRET=.FALSE.
         CALL ECRNEU(NBNODE,AMA,BMA,CMA,AMI,BMI,CMI,MIN,MAN,ITES)
C
      ELSE IF (DATSET.EQ.18) THEN
C
C  -->   LECTURE ET ECRITURE DU(DES) SYSTEME(S) DE COORDONNEES
C
C  ----  L'OPTION ACTUELLE EST DE NE PAS TRAITER LES SYSTEMES DE
C         COORDONNEES UTILISATEURS : MESSAGE D'ERREUR
C
         CALL UTMESS('F','PRESUP',' ATTENTION LES SYSTEMES DE'
     &    //' COORDONNES SUPERTAB NE SONT PAS RELUS DANS ASTER.')
         GOTO 99999
      ELSE IF (DATSET.EQ.2420) THEN
         CALL SLECOR
C
      ELSE IF (DATSET.EQ.71.OR.DATSET.EQ.780
     &         .OR.DATSET.EQ.2412)   THEN
C
C  -->   LECTURE ET ECRITURE DES  MAILLES
         CALL SLEELT(MAXNOD,NBTYMA,INDIC,PERMUT,NBMAIL,MINT,MANT,
     &               DATSET,NBMTOT)
         LARRET=.FALSE.
         CALL ECRELT(MAXNOD,NBTYMA,NOMAIL,NBMAIL,MINT,MANT,
     &               LIMAIL,NBMTOT)
         CALL SLECOL(NBMTOT)
C
      ELSE IF (DATSET.EQ.752.OR.DATSET.EQ.2417.
     &          OR.DATSET.EQ.2429.OR.DATSET.EQ.2430.
     &          OR.DATSET.EQ.2432.OR.DATSET.EQ.2435 ) THEN
C
C  -->   LECTURE ET ECRITURE DES GROUPES DE NOEUDS OU D'MAILLES
         CALL SLEGRO(DATSET)
         LARRET=.FALSE.
C
      ELSE IF (DATSET.EQ.735) THEN
C
C  -->   LECTURE ET ECRITURE DES NOEUDS ET MAILLES RATTACHES AUX
C        CURVES,MESHS AREA ET MESHS VOLUME
         CALL SLEGEO
C
      ELSE
C
C  -->   LECTURE D'UNE RUBRIQUE INCONNUE
         WRITE (IMES,*) 'ON NE TRAITE PAS LE DATASET:',DATSET
    2    CONTINUE
         READ (IUNV,'(A)',END=9999) CHAR
         IF (CHAR.NE.MOINS1) GO TO 2
      END IF
      GO TO 1
 9999 CONTINUE
      IF (LARRET) THEN
         CALL UTMESS('F','PRESUP','LE FICHIER IDEAS EST VIDE, OU'
     +      //' NE CONTIENT PAS DE DATATSET TRAITE PAR L''INTERFACE')
      ENDIF
C
      CALL JEDETR('&&PRESUP.INFO.NOEUDS')
      CALL JEDETR('&&PRESUP.COOR.NOEUDS')
      CALL JEDETR('&&PRESUP.INFO.MAILLE')
      CALL JEDETR('&&PRESUP.CONN.MAILLE')
      CALL JEDETR('&&IDEAS.SYST')
99999 CONTINUE
      END

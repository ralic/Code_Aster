      SUBROUTINE PRESUP ( LGRCOU )
      IMPLICIT NONE
      LOGICAL             LGRCOU
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF STBTRIAS  DATE 23/01/2006   AUTEUR NICOLAS O.NICOLAS 
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
C  !  DATASET N  18  (SUPERTAB 4)      ---> SYS DE COORD       !
C  !          N  2420 (SUPERTAB 5-10)  --->                    !
C  !  DATASET N   15 (SUPERTAB 4)      ---> COORDONNEES DES    !
C  !          N  781 (SUPERTAB 6)      --->   NOEUDS           !
C  !          N 2411 (SUPERTAB 7-10)   --->                    !
C  !  DATASET N   71 (SUPERTAB 4)      ---> DESCRIPTION DES    !
C  !          N  780 (SUPERTAB 6)      --->   ELEMENTS         !
C  !          N 2412 (SUPERTAB 7-10)   --->                    !
C  !  DATASET N  752 (SUPERTAB 4 & 6)  ---> GROUPES DE NOEUDS  !
C  !          N 2417 (SUPERTAB 7)      --->   OU MAILLES       !
C  !          N 2428 (MASTER SERIES 3) --->                    !
C  !          N 2429 (MASTER SERIES 3) --->                    !
C  !          N 2430 (MASTER SERIES 3) --->                    !
C  !          N 2432 (MASTER SERIES 3) --->                    !
C  !          N 2435 (SUPERTAB 7)      --->                    !
C  !          N 2452 (MASTER SERIES 3) --->                    !
C  !          N 2467 (MASTER SERIES 3) --->                    !
C  !          N 2477 (MASTER SERIES v11) --->                  !
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
      LOGICAL LARRET
      INTEGER MIN,MAN,IUNIFI,IMOD,IMES,IUNV,MXTYMA,MXPERM,MXPERF
      INTEGER MAXFA,I,NBTYMA,ITES,MAXNOD
      PARAMETER (MXTYMA=99,MAXNOD=32,MXPERM=MAXNOD*MXTYMA)
      PARAMETER (MAXFA=6,MXPERF=MAXFA*MXTYMA)
      INTEGER LIMAIL(MXTYMA),NBMTOT
      INTEGER DATSET,NBNODE,NBMAIL(MXTYMA),INDIC(MXTYMA),PERMUT(MXPERM)
      INTEGER INDICF(MXTYMA),PERMUF(MXPERF)
      INTEGER MINT(MXTYMA),MANT(MXTYMA)
      CHARACTER*4 CT(3)
      CHARACTER*6 CHAR,MOINS1
      CHARACTER*8 NOMAIL(MXTYMA),RQUOI
      CHARACTER*12 AUT
      REAL*8 AMA,AMI,BMA,BMI,CMA,CMI
C
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
      IUNV = IUNIFI('IDEAS')
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

C   QUOIQU'IL ARRIVE, ON ECRIT DANS LE TITRE QUE LE MAILLAGE
C   A ETE LU AU FORMAT IDEAS :
C   ---------------------------------------------------------------
      IMOD = IUNIFI('FICHIER-MODELE')
      WRITE (IMOD,'(A,4X,A)')'TITRE','NOM=INDEFINI'
      CALL JJMMAA(CT,AUT)
      WRITE(IMOD,'(9X,A,17X,A,A2,A,A2,A,A4)')'AUTEUR=INTERFACE_IDEAS'
     &    ,'DATE=', CT(1)(1:2),'/',CT(2)(1:2),'/',CT(3)
      WRITE (IMOD,'(A)') 'FINSF'
      WRITE (IMOD,'(A)') '%'


    1 CONTINUE
    
      READ (IUNV,'(I6)',END=9999) DATSET
      
      IF (DATSET.EQ.-1) THEN
C  -->   FIN DE DATASET
  
      ELSE IF ((DATSET.EQ.18).OR.(DATSET.EQ.2420)) THEN
C
C  -->   LECTURE ET ECRITURE DU(DES) SYSTEME(S) DE COORDONNEES
C
         CALL SLECOR(DATSET)
  
      ELSE IF ((DATSET.EQ.15).OR.(DATSET.EQ.781)
     &         .OR.(DATSET.EQ.2411)) THEN
C
C  -->   LECTURE ET ECRITURE DES  NOEUDS
         CALL SLENEU(NBNODE,AMA,BMA,CMA,AMI,BMI,CMI,MIN,MAN,ITES,
     &               DATSET)
         LARRET=.FALSE.
         CALL ECRNEU(NBNODE,AMA,BMA,CMA,AMI,BMI,CMI,MIN,MAN,ITES)
         IF ( LGRCOU )  CALL SNECOL(NBNODE)
C
C
      ELSE IF ((DATSET.EQ.71).OR.(DATSET.EQ.780)
     &         .OR.(DATSET.EQ.2412).OR.
     &        (DATSET.EQ.2431).OR.(DATSET.EQ.82))   THEN
C
C  -->   LECTURE ET ECRITURE DES  MAILLES
         CALL SLEELT(MAXNOD,NBTYMA,INDIC,PERMUT,NBMAIL,MINT,MANT,
     &               DATSET,NBMTOT)
         LARRET=.FALSE.
         CALL ECRELT(MAXNOD,NBTYMA,NOMAIL,NBMAIL,MINT,MANT,
     &               LIMAIL,NBMTOT)
         IF ( LGRCOU )  CALL SLECOL(NBMTOT)
C
      ELSE IF (DATSET.EQ.752.OR.DATSET.EQ.2417.
     &          OR.DATSET.EQ.2428.OR.DATSET.EQ.2429.
     &          OR.DATSET.EQ.2430.OR.DATSET.EQ.2432.
     &          OR.DATSET.EQ.2435.OR.DATSET.EQ.2452.
     &          OR.DATSET.EQ.2467.OR.DATSET.EQ.2477) THEN
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

      SUBROUTINE ARLCH0(DIME  ,IAN   ,IAC   ,NNC   ,ILGN  ,
     &                  MAXMT    ,PREC  ,EQ    ,VLMT     ,NTRG)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/10/2009   AUTEUR CAO B.CAO 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C
C ======================================================================
C RESPONSABLE MEUNIER S.MEUNIER
C
      IMPLICIT NONE
      INTEGER  DIME
      LOGICAL  IAN,IAC
      INTEGER  NNC
      REAL*8   VLMT(*)
      INTEGER  ILGN(*)
      REAL*8   MAXMT(2,*)
      REAL*8   PREC
      LOGICAL  EQ(5,*)
      INTEGER  NTRG
C
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C COMPTE NOMBRE DE TERMES NON-NEGLIGEABLES ET ADIMENSIONNEMENT
C             DES RELATIONS LINEAIRES DE COUPLAGE ARLEQUIN
C
C ----------------------------------------------------------------------
C
C
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  IAN    : .TRUE. SI MODELE ZONE MECANIQUE EST DE TYPE COQUE
C IN  IAC    : .TRUE. SI MODELE ZONE COLLAGE EST DE TYPE COQUE
C IN  NNC    : NOMBRE DE NOEUDS MAILLES DE COLLAGE
C IN  ILGN   : LONGUEUR CUMULEE COLLECTION NOEUDS COLONNES DE B
C IN  MAXMT  : MAXIMA EN VALEUR ABSOLUE SUIVANT LIGNES DE B
C                       POUR TRANSLATION ET ROTATION (CF ARLMAX)
C IN  PREC   : PRECISION RELATIVE SUR LES TERMES DE B
C IN  EQ     : EQUATIONS DE COUPLAGE SELECTIONNEES (CF ARLCLR)
C I/O VLMT   : VALEURS DE LA MATRICE ARLEQUIN MORSE (CF ARLCAL)O
C I/O NTRG   : NOMBRE DE TERMES NON-NEGLIGEABLES DANS LES
C                       RELATIONS LINEAIRES DE COUPLAGE ARLEQUIN
C
C ----------------------------------------------------------------------
C
      INTEGER  PMAT0,PMAT1,PMAT2,IAUX,JAUX,KAUX,LAUX,VLTERR,OF
      REAL*8   VLR
C
C ----------------------------------------------------------------------
C

C --- PARCOURS DES LIGNES

      VLTERR = 2*DIME - 3
      OF = DIME - 1
      PMAT1 = ILGN(1)
      PMAT2 = 0

      DO 10 IAUX = 1, NNC

        PMAT0 = PMAT1
        PMAT1 = ILGN(IAUX+1)

        DO 20 JAUX = 1, PMAT1-PMAT0

C ------- COUPLAGE TRANSLATION / TRANSLATION

          DO 30 KAUX = 1, DIME
            IF (.NOT.EQ(KAUX,IAUX)) THEN
              PMAT2 = PMAT2 + DIME
            ELSE
              DO 40 LAUX = 1, DIME
                PMAT2 = PMAT2 + 1
                VLR = VLMT(PMAT2)/MAXMT(1,IAUX)
                VLMT(PMAT2) = VLR
                IF (ABS(VLR).GT.PREC) NTRG = NTRG + 1
 40           CONTINUE
            ENDIF
 30       CONTINUE

C ------- COUPLAGE ROTATION / TRANSLATION

          IF (IAC) THEN
            DO 50 KAUX = 2, DIME
              IF (.NOT.EQ(OF+KAUX,IAUX)) THEN
                PMAT2 = PMAT2 + DIME
              ELSE
                DO 60 LAUX = 1, DIME
                  PMAT2 = PMAT2 + 1
                  VLR = VLMT(PMAT2)/MAXMT(2,IAUX)
                  VLMT(PMAT2) = VLR
                  IF (ABS(VLR).GT.PREC) NTRG = NTRG + 1
 60             CONTINUE
              ENDIF
 50         CONTINUE
          ENDIF

C ------- COUPLAGE TRANSLATION / ROTATION

          IF (IAN) THEN

            DO 70 KAUX = 1, DIME
              IF (.NOT.EQ(KAUX,IAUX)) THEN
                PMAT2 = PMAT2 + VLTERR
              ELSE
                DO 80 LAUX = 1, VLTERR
                  PMAT2 = PMAT2 + 1
                  VLR = VLMT(PMAT2)/MAXMT(1,IAUX)
                  VLMT(PMAT2) = VLR
                  IF (ABS(VLR).GT.PREC) NTRG = NTRG + 1
 80             CONTINUE
              ENDIF
 70         CONTINUE

C --------- COUPLAGE ROTATION / ROTATION

            IF (IAC) THEN
              DO 90 KAUX = 2, DIME
                IF (.NOT.EQ(OF+KAUX,IAUX)) THEN
                  PMAT2 = PMAT2 + VLTERR
                ELSE
                  DO 100 LAUX = 1, VLTERR
                    PMAT2 = PMAT2 + 1
                    VLR = VLMT(PMAT2)/MAXMT(2,IAUX)
                    VLMT(PMAT2) = VLR
                    IF (ABS(VLR).GT.PREC) NTRG = NTRG + 1
 100              CONTINUE
                ENDIF
 90           CONTINUE
            ENDIF

          ENDIF

 20     CONTINUE

 10   CONTINUE

      END

      SUBROUTINE ARLCH2(DIME,NNDC  ,SIGN,NELEM  ,VLMT   ,
     &                  INO ,INC ,PREC,EQ  ,NTERM  ,
     &                  NEQ  ,NLAGR ,IMPO,MULT,LIEL,
     &                  NEMA)
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
      INTEGER DIME
      INTEGER SIGN
      INTEGER NTERM
      INTEGER NEQ
      INTEGER NLAGR
      INTEGER NNDC
      INTEGER NELEM(*)
      INTEGER INO(*)
      INTEGER INC(*)
      REAL*8  VLMT(*)
      REAL*8  PREC
      LOGICAL EQ(5,*)
      INTEGER LIEL(*)
      INTEGER NEMA(*)
      REAL*8  IMPO(*)
      REAL*8  MULT(*)
C
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C ECRITURE RELATIONS LINEAIRES COUPLAGE
C MAILLE SOLIDE / COQUE
C
C ----------------------------------------------------------------------
C
C
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  NNDC     : NOMBRE DE NOEUDS DOMAINE DE COLLAGE
C IN  SIGN   : SIGNE DEVANT LA MATRICE B (0 : +, 1 : -)
C IN  NELEM     : NUMERO DES ELEMENTS DE LIAISON 'D_DEPL_R_*'
C                       * = (DX, DY, DZ, DRX, DRY, DRZ)
C IN  VLMT      : VALEURS DE LA MATRICE ARLEQUIN MORSE (CF ARLCPL)
C IN  INO    : COLLECTION NOEUDS COLONNES DE VLMT
C IN  INC    : LONGUEUR CUMULEE ASSOCIEE A INO
C IN  PREC   : PRECISION RELATIVE SUR LES TERMES DE VLMT
C IN  EQ     : EQUATIONS SELECTIONNEES
C I/O NTERM     : NOMBRE DE TERMES
C I/O NEQ     : NOMBRE D'EQUATIONS
C I/O NLAGR    : NOMBRE DE LAGRANGES
C I/O IMPO   : VECTEUR CHME.CIMPO.VALE
C I/O MULT   : VECTEUR CHME.CMULT.VALE
C I/O LIEL   : COLLECTION CHME.LIGRE.LIEL
C I/O NEMA   : COLLECTION CHME.LIGRE.NEMA
C
C ----------------------------------------------------------------------
C
      INTEGER IAUX,JAUX,KAUX,LAUX,PMAT0,PMAT1,LGNM,NMTE,NBEVO
      REAL*8  VLR
C
C ----------------------------------------------------------------------
C
      LGNM  = 0
      PMAT1 = INC(1)
      DO 10 IAUX = 1, NNDC
        PMAT0 = PMAT1
        PMAT1 = INC(1+IAUX)
        DO 20 JAUX = PMAT0, PMAT1-1
          NBEVO = INO(JAUX)
C ------- RELATIONS COUPLAGE TRANSLATION / TRANSLATION
          NMTE = NLAGR
          DO 30 KAUX = 1, DIME
            IF (.NOT.EQ(KAUX,IAUX)) THEN
              LGNM = LGNM + DIME
            ELSE
              DO 40 LAUX = 1, DIME
                LGNM = LGNM + 1
                VLR = VLMT(LGNM)
                IF (ABS(VLR).GT.PREC) THEN
                  CALL ARLASS(SIGN,NBEVO  ,NELEM  ,LAUX   ,VLR   ,
     &                        NTERM  ,NEQ  ,NMTE  ,IMPO,MULT,
     &                        LIEL,NEMA)
                ENDIF
 40           CONTINUE
              NMTE = NMTE + 2
            ENDIF
 30       CONTINUE
          NMTE = NLAGR
C ------- RELATIONS COUPLAGE TRANSLATION / ROTATION 2D
          IF (DIME.EQ.2) THEN
            DO 50 KAUX = 1, 2
              LGNM = LGNM + 1
              IF (.NOT.EQ(KAUX,IAUX)) GOTO 50
              VLR = VLMT(LGNM)
              IF (ABS(VLR).GT.PREC) THEN
                CALL ARLASS(SIGN,NBEVO  ,NELEM  ,6   ,VLR   ,
     &                      NTERM  ,NEQ  ,NMTE  ,IMPO,MULT,
     &                      LIEL,NEMA)
              ENDIF
              NMTE = NMTE + 2
 50         CONTINUE
C ------- RELATIONS COUPLAGE TRANSLATION / ROTATION 3D
          ELSE
            DO 60 KAUX = 1, 3
              IF (.NOT.EQ(KAUX,IAUX)) THEN
                LGNM = LGNM + 3
              ELSE
                DO 70 LAUX = 1, 3
                  LGNM = LGNM + 1
                  VLR = VLMT(LGNM)
                  IF (ABS(VLR).GT.PREC) THEN
                    CALL ARLASS(SIGN,NBEVO  ,NELEM  ,3+LAUX ,VLR   ,
     &                          NTERM  ,NEQ  ,NMTE  ,IMPO,MULT,
     &                          LIEL,NEMA)
                  ENDIF
 70             CONTINUE
                NMTE = NMTE + 2
              ENDIF
 60         CONTINUE
          ENDIF
 20     CONTINUE
        NLAGR = NMTE
 10   CONTINUE

      END

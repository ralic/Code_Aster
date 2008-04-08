      SUBROUTINE BOITEL(CNOEUD,TYPEMA,NOEPAN,NPAN  ,NSOM  ,
     &                  DIME  ,MINMAX,PAN)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/04/2008   AUTEUR MEUNIER S.MEUNIER 
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
      CHARACTER*8  TYPEMA
      INTEGER      NOEPAN(*),NPAN,DIME,NSOM
      REAL*8       CNOEUD(DIME,*),MINMAX(2,*),PAN(DIME+2,*)
C
C ----------------------------------------------------------------------
C
C CONSTRUCTION DE BOITES ENGLOBANTES POUR UN GROUPE DE MAILLES
C
C BOITE ENGLOBANTE POUR MAILLE LINEAIRE
C
C ----------------------------------------------------------------------
C
C
C IN  TYPEMA : TYPE DE MAILLE
C IN  NOEPAN : NOEUDS DEFINISSANT LES PANS
C                   ( NOMBRE NOEUDS PAN 1, N1, N2, ...
C                     ..., NOMBRE NOEUDS PAN 2, ...)
C                     EN 3D, NB NOEUDS < 0 : TRIANGLE
C                            NB NOEUDS > 0 : QUADRANGLE
C IN  CNOEUD : COORD. NOEUDS DE LA MAILLE
C               DIM: (DIME,*)   (X1, [Y1, Z1], X2, ...)
C IN  NSOM   : NOMBRE DE SOMMETS DE LA MAILLE
C IN  NPAN   : NOMBRE DE PANS DE LA MAILLE
C IN  DIME   : DIMENSION DE L'ESPACE
C OUT MINMAX : BOITE ENGLOBANT LA MAILLE SUIVANT X,Y,[Z]
C               DIM: (2,DIME)
C OUT PAN    : EQUATION DES PANS DU CONVEXE ENGLOBANT
C              ET INSCRIT DE LA MAILLE (CF BOITE)
C               DIM: (DIME+2,*)
C
C ---------------------------------------------------------------------
C
      INTEGER   K1,K2
      INTEGER   NBPAN
      INTEGER   JPAN
      INTEGER   IPAN,IDIME,IND
      REAL*8    X1(3),X2(3),X3(3),R1,R2,R3,R,S1,S2
C
C ----------------------------------------------------------------------
C
C
C --- RE-ORIENTATION DE LA MAILLE SI NECESSAIRE
C
      CALL ORIEM2(TYPEMA,CNOEUD)
C
C --- BOITE MINMAX
C
      DO 10 IDIME = 1, DIME
        R = CNOEUD(IDIME,1)
        MINMAX(1,IDIME) = R
        MINMAX(2,IDIME) = R
10    CONTINUE
C
      DO 20 IND = 2, NSOM
        DO 21 IDIME= 1, DIME
          R = CNOEUD(IDIME,IND)
          IF (R.LT.MINMAX(1,IDIME)) THEN
            MINMAX(1,IDIME) = R
          ELSEIF (R.GT.MINMAX(2,IDIME)) THEN
            MINMAX(2,IDIME) = R
          ENDIF
 21     CONTINUE
 20   CONTINUE
C
C --- CALCUL DES PANS
C
      JPAN = 0
      IF (DIME.EQ.2) THEN
        DO 30 IPAN = 1, NPAN
          JPAN  = JPAN + 1
          NBPAN = NOEPAN(JPAN)
          K1    = NOEPAN(JPAN+1)
          K2    = NOEPAN(JPAN+2)
          DO 40 IDIME = 1, DIME
            X1(IDIME) = CNOEUD(IDIME,K1)
            X2(IDIME) = CNOEUD(IDIME,K2)
 40       CONTINUE
          JPAN  = JPAN + NBPAN
          R1    = X2(2) - X1(2)
          R2    = X1(1) - X2(1)
          S1    = X1(1)*X2(2) - X1(2)*X2(1)
          S2    = S1
          IF ((NSOM.EQ.4).AND.(TYPEMA(5:5).NE.'4')) THEN
            DO 50 IND = 1, NSOM
              R = R1*CNOEUD(1,IND) + R2*CNOEUD(2,IND)
              IF (R.GT.S1) THEN
                S1 = R
              ENDIF
 50         CONTINUE
          ENDIF
          PAN(1,IPAN) = R1
          PAN(2,IPAN) = R2
          PAN(3,IPAN) = -S1
          PAN(4,IPAN) = -S2
 30     CONTINUE
      ELSEIF (DIME.EQ.3) THEN
        DO 60 IPAN = 1, NPAN
          JPAN  = JPAN + 1
          NBPAN = NOEPAN(JPAN)
          IF (NBPAN.LT.0) THEN
C
C --- FACE TRIANGLE
C
            K1 = NOEPAN(JPAN+1)
            DO 70 IDIME = 1, DIME
              X1(IDIME) = CNOEUD(IDIME,K1)
 70         CONTINUE
            K1 = NOEPAN(JPAN+2)
            K2 = NOEPAN(JPAN+3)
            DO 80 IDIME = 1, DIME
              X2(IDIME) = CNOEUD(IDIME,K1) - X1(IDIME)
              X3(IDIME) = CNOEUD(IDIME,K2) - X1(IDIME)
 80         CONTINUE
          ELSE
C
C --- FACE QUADRANGLE
C
            K1 = NOEPAN(JPAN+1)
            K2 = NOEPAN(JPAN+3)
            DO 90 IDIME = 1, DIME
              X2(IDIME) = CNOEUD(IDIME,K2) - CNOEUD(IDIME,K1)
 90         CONTINUE
            K1 = NOEPAN(JPAN+2)
            K2 = NOEPAN(JPAN+4)
            DO 100 IDIME = 1, DIME
              X3(IDIME) = CNOEUD(IDIME,K2) - CNOEUD(IDIME,K1)
 100        CONTINUE
          ENDIF
          R1 = X2(2)*X3(3)-X2(3)*X3(2)
          R2 = X2(3)*X3(1)-X2(1)*X3(3)
          R3 = X2(1)*X3(2)-X2(2)*X3(1)
          IF ((NBPAN.EQ.-3).OR.(NSOM.EQ.4)) THEN
            S1 = R1*X1(1) + R2*X1(2) + R3*X1(3)
            S2 = S1
          ELSE
            S1 = R1*CNOEUD(1,1) + R2*CNOEUD(2,1) + R3*CNOEUD(3,1)
            DO 110 IND = 2, NSOM
              R = R1*CNOEUD(1,IND) + R2*CNOEUD(2,IND) + R3*CNOEUD(3,IND)
              IF (R.GT.S1) THEN
                S1 = R
              ENDIF
 110        CONTINUE
            K1 = NOEPAN(JPAN+1)
            K2 = NOEPAN(JPAN+2)
            S2 = MIN(R1*CNOEUD(1,K1)+R2*CNOEUD(2,K1)+R3*CNOEUD(3,K1),
     &               R1*CNOEUD(1,K2)+R2*CNOEUD(2,K2)+R3*CNOEUD(3,K2) )
          ENDIF
          PAN(1,IPAN) = R1
          PAN(2,IPAN) = R2
          PAN(3,IPAN) = R3
          PAN(4,IPAN) = -S1
          PAN(5,IPAN) = -S2
          JPAN = JPAN + ABS(NBPAN)
60      CONTINUE
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C

      END

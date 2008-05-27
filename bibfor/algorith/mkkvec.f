      SUBROUTINE MKKVEC(RESE,NRESE,NDIM,VEC,RESU)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/05/2008   AUTEUR DESOZA T.DESOZA 
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
      IMPLICIT NONE
      REAL*8  RESE(3),NRESE,RESU(3),VEC(3)
      INTEGER NDIM
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : MMMAB2
C ----------------------------------------------------------------------
C
C SUBROUTINE QUI CALCULE RESU = K(LAMBDA +RHO[[U]]_TAU)*VEC
C
C IN  RESE   : LAMBDA +RHO[[U]]_TAU
C IN  NRESE  : RACINE DE LA NORME DE RESE
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  VEC    : LE VECTEUR A MULTIPLIER
C OUT RESU   : LE RESULTAT  [K]*VEC
C                      K(x) = (Id-x*xt/!!x!!**)1/!!x!!
C
C ----------------------------------------------------------------------
C
      REAL*8  MAT(3,3)
      REAL*8  NORME,THETA
      INTEGER I,J
C
C ----------------------------------------------------------------------
C
C --- INITIALISATIONS
C
      DO 11 I = 1,3
        DO 12 J = 1,3
          MAT(I,J) = 0.D0
12      CONTINUE
        RESU(I) = 0.D0
11    CONTINUE
      THETA = 1.D0
C
C --- CALCUL DE LA NORME DE LAMBDA +RHO[[U]]_TAU
C
      NORME = NRESE*NRESE
      IF ((NORME.EQ.0.D0).OR.(NRESE.EQ.0.D0)) THEN
         CALL U2MESS('F','ALGORITH5_82')
      ENDIF
C
C --- CALCUL DU PRODUIT IK()VEC
C
      IF (NDIM.EQ.2) THEN
        DO 13 I = 1,2
          DO 14 J = 1,2
            MAT(I,J) = -THETA*RESE(I)*RESE(J)/NORME
14        CONTINUE
13      CONTINUE
        DO 15 J = 1,2
          MAT(J,J) = 1.D0+MAT(J,J)
15      CONTINUE
        DO 16 I = 1,2
          DO 17 J = 1,2
            MAT(I,J) = MAT(I,J)/NRESE
17        CONTINUE
16      CONTINUE
        DO 18 I=1,2
          DO 19 J=1,2
            RESU(I) = MAT(I,J)*VEC(J)+RESU(I)
            RESU(3) = 0.D0
19        CONTINUE
18      CONTINUE
      ELSE IF (NDIM.EQ.3) THEN
        DO 20 I = 1,3
          DO 21 J = 1,3
            MAT(I,J) = -THETA*RESE(I)*RESE(J)/NORME
21        CONTINUE
20      CONTINUE
        DO 22 J = 1,3
          MAT(J,J) = 1.D0+MAT(J,J)
22      CONTINUE
        DO 23 I = 1,3
          DO 24 J = 1,3
            MAT(I,J) = MAT(I,J)/NRESE
24        CONTINUE
23      CONTINUE
        DO 25 I=1,3
          DO 26  J=1,3
            RESU(I) = MAT(I,J)*VEC(J)+RESU(I)
26        CONTINUE
25      CONTINUE
      END IF

      END

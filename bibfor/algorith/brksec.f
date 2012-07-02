      SUBROUTINE BRKSEC(H66,BT3,BC,NU,E,S3)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

C      H66 MATRICE SECANTE
C      B  INDICE DE FISSURATION  EN BASE PRINCIPALE D ENDO
C      BC INDICE DE FISSURATION COMP
C      E,NU ELAST MATR SAIN

C     CALCUL DE LA MATRICE SECANTE ORTHOTROPE EN
C       BASE PRINCIPALE D ENDOMMAGEMENT
      IMPLICIT NONE
      REAL*8 H66(6,6),B(3),S3(3),NU,E,BT3(3)
      INTEGER I,J
      REAL*8 BC ,T1 ,T17 ,T2 ,T21 ,T24 ,T26
      REAL*8 T31 ,T5 ,T7 ,T8
C-----------------------------------------------------------------------

C     MISE  AZERO
      DO 10 I=1,6
       DO 20 J=1,6
        H66(I,J)=0.D0
20     CONTINUE
10    CONTINUE


C     PRISE EN COMPTE DU CARACTERE UNILATERAL
      DO 30 I=1,3
       IF(S3(I).LE.0.D0)THEN
        BT3(I)=0.D0
        ELSE
        IF (BT3(I).GT.2.3D0) THEN
        BT3(I)=2.3D0
        END IF
       END IF
        B(I)=EXP(BT3(I))
30    CONTINUE

C     CARR� SUPERIEUR RELIANT LES CONTRAINTES NORMALES DANS
C       LA MATRICE SECANTE
      T1 = B(2)
      T2 = B(3)
      T5 = NU ** 2
      T7 = B(1)
      T8 = T7 * T1
      T17 = 0.1D1 / (-0.1D1 * T8 * T2 + T7 * T5 + T5 * T2 + 0.2D1 * T5 *
     # NU + T5 * T1)
      T21 = (T2 + NU) * NU * T17
      T24 = (NU + T1) * NU * T17
      T26 = 0.1D1 * T5
      T31 = (T7 + NU) * NU * T17
      H66(1,1) = (-0.1D1 * T1 * T2 + T5) * T17
      H66(1,2) = -T21
      H66(1,3) = -T24
      H66(2,1) = -T21
      H66(2,2) = -(T7 * T2 - T26) * T17
      H66(2,3) = -T31
      H66(3,1) = -T24
      H66(3,2) = -T31
      H66(3,3) = -(T8 - T26) * T17
C     SUITE DE LA MATRICE SECANTE (TERMES DE CISAILLEMENT)
      H66(4,4) = EXP(-(BT3(1)+BT3(2)))/(1.D0+NU)
      H66(5,5) = EXP(-(BT3(1)+BT3(3)))/(1.D0+NU)
      H66(6,6) = EXP(-(BT3(2)+BT3(3)))/(1.D0+NU)

      DO 40 I=1,6
       DO 50 J=1,6
        H66(I,J)=E*H66(I,J)*EXP(-BC)
50     CONTINUE
40    CONTINUE
      END

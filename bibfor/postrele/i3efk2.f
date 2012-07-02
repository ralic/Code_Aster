      SUBROUTINE I3EFK2(FK,NBEVAL,R,S,EVALFK)
      IMPLICIT NONE
C
      REAL*8  FK(4,*),R(*),S(*),EVALFK(3,*)
      INTEGER NBEVAL
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ------------------------------------------------------------------
C     EVALUATION EN NBEVAL COUPLES (R,S) DE FK
C     ------------------------------------------------------------------
C IN  FK     : R : TABLE(1..4,1..2) DES COEF DE FK
C IN  NBEVAL : R : NOMBRE D' EVALUTAION DEMANDE
C IN  R      : R : TABLE(1..NBEVAL) DES VALEUR DE LA COORDO DE REF 1
C IN  S      : R : TABLE(1..NBEVAL) DES VALEUR DE LA COORDO DE REF 2
C OUT EVALFK : R : TABLE(1..3,1..NBEVAL) DES VALEUR DE FK
C     ------------------------------------------------------------------
C
      INTEGER I
      REAL*8  R1,S1
C
C======================================================================
C
      DO 10, I = 1, NBEVAL, 1
         R1          = R(I)
         S1          = S(I)
         EVALFK(1,I) = FK(1,1) + FK(2,1)*R1 + S1*(FK(3,1) + FK(4,1)*R1)
         EVALFK(2,I) = FK(1,2) + FK(2,2)*R1 + S1*(FK(3,2) + FK(4,2)*R1)
         EVALFK(3,I) = FK(1,3) + FK(2,3)*R1 + S1*(FK(3,3) + FK(4,3)*R1)
10    CONTINUE
      END

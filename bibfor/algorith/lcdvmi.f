      SUBROUTINE LCDVMI (SIGMA,Y,F,DFDS,D2FDS,SEQ)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL*8    Y, SIGMA(6), F, SEQ, DFDS(6), D2FDS(6,6)
C-----------------------------------------------------------------------
C  ROUTINE D EVALUATION DU CRITERE DE VON-MISES ISOTROPE
C  ET DE SES DERIVEES PARTIELLES PAR RAPPORT A SIGMA
C
C        !!! DONNEES ET RESULTATS PAR PG !!!
C-----------------------------------------------------------------------
C  ENTREES
C    SIGMA : VECTEUR CONTRAINTE
C    Y     : VALEUR COURANTE DU CRITERE
C
C  SORTIES
C    F     : CRITERE (NUL A LA CONVERGENCE)
C    DFDS  : DERIVEE DE F PAR RAPPORT AUX CONTRAINTES
C    D2FDS : DERIVEE SECONDE DE F PAR RAPPORT AUX CONTRAINTES
C    SEQ   : CONTRAINTE EQUIVALENTE (= Y A LA CONVERGENCE)
C-----------------------------------------------------------------------
      INTEGER  I, J
      REAL*8   S23, S31, S12, T12, T23, T31
C-----------------------------------------------------------------------
C  CALCUL DE F
C  -----------
C
      S23 = (SIGMA(2)-SIGMA(3))*(SIGMA(2)-SIGMA(3))
      S31 = (SIGMA(3)-SIGMA(1))*(SIGMA(3)-SIGMA(1))
      S12 = (SIGMA(1)-SIGMA(2))*(SIGMA(1)-SIGMA(2))
      T12 = SIGMA(4)*SIGMA(4)
      T23 = SIGMA(5)*SIGMA(5)
      T31 = SIGMA(6)*SIGMA(6)
C
      SEQ = SQRT((S23+S31+S12)/2.D0+1.5D0*(T12+T23+T31))
C
      F = SEQ - Y
C
      IF(SEQ.LT.1.D-9) GOTO 20
C
C  CALCUL DE DF/DSIG (DFDS(6))
C  ---------------------------
C
      DFDS(1) = (2.D0*SIGMA(1)-SIGMA(2)-SIGMA(3))/(2.D0*SEQ)
      DFDS(2) = (2.D0*SIGMA(2)-SIGMA(3)-SIGMA(1))/(2.D0*SEQ)
      DFDS(3) = (2.D0*SIGMA(3)-SIGMA(1)-SIGMA(2))/(2.D0*SEQ)
      DFDS(4) = 3.D0*SIGMA(4)/SEQ/2.D0
      DFDS(5) = 3.D0*SIGMA(5)/SEQ/2.D0
      DFDS(6) = 3.D0*SIGMA(6)/SEQ/2.D0
C
      D2FDS(1,1) = ( 1.0D0-DFDS(1)*DFDS(1) )/SEQ
      D2FDS(1,2) = (-0.5D0-DFDS(1)*DFDS(2) )/SEQ
      D2FDS(1,3) = (-0.5D0-DFDS(1)*DFDS(3) )/SEQ
      D2FDS(1,4) = (      -DFDS(1)*DFDS(4) )/SEQ
      D2FDS(1,5) = (      -DFDS(1)*DFDS(5) )/SEQ
      D2FDS(1,6) = (      -DFDS(1)*DFDS(6) )/SEQ
C
      D2FDS(2,2) = ( 1.0D0-DFDS(2)*DFDS(2) )/SEQ
      D2FDS(2,3) = (-0.5D0-DFDS(2)*DFDS(3) )/SEQ
      D2FDS(2,4) = (      -DFDS(2)*DFDS(4) )/SEQ
      D2FDS(2,5) = (      -DFDS(2)*DFDS(5) )/SEQ
      D2FDS(2,6) = (      -DFDS(2)*DFDS(6) )/SEQ
C
      D2FDS(3,3) = ( 1.0D0-DFDS(3)*DFDS(3) )/SEQ
      D2FDS(3,4) = (      -DFDS(3)*DFDS(4) )/SEQ
      D2FDS(3,5) = (      -DFDS(3)*DFDS(5) )/SEQ
      D2FDS(3,6) = (      -DFDS(3)*DFDS(6) )/SEQ
C
      D2FDS(4,4) = ( 3.D0-DFDS(4)*DFDS(4) )/SEQ
      D2FDS(4,5) = (     -DFDS(4)*DFDS(5) )/SEQ
      D2FDS(4,6) = (     -DFDS(4)*DFDS(6) )/SEQ
C
      D2FDS(5,5) = ( 3.D0-DFDS(5)*DFDS(5) )/SEQ
      D2FDS(5,6) = (     -DFDS(5)*DFDS(6) )/SEQ
C
      D2FDS(6,6) = ( 3.D0-DFDS(6)*DFDS(6) )/SEQ
C
      DO 10 I=1,6
        DO 10 J=I,6
          D2FDS(J,I) = D2FDS(I,J)
10    CONTINUE
C
20    CONTINUE
C
      END

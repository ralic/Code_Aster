      SUBROUTINE DXSIRO ( NE , T2VE , TENSAV , TENSAP )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
      IMPLICIT NONE
      INTEGER  NE
      REAL*8   T2VE(2,2)
      REAL*8   TENSAV(*)
      REAL*8   TENSAP(*)
C     ------------------------------------------------------------------
C     PASSAGE DES CONTRAINTES (OU DEFORMATIONS) DU REPERE INTRINSEQUE DE
C     L'ELEMENT AU REPERE UTILISATEUR (OU L'INVERSE)
C     ------------------------------------------------------------------
C     IN  NE    I      NOMBRE DE POINTS (OU SOUS-POINTS) A TRAITER
C     IN  T2VE  R  2,2  MATRICE DE PASSAGE (OBTENUE PAR DXREPE) :
C                       T2VE : INTRINSEQUE -> UTILISATEUR
C                       T2EV : UTILISATEUR -> INTRINSEQUE
C     IN  TENSAV R    *   XX  YY  ZZ  XY  XZ  YZ
C     OUT TENSAP R    *   XX  YY  ZZ  XY  XZ  YZ
C
C  REMARQUE : ON PEUT APPELER CETTE ROUTINE AVEC UN TABLEAU EN IN/OUT
C     ------------------------------------------------------------------
      REAL*8        SIGMAV(4) , SIGMAP(4), TAMPON(2)
      REAL*8        XAB(2,2) , T2EV(2,2)
C
C     TRANSPOSEE DE T2VE
C-----------------------------------------------------------------------
      INTEGER I 
C-----------------------------------------------------------------------
      T2EV(1,1) = T2VE(1,1)
      T2EV(1,2) = T2VE(2,1)
      T2EV(2,1) = T2VE(1,2)
      T2EV(2,2) = T2VE(2,2)
C
      DO 120 I = 1 , NE
         SIGMAV(1) = TENSAV(1+6*(I-1))
         SIGMAV(2) = TENSAV(4+6*(I-1))
         SIGMAV(3) = TENSAV(4+6*(I-1))
         SIGMAV(4) = TENSAV(2+6*(I-1))
C
         CALL UTBTAB ('ZERO',2,2,SIGMAV,T2EV,XAB,SIGMAP)
C
         TENSAP(1+6*(I-1)) = SIGMAP(1)
         TENSAP(2+6*(I-1)) = SIGMAP(4)
         TENSAP(3+6*(I-1)) = TENSAV(3+6*(I-1))
         TENSAP(4+6*(I-1)) = SIGMAP(2)

         TAMPON(1)=TENSAV(5+6*(I-1))
         TAMPON(2)=TENSAV(6+6*(I-1))
         TENSAP(5+6*(I-1)) = TAMPON(1) * T2EV(1,1) +
     &                       TAMPON(2) * T2EV(2,1)
         TENSAP(6+6*(I-1)) = TAMPON(1) * T2EV(1,2) +
     &                       TAMPON(2) * T2EV(2,2)
C
  120 CONTINUE
C
      END

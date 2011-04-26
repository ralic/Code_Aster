      SUBROUTINE DIAGO3(TENS,VECP,VALP)

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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================

      IMPLICIT NONE
      REAL*8   TENS(6),VALP(3),VECP(3,3)

C ----------------------------------------------------------------------
C  DIAGONALISATION MATRICE 3x3 SYMETRIQUE
C    IN    TENS   : TENSEUR SOUS LA FORME
C                     (XX YY ZZ XY XZ YZ)
C ----------------------------------------------------------------------
      REAL*8      TOL, TOLDYN
      REAL*8      TR(6), TU(6) ,  JACAUX(3)
      INTEGER     NPERM, NITJAC, TTRIJ, OTRIJ,NBIND

      DATA   NPERM ,TOL,TOLDYN    /12,1.D-10,1.D-2/
      DATA   TTRIJ,OTRIJ  /2,2/
C                          0 - tri en valeur relative
C                          2 - pas de tri
C                            0 - ordre croissant
C                            2 - pas de tri


      TR(1) = TENS(1)
      TR(2) = TENS(4)
      TR(3) = TENS(5)
      TR(4) = TENS(2)
      TR(5) = TENS(6)
      TR(6) = TENS(3)
      NBIND=3
      TU(1) = 1.D0
      TU(2) = 0.D0
      TU(3) = 0.D0
      TU(4) = 1.D0
      TU(5) = 0.D0
      TU(6) = 1.D0

         CALL JACOBI(NBIND,NPERM,TOL,TOLDYN,TR,TU,VECP,VALP,JACAUX,
     &               NITJAC,TTRIJ,OTRIJ)

      END

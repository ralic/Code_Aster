      SUBROUTINE UT2AGL ( NN , NC , P , SG , SL )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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

      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8              P(3,3) , SL(*) , SG(*)
C     ------------------------------------------------------------------
C     PASSAGE EN 2D D'UNE MATRICE TRIANGULAIRE DE NN*NC LIGNES
C     DU REPERE LOCAL AU REPERE GLOBAL
C     ------------------------------------------------------------------
CIN   I   NN   NOMBRE DE NOEUDS
CIN   I   NC   NOMBRE DE COMPOSANTES
CIN   R   P    MATRICE DE PASSAGE 3D DE GLOBAL A LOCAL
CIN   R   SL   NN*NC COMPOSANTES DE LA TRIANGULAIRE SL DANS LOCAL
COUT  R   SG   NN*NC COMPOSANTES DE LA TRIANGULAIRE SG DANS GLOBAL
C     ------------------------------------------------------------------
      REAL*8   R(4)
      INTEGER  IN(2)
C
      ZERO = 0.0D0
C
      IF ( MOD(NC,2) .EQ. 0 ) THEN
         NB = NN * NC / 2
         DO 10 I = 1 , NB
           K = 2 * ( I - 1 )
           DO 20 J = 1 , I
             IN(1) =  K    * (K+1) / 2 + 2*(J-1)
             IN(2) = (K+1) * (K+2) / 2 + 2*(J-1)
             IF ( I .EQ. J ) THEN
C            --------- BLOC DIAGONAL
               R(1) = SG(IN(1)+1)
               R(2) = - SG(IN(2)+1)
               R(3) = SG(IN(2)+1)
               R(4) = SG(IN(2)+2)
C
               DO 30 M = 1 , 2
                 DO 40 N = 1 , M
                   SL(IN(M)+N) = ZERO
                   DO 50 L = 1 , 2
                     SL(IN(M)+N) = SL(IN(M)+N) +
     +                              P(L,M) * ( R(2*(L-1)+1)*P(1,N) +
     +                                         R(2*(L-1)+2)*P(2,N) )
  50              CONTINUE
  40            CONTINUE
  30          CONTINUE
             ELSE
C              --------- BLOC EXTRA - DIAGONAL
               DO 60 M = 1 , 2
                 DO 70 N = 1 , 2
                   SL(IN(M)+N) = ZERO
                   DO 80 L = 1 , 2
                     SL(IN(M)+N) = SL(IN(M)+N) +
     +                              P(L,M) * ( SG(IN(L)+1)*P(1,N) +
     +                                         SG(IN(L)+2)*P(2,N) )
 80               CONTINUE
 70             CONTINUE
 60           CONTINUE
             ENDIF
 20       CONTINUE
 10     CONTINUE
C
      ENDIF
C
      END

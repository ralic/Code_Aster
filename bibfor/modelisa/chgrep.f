      SUBROUTINE CHGREP ( TYPE, PGL1, PGL2, MATL, MATG )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*2         TYPE
      REAL*8                    PGL1(3,3), PGL2(3,3), MATL(*), MATG(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/02/2008   AUTEUR MACOCCO K.MACOCCO 
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
C     ------------------------------------------------------------------
C IN  : TYPE   : 'LG' > PASSAGE REPERE LOCAL  > GLOBAL
C              : 'GL' > PASSAGE REPERE GLOBAL > LOCAL
C IN  : PGL1   : MATRICE DE PASSAGE DU GLOBAL > LOCAL AU NOEUD 1
C IN  : PGL2   : MATRICE DE PASSAGE DU GLOBAL > LOCAL AU NOEUD 2
C IN  : MATL   : DEMI - MATRICE STOCKE COLONNE = VECTEUR LONGUEUR N
C OUT : MATG   : DEMI - MATRICE STOCKE COLONNE = VECTEUR LONGUEUR N
C     ------------------------------------------------------------------
C               T                                  T        -1     T
C     (MA) = (R) (ML) (R)   OU  (ML) = (R) (MA) (R)  CAR (R)  = (R)
C     ------------------------------------------------------------------
      REAL*8     ML12(12,12), MR12(12,12), MTR12(12,12), MV12(12,12)
C
      CALL ASSERT(( TYPE .EQ. 'LG' ).OR.( TYPE .EQ. 'GL' ))
      IF ( TYPE .EQ. 'LG' ) THEN
         DO 10 I = 1 , 3
            DO 12 J = 1 , 3
               MR12(I  ,J  ) = PGL1(I,J)
               MR12(I  ,J+3) = 0.D0
               MR12(I  ,J+6) = 0.D0
               MR12(I  ,J+9) = 0.D0
               MR12(I+3,J  ) = 0.D0
               MR12(I+3,J+3) = PGL1(I,J)
               MR12(I+3,J+6) = 0.D0
               MR12(I+3,J+9) = 0.D0
               MR12(I+6,J+3) = 0.D0
               MR12(I+6,J  ) = 0.D0
               MR12(I+6,J+6) = PGL2(I,J)
               MR12(I+6,J+9) = 0.D0
               MR12(I+9,J  ) = 0.D0
               MR12(I+9,J+3) = 0.D0
               MR12(I+9,J+6) = 0.D0
               MR12(I+9,J+9) = PGL2(I,J)
 12         CONTINUE
 10      CONTINUE
         CALL TMAT  ( 12, MR12, MTR12 )
C
      ELSEIF ( TYPE .EQ. 'GL' ) THEN
         DO 20 I = 1 , 3
            DO 22 J = 1 , 3
               MTR12(I  ,J  ) = PGL1(I,J)
               MTR12(I  ,J+3) = 0.D0
               MTR12(I  ,J+6) = 0.D0
               MTR12(I  ,J+9) = 0.D0
               MTR12(I+3,J  ) = 0.D0
               MTR12(I+3,J+3) = PGL1(I,J)
               MTR12(I+3,J+6) = 0.D0
               MTR12(I+3,J+9) = 0.D0
               MTR12(I+6,J+3) = 0.D0
               MTR12(I+6,J  ) = 0.D0
               MTR12(I+6,J+6) = PGL2(I,J)
               MTR12(I+6,J+9) = 0.D0
               MTR12(I+9,J  ) = 0.D0
               MTR12(I+9,J+3) = 0.D0
               MTR12(I+9,J+6) = 0.D0
               MTR12(I+9,J+9) = PGL2(I,J)
 22         CONTINUE
 20      CONTINUE
         CALL TMAT  ( 12, MTR12, MR12 )
C
      ENDIF
      CALL VECMA ( MATL, 78, ML12, 12)
      CALL PMAT  ( 12, MTR12, ML12, MV12)
      CALL PMAT  ( 12, MV12, MR12, MTR12)
      CALL MAVEC ( MTR12, 12, MATG, 78 )
C
      END

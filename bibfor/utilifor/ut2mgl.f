      SUBROUTINE UT2MGL ( NN , NC , P , SG , SL )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 12/11/97   AUTEUR CIBHHGB G.BERTRAND 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8              P(3,3) , SG(*) , SL(*)
C     ------------------------------------------------------------------
C     PASSAGE EN 2D D'UNE MATRICE TRIANGULAIRE DE NN*NC LIGNES
C     DU REPERE GLOBAL AU REPERE LOCAL
C     ------------------------------------------------------------------
CIN   I   NN   NOMBRE DE NOEUDS
CIN   I   NC   NOMBRE DE COMPOSANTES
CIN   R   P    MATRICE DE PASSAGE 3D DE GLOBAL A LOCAL
CIN   R   SG   NN*NC COMPOSANTES DE LA TRIANGULAIRE SG DANS GLOBAL
COUT  R   SL   NN*NC COMPOSANTES DE LA TRIANGULAIRE SL DANS LOCALL
C     ------------------------------------------------------------------
      REAL*8   R(4) 
      REAL*8   ML6(6,6), MR6(6,6), MTR6(6,6), MV6(6,6)
      REAL*8   ML3(3,3), MR3(3,3), MTR3(3,3), MV3(3,3)
      INTEGER  IN(2)
C
      ZERO = 0.0D0
      UN   = 1.0D0
C
      IF ( MOD(NC,2) .EQ. 0 ) THEN
        NB = NN * NC / 2
        DO 10 I = 1 , NB
          K = 2 * ( I - 1 )
          DO 20 J = 1 , I
            IN(1) =  K    * (K+1) / 2 + 2*(J-1)
            IN(2) = (K+1) * (K+2) / 2 + 2*(J-1)
            IF ( I .EQ. J ) THEN
C             --------- BLOC DIAGONAL
              R(1) = SG(IN(1)+1)
              R(2) = SG(IN(2)+1)
              R(3) = SG(IN(2)+1)
              R(4) = SG(IN(2)+2)
              DO 30 M = 1 , 2
                DO 40 N = 1 , M
                  SL(IN(M)+N) = ZERO
                  DO 50 L = 1 , 2
                    SL(IN(M)+N) = SL(IN(M)+N)+ P(M,L)*
     +                            (R(2*(L-1)+1)*P(N,1)
     +                           + R(2*(L-1)+2)*P(N,2))
  50              CONTINUE
  40            CONTINUE
  30          CONTINUE
            ELSE
C             --------- BLOC EXTRA - DIAGONAL
              DO 60 M = 1 , 2
                DO 70 N = 1 , 2
                  SL(IN(M)+N) = ZERO
                  DO 80 L = 1 , 2
                    SL(IN(M)+N) = SL(IN(M)+N)+ P(M,L)*
     +                                      ( SG(IN(L)+1)*P(N,1)
     +                                      + SG(IN(L)+2)*P(N,2) )
  80              CONTINUE
  70            CONTINUE
  60          CONTINUE
            ENDIF
 20       CONTINUE
 10     CONTINUE
C
      ELSEIF ( MOD(NC,2) .EQ. 1 ) THEN
       IF (NC*NN.EQ.3) THEN
         DO 90 I = 1 , 3
         DO 90 J = 1 , 3
           MTR3(I,J) = ZERO
 90      CONTINUE
C
         MTR3(1,1) = P(1,1)
         MTR3(1,2) = P(1,2)
         MTR3(2,1) = P(2,1)
         MTR3(2,2) = P(2,2)
         MTR3(3,3) = UN
C
         CALL TMAT(3,MTR3,MR3)
         CALL VECMA(SG,6,ML3,3)
         CALL PMAT(3,MTR3,ML3,MV3)
         CALL PMAT(3,MV3,MR3,MTR3)
         CALL MAVEC(MTR3,3,SL,6)
C
       ELSEIF (NC*NN.EQ.6) THEN
         DO 100 I = 1 , 6
         DO 100 J = 1 , 6
           MTR6(I,J) = ZERO
 100     CONTINUE
C
         MTR6(1,1) = P(1,1)
         MTR6(1,2) = P(1,2)
         MTR6(2,1) = P(2,1)
         MTR6(2,2) = P(2,2)
         MTR6(3,3) = UN
C
         MTR6(4,4) = P(1,1)
         MTR6(4,5) = P(1,2)
         MTR6(5,4) = P(2,1)
         MTR6(5,5) = P(2,2)
         MTR6(6,6) = UN
C
         CALL TMAT(6,MTR6,MR6)
         CALL VECMA(SG,21,ML6,6)
         CALL PMAT(6,MTR6,ML6,MV6)
         CALL PMAT(6,MV6,MR6,MTR6)
         CALL MAVEC(MTR6,6,SL,21)
C
       ENDIF
C
      ENDIF
C
      END

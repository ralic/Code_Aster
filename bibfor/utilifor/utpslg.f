      SUBROUTINE UTPSLG ( NN , NC , P , SL , SG )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 01/08/95   AUTEUR CIBHHLV L.VIVAN 
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
      REAL*8              P(3,3) , SL(*) , SG(*)
C     ------------------------------------------------------------------
C     PASSAGE EN 3D D'UNE MATRICE TRIANGULAIRE DE NN*NC LIGNES
C     DU REPERE LOCAL AU REPERE GLOBAL
C     ------------------------------------------------------------------
CIN   I   NN   NOMBRE DE NOEUDS
CIN   I   NC   NOMBRE DE COMPOSANTES
CIN   R   P    MATRICE DE PASSAGE 3D DE GLOBAL A LOCAL
CIN   R   SL   NN*NC COMPOSANTES DE LA TRIANGULAIRE SL DANS LOCAL
COUT  R   SG   NN*NC COMPOSANTES DE LA TRIANGULAIRE SG DANS GLOBAL
C     ------------------------------------------------------------------
      REAL*8   R(9) , ZERO
      REAL*8   ML14(14,14), MR14(14,14), MTR14(14,14), MV14(14,14)
      REAL*8   ML16(16,16), MR16(16,16), MTR16(16,16), MV16(16,16)
      INTEGER  IN(3)
      DATA     ZERO / 0.D0 /
C
      IF ( MOD(NC,3) .EQ. 0 ) THEN
         NB = NN * NC / 3
         DO 100 I = 1 , NB
           K = 3 * ( I - 1 )
           DO 110 J = 1 , I
             IN(1) =  K    * (K+1) / 2 + 3*(J-1)
             IN(2) = (K+1) * (K+2) / 2 + 3*(J-1)
             IN(3) = (K+2) * (K+3) / 2 + 3*(J-1)
             IF ( I .EQ. J ) THEN
C            --------- BLOC DIAGONAL
               R(1) = SL(IN(1)+1)
               R(2) = SL(IN(2)+1)
               R(3) = SL(IN(3)+1)
               R(4) = SL(IN(2)+1)
               R(5) = SL(IN(2)+2)
               R(6) = SL(IN(3)+2)
               R(7) = SL(IN(3)+1)
               R(8) = SL(IN(3)+2)
               R(9) = SL(IN(3)+3)
               DO 120 M = 1 , 3
                 DO 130 N = 1 , M
                   SG(IN(M)+N) = ZERO
                   DO 140 L = 1 , 3
                     SG(IN(M)+N) = SG(IN(M)+N) + 
     +                              P(L,M) * ( R(3*(L-1)+1)*P(1,N) +
     +                                         R(3*(L-1)+2)*P(2,N) +
     +                                         R(3*(L-1)+3)*P(3,N) )
  140              CONTINUE
  130            CONTINUE
  120          CONTINUE
             ELSE
C              --------- BLOC EXTRA - DIAGONAL
               DO 150 M = 1 , 3
                 DO 160 N = 1 , 3
                   SG(IN(M)+N) = ZERO
                   DO 170 L = 1 , 3
                     SG(IN(M)+N) = SG(IN(M)+N) + 
     +                              P(L,M) * ( SL(IN(L)+1)*P(1,N) +
     +                                         SL(IN(L)+2)*P(2,N) +
     +                                         SL(IN(L)+3)*P(3,N) )
 170               CONTINUE
 160             CONTINUE
 150           CONTINUE
             ENDIF
 110       CONTINUE
 100     CONTINUE
C
      ELSEIF ( MOD(NC,3) .EQ. 1 ) THEN
         DO 202 I = 1 , 14
            DO 204 J = 1 , 14
               MR14(I,J) = 0.D0
 204        CONTINUE
 202     CONTINUE
         DO 200 I = 1 , 3
            DO 210 J = 1 , 3
               MR14(I   ,J   ) = P(I,J)
               MR14(I+3 ,J+3 ) = P(I,J)
               MR14(I+7 ,J+7 ) = P(I,J)
               MR14(I+10,J+10) = P(I,J)
 210        CONTINUE
 200     CONTINUE
         MR14(  7,  7) = 1.D0
         MR14( 14, 14) = 1.D0
         CALL TMAT(14,MR14,MTR14)
         CALL VECMA(SL,105,ML14,14)
         CALL PMAT(14,MTR14,ML14,MV14)
         CALL PMAT(14,MV14,MR14,MTR14)
         CALL MAVEC(MTR14,14,SG,105)
C
      ELSEIF ( MOD(NC,3) .EQ. 2 ) THEN
         DO 302 I = 1 , 16
            DO 304 J = 1 , 16
               MR16(I,J) = 0.D0
 304        CONTINUE
 302     CONTINUE
         DO 300 I = 1 , 3
            DO 310 J = 1 , 3
               MR16(I   ,J   ) = P(I,J)
               MR16(I+3 ,J+3 ) = P(I,J)
               MR16(I+8 ,J+8 ) = P(I,J)
               MR16(I+11,J+11) = P(I,J)
 310        CONTINUE
 300     CONTINUE
         MR16(  7,  7) = 1.D0
         MR16(  8,  8) = 1.D0
         MR16( 15, 15) = 1.D0
         MR16( 16, 16) = 1.D0
         CALL TMAT(16,MR16,MTR16)
         CALL VECMA(SL,136,ML16,16)
         CALL PMAT(16,MTR16,ML16,MV16)
         CALL PMAT(16,MV16,MR16,MTR16)
         CALL MAVEC(MTR16,16,SG,136)
      ENDIF
C
      END

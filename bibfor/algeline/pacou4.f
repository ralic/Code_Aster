      SUBROUTINE PACOU4 ( A, N, C, D, SING )
      IMPLICIT NONE
C ---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C
C ARGUMENTS
C ---------
      INCLUDE 'jeveux.h'
      INTEGER N
      REAL*8 A(N,*), C(*), D(*)
      LOGICAL SING
C
C --------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,J ,K 
      REAL*8 SCALE ,SIGMA ,SUM ,TAU 
C-----------------------------------------------------------------------
      SING  = .FALSE.
      SCALE = 0.0D0
C
      DO 17 K = 1, N-1
C
         DO 11 I = K, N
            SCALE = MAX(SCALE,ABS(A(I,K)))
   11    CONTINUE
C
         IF ( ABS(SCALE) .LE. 1.0D-30 ) THEN
            SING = .TRUE.
            C(K) = 0.0D0
            D(K) = 0.0D0
C
         ELSE
            DO 12 I = K, N
               A(I,K) = A(I,K)/SCALE
   12       CONTINUE
C
            SUM = 0.0D0
            DO 13 I = K, N
               SUM = SUM + A(I,K)**2
   13       CONTINUE
C
            SIGMA  = SIGN ( SQRT(SUM), A(K,K) )
            A(K,K) = A(K,K) + SIGMA
            C(K)   = SIGMA*A(K,K)
            D(K)   = -SCALE*SIGMA
C
            DO 16 J = K+1, N
C
               SUM = 0.0D0
               DO 14 I = K, N
                  SUM = SUM + A(I,K)*A(I,J)
   14          CONTINUE
C
               TAU = SUM/C(K)
               DO 15 I = K, N
                  A(I,J) = A(I,J) - TAU*A(I,K)
   15          CONTINUE
   16       CONTINUE
C
         END IF
   17 CONTINUE
C
      D(N) = A(N,N)
      IF ( ABS(D(N)) .LE. 1.0D-30 ) SING = .TRUE.
C
      END

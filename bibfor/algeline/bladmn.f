      SUBROUTINE BLADMN(N,M,P,MAT,LDA,TRAV,LDB,C,LDC,BETA,OPTA,OPTB)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 13/01/2003   AUTEUR PABHHHH N.TARDIEU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER N,M,P,LDA,LDB,LDC,OPTA,OPTB
      INTEGER I,J,K
      REAL*8 C(LDC,*),TRAV(LDB,*),MAT(LDA,*),BETA,S

      IF (OPTA.EQ.1) THEN
        IF (OPTB.EQ.1) THEN
          DO 30 J = 1,M
            DO 20 I = 1,N
              S = 0.D0
              DO 10 K = 1,P
                S = S + MAT(I,K)*TRAV(K,J)
   10         CONTINUE
              C(I,J) = BETA*C(I,J) - S
   20       CONTINUE
   30     CONTINUE
        ELSE
          DO 60 J = 1,M
            DO 50 I = 1,N
              S = 0.D0
              DO 40 K = 1,P
                S = S + MAT(I,K)*TRAV(J,K)
   40         CONTINUE
              C(I,J) = BETA*C(I,J) - S
   50       CONTINUE
   60     CONTINUE
        END IF
      ELSE
        IF (OPTB.EQ.1) THEN
          DO 90 J = 1,M
            DO 80 I = 1,N
              S = 0.D0
              DO 70 K = 1,P
                S = S + MAT(K,I)*TRAV(K,J)
   70         CONTINUE
              C(I,J) = BETA*C(I,J) - S
   80       CONTINUE
   90     CONTINUE
        ELSE
          DO 120 J = 1,M
            DO 110 I = 1,N
              S = 0.D0
              DO 100 K = 1,P
                S = S + MAT(K,I)*TRAV(J,K)
  100         CONTINUE
              C(I,J) = BETA*C(I,J) - S
  110       CONTINUE
  120     CONTINUE
        END IF
      END IF
      END

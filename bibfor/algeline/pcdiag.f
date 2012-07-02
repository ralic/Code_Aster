      SUBROUTINE PCDIAG(N,ICPL,ICPC,ICPD)
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
C TOLE CRP_4
C  CALCULE LE POINTEUT ICPD=ADRESSE DANS CA DU DERNIER COEFF
C  DE L (DIAGONALE A PART)
      IMPLICIT NONE
      INTEGER*4 ICPC(*)
      INTEGER   ICPD(N),ICPL(0:N)

C-----------------------------------------------------------------------
      INTEGER I ,K ,K1 ,K2 ,N 
C-----------------------------------------------------------------------
      K1 = 1
      DO 30 I = 1,N
        K2 = ICPL(I)
        ICPD(I) = K1 - 1
        DO 10 K = K1,K2
          IF (ICPC(K).LT.I) THEN
            ICPD(I) = K
          ELSE
            GO TO 20
          END IF
   10   CONTINUE
   20   CONTINUE
        K1 = K2 + 1
   30 CONTINUE

      END

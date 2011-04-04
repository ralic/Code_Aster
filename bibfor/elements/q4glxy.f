      SUBROUTINE Q4GLXY (HLT2, DEPF, LAMBDA )
      IMPLICIT  NONE
      REAL*8    HLT2(4,6),DEPF(12),LAMBDA(4)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2011   AUTEUR DESOZA T.DESOZA 
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
C     ------------------------------------------------------------------
C     'LAMBDA' DE L'ELEMENT DE PLAQUE Q4G
C     ------------------------------------------------------------------
      INTEGER  I, J, K
      REAL*8   TB(6,12)
      REAL*8   BLB(4,12)
C     ------------------------------------------------------------------
C
C       ---- CALCUL DE LA MATRICE TB -------------------------------
      DO 200 K = 1,6
        DO 201 J = 1,12
          TB(K,J) = 0.D0
 201    CONTINUE
 200  CONTINUE
      TB(3,2) = 0.25D0
      TB(3,5) = -0.25D0
      TB(3,8) = 0.25D0
      TB(3,11) = -0.25D0
      TB(6,3) = 0.25D0
      TB(6,6) = -0.25D0
      TB(6,9) = 0.25D0
      TB(6,12) = -0.25D0

C        -------------- BLB = HLT2.TB ---------------------------
      DO 234 I = 1,4
        DO 236 J = 1,12
          BLB(I,J) = 0.D0
          DO 238 K = 1,6
            BLB(I,J) = BLB(I,J) + HLT2(I,K)*TB(K,J)
 238      CONTINUE
 236    CONTINUE
 234  CONTINUE
C        -------- LAMBDA = BLB.DEPF -----------------------------
      DO 240 I = 1,4
        LAMBDA(I) = 0.D0
 240  CONTINUE
      DO 242 I = 1,4
        DO 244 J = 1,12
          LAMBDA(I) = LAMBDA(I) + BLB(I,J)*DEPF(J)
 244    CONTINUE
 242  CONTINUE
C
      END

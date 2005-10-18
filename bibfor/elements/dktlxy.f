      SUBROUTINE DKTLXY ( CODI, LCOT, HLT2, DEPF, LAMBDA )
      IMPLICIT  NONE
      REAL*8    HLT2(4,6), DEPF(9), CODI(*), LCOT(*), LAMBDA(4)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/10/2005   AUTEUR CIBHHLV L.VIVAN 
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
C     'LAMBDA' DE L'ELEMENT DE PLAQUE DKT
C     ------------------------------------------------------------------
      INTEGER  I, J, K, NNO, NC
      REAL*8   TKT(6,9), BL(4,9), LAMDA(4)
      REAL*8   CL(3) , SL(3) , CS(3) , CU(3) , SU(3)
C     ------------------------------------------------------------------
C
      NNO = 3
      NC  = 3
C
      DO 100 I = 1, NC
         CL(I) = 6.D0 * CODI(   I) / LCOT(I)
         SL(I) = 6.D0 * CODI(NC+I) / LCOT(I)
         CS(I) = 3.D0 * CODI(   I) * CODI(NC+I)
         CU(I) = 3.D0 * CODI(   I) * CODI(   I)
         SU(I) = 3.D0 * CODI(NC+I) * CODI(NC+I)
  100 CONTINUE
C
      TKT(1,1) = - CL(1) - CL(1)
      TKT(1,2) =   CU(1) + CU(1)
      TKT(1,3) =   CS(1) + CS(1)
      TKT(1,4) =   CL(1) + CL(1)
      TKT(1,5) =   CU(1) + CU(1)
      TKT(1,6) =   CS(1) + CS(1)
      TKT(1,7) =   0.D0
      TKT(1,8) =   0.D0
      TKT(1,9) =   0.D0
      TKT(2,1) =   CL(3) + CL(3)
      TKT(2,2) =   CU(3) + CU(3)
      TKT(2,3) =   CS(3) + CS(3)
      TKT(2,4) =   0.D0
      TKT(2,5) =   0.D0
      TKT(2,6) =   0.D0
      TKT(2,7) = - CL(3) - CL(3)
      TKT(2,8) =   CU(3) + CU(3)
      TKT(2,9) =   CS(3) + CS(3)
      TKT(3,1) =   CL(3) - CL(1)
      TKT(3,2) =   CU(3) + CU(1)
      TKT(3,3) =   CS(3) + CS(1)
      TKT(3,4) =   CL(1) + CL(2)
      TKT(3,5) =   CU(1) - CU(2)
      TKT(3,6) =   CS(1) - CS(2)
      TKT(3,7) = - CL(3) - CL(2)
      TKT(3,8) =   CU(3) - CU(2)
      TKT(3,9) =   CS(3) - CS(2)
      TKT(4,1) = - SL(1) - SL(1)
      TKT(4,2) =   CS(1) + CS(1)
      TKT(4,3) =   SU(1) + SU(1)
      TKT(4,4) =   SL(1) + SL(1)
      TKT(4,5) =   CS(1) + CS(1)
      TKT(4,6) =   SU(1) + SU(1)
      TKT(4,7) =   0.D0
      TKT(4,8) =   0.D0
      TKT(4,9) =   0.D0
      TKT(5,1) =   SL(3) + SL(3)
      TKT(5,2) =   CS(3) + CS(3)
      TKT(5,3) =   SU(3) + SU(3)
      TKT(5,4) =   0.D0
      TKT(5,5) =   0.D0
      TKT(5,6) =   0.D0
      TKT(5,7) = - SL(3) - SL(3)
      TKT(5,8) =   CS(3) + CS(3)
      TKT(5,9) =   SU(3) + SU(3)
      TKT(6,1) =   SL(3) - SL(1)
      TKT(6,2) =   CS(3) + CS(1)
      TKT(6,3) =   SU(3) + SU(1)
      TKT(6,4) =   SL(1) + SL(2)
      TKT(6,5) =   CS(1) - CS(2)
      TKT(6,6) =   SU(1) - SU(2)
      TKT(6,7) = - SL(3) - SL(2)
      TKT(6,8) =   CS(3) - CS(2)
      TKT(6,9) =   SU(3) - SU(2)
C
C     ------ LAMDA = HLT2.TKT.DEPF ------------------------------------
      DO 120 K = 1, 36
         BL(K,1) = 0.D0
  120 CONTINUE
      DO 150 I = 1, 4
         DO 140 J = 1, 3*NNO
            DO 130 K = 1, 6
               BL(I,J) = BL(I,J) + HLT2(I,K) * TKT(K,J)
  130       CONTINUE
  140    CONTINUE
  150 CONTINUE
      DO 190 I = 1, 4
         LAMBDA(I) = 0.D0
  190 CONTINUE
      DO 210 I = 1, 4
         DO 200 J = 1, 3*NNO
            LAMBDA(I) = LAMBDA(I) + BL(I,J) * DEPF(J)
  200    CONTINUE
  210 CONTINUE
C
      END

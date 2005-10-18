      SUBROUTINE DSXHLT ( DF , JACOB , HLT2 )
      IMPLICIT  NONE
      REAL*8    DF(3,3), JACOB(*), HLT2(4,6)
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
C     -----------------------------------------------------------------
C     MATRICE PRODUIT HL.T2(4,6)
C     -----------------------------------------------------------------
      INTEGER I, J, K
      REAL*8  VJ11, VJ12, VJ21, VJ22, HL(4,6), T2(3,3)
C     ---------------------------------------------------------------
      VJ11 = JACOB(1)
      VJ12 = JACOB(2)
      VJ21 = JACOB(3)
      VJ22 = JACOB(4)
C
      HL(1,1) =   DF(1,1)
      HL(1,2) = - DF(3,3)
      HL(1,3) =   0.D0
      HL(1,4) =   DF(1,3)
      HL(1,5) = - DF(2,3)
      HL(1,6) =   DF(1,2) - DF(3,3)
      HL(2,1) =   DF(1,3)
      HL(2,2) = - DF(2,3)
      HL(2,3) =   DF(3,3) - DF(1,2)
      HL(2,4) =   DF(3,3)
      HL(2,5) = - DF(2,2)
      HL(2,6) =   0.D0
      HL(3,1) =   DF(1,2)
      HL(3,2) =   0.D0
      HL(3,3) =   DF(2,3)
      HL(3,4) =   DF(2,3)
      HL(3,5) =   0.D0
      HL(3,6) =   DF(2,2)
      HL(4,1) =   0.D0
      HL(4,2) =   DF(1,3)
      HL(4,3) =   DF(1,1)
      HL(4,4) =   0.D0
      HL(4,5) =   DF(1,2)
      HL(4,6) =   DF(1,3)
C
      T2(1,1) =        VJ11 * VJ11
      T2(1,2) =        VJ12 * VJ12
      T2(1,3) = 2.D0 * VJ11 * VJ12
      T2(2,1) =        VJ21 * VJ21
      T2(2,2) =        VJ22 * VJ22
      T2(2,3) = 2.D0 * VJ21 * VJ22
      T2(3,1) =        VJ11 * VJ21
      T2(3,2) =        VJ12 * VJ22
      T2(3,3) = VJ11 * VJ22 + VJ12 * VJ21
C
      DO 100 K = 1 , 24
         HLT2(K,1) = 0.D0
 100  CONTINUE
      DO 110 I = 1, 4
         DO 110 J = 1, 3
            DO 110 K = 1, 3
               HLT2(I,J)   = HLT2(I,J)   + HL(I,K)   * T2(K,J)
               HLT2(I,J+3) = HLT2(I,J+3) + HL(I,K+3) * T2(K,J)
 110  CONTINUE
      END

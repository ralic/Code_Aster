      SUBROUTINE DSQBFB ( INT , R , BFB )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/03/98   AUTEUR CIBHHLV L.VIVAN 
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
      INTEGER  INT
      REAL*8   R(*)
      REAL*8   BFB(3,12)
C     -----------------------------------------------------
C     MATRICE BFB(3,12) AU POINT QSI ETA POUR L'ELEMENT DSQ
C     -----------------------------------------------------
C
      REAL*8  VJ11 , VJ12 , VJ21 , VJ22
      REAL*8  QSI,ETA,PETA,META,PQSI,MQSI
C     ------------------ PARAMETRAGE QUADRANGLE ------------------------
      INTEGER NPG , NC , NNO
      INTEGER LJACO,LTOR,LQSI,LETA
               PARAMETER (NPG   = 4)
               PARAMETER (NNO   = 4)
               PARAMETER (NC    = 4)
               PARAMETER (LJACO = 2)
               PARAMETER (LTOR  = LJACO + 4)
               PARAMETER (LQSI  = LTOR  + 1)
               PARAMETER (LETA  = LQSI + NPG + NNO + 2*NC)
C     ------------------------------------------------------------------
      QSI  = R(LQSI+INT-1)
      ETA  = R(LETA+INT-1)
      VJ11 = R(LJACO)
      VJ12 = R(LJACO+1)
      VJ21 = R(LJACO+2)
      VJ22 = R(LJACO+3)
C
      PETA = (1.D0 + ETA) / 4.D0
      META = (1.D0 - ETA) / 4.D0
      PQSI = (1.D0 + QSI) / 4.D0
      MQSI = (1.D0 - QSI) / 4.D0
C
      DO 100 K = 1 , 36
         BFB(K,1) = 0.D0
 100  CONTINUE
C
      BFB(1, 2) = - META * VJ11 - MQSI * VJ12
      BFB(1, 5) =   META * VJ11 - PQSI * VJ12
      BFB(1, 8) =   PETA * VJ11 + PQSI * VJ12
      BFB(1,11) = - PETA * VJ11 + MQSI * VJ12
      BFB(2, 3) = - META * VJ21 - MQSI * VJ22
      BFB(2, 6) =   META * VJ21 - PQSI * VJ22
      BFB(2, 9) =   PETA * VJ21 + PQSI * VJ22
      BFB(2,12) = - PETA * VJ21 + MQSI * VJ22
      BFB(3, 2) = BFB(2, 3)
      BFB(3, 3) = BFB(1, 2)
      BFB(3, 5) = BFB(2, 6)
      BFB(3, 6) = BFB(1, 5)
      BFB(3, 8) = BFB(2, 9)
      BFB(3, 9) = BFB(1, 8)
      BFB(3,11) = BFB(2,12)
      BFB(3,12) = BFB(1,11)
      END

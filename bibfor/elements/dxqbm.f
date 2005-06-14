      SUBROUTINE DXQBM  ( INT , R , BM )
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
      REAL*8   BM(3,8)
C     ------------------------------------------------------------------
C     MATRICE BM(3,8) MEMBRANE AU POINT QSI ETA POUR ELEMENTS DKQ ET DSQ
C     ------------------------------------------------------------------
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
      QSI = R(LQSI+INT-1)
      ETA = R(LETA+INT-1)
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
      BM(1,1) = - META * VJ11 - MQSI * VJ12
      BM(1,2) =   0.D0
      BM(1,3) =   META * VJ11 - PQSI * VJ12
      BM(1,4) =   0.D0
      BM(1,5) =   PETA * VJ11 + PQSI * VJ12
      BM(1,6) =   0.D0
      BM(1,7) = - PETA * VJ11 + MQSI * VJ12
      BM(1,8) =   0.D0
      BM(2,1) =   0.D0
      BM(2,2) = - META * VJ21 - MQSI * VJ22
      BM(2,3) =   0.D0
      BM(2,4) =   META * VJ21 - PQSI * VJ22
      BM(2,5) =   0.D0
      BM(2,6) =   PETA * VJ21 + PQSI * VJ22
      BM(2,7) =   0.D0
      BM(2,8) = - PETA * VJ21 + MQSI * VJ22
      BM(3,1) = BM(2,2)
      BM(3,2) = BM(1,1)
      BM(3,3) = BM(2,4)
      BM(3,4) = BM(1,3)
      BM(3,5) = BM(2,6)
      BM(3,6) = BM(1,5)
      BM(3,7) = BM(2,8)
      BM(3,8) = BM(1,7)
      END

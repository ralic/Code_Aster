      SUBROUTINE DKQBF  ( INT , R , BF )
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
      REAL*8   BF(3,12)
C     ---------------------------------------------------
C     MATRICE B(3,12) AU POINT QSI ETA POUR L'ELEMENT DKQ
C     ---------------------------------------------------
      REAL*8  VJ11 , VJ12 , VJ21 , VJ22
      REAL*8  QSI,ETA,PETA,META,PQSI,MQSI,QSIC,ETAC
      REAL*8  L5,L6,L7,L8 , C5,C6,C7,C8 , S5,S6,S7,S8
      REAL*8  CU5,CU6,CU7,CU8 , SU5,SU6,SU7,SU8 , CS5,CS6,CS7,CS8
      REAL*8  CL5,CL6,CL7,CL8 , SL5,SL6,SL7,SL8
      REAL*8  BXQ(12) , BYQ(12) , BXE(12) , BYE(12)
C     ------------------ PARAMETRAGE QUADRANGLE ------------------------
      INTEGER NPG , NC , NNO
      INTEGER LJACO,LTOR,LQSI,LETA,LWGT,LXYC,LCOTE,LCOS,LSIN
               PARAMETER (NPG   = 4)
               PARAMETER (NNO   = 4)
               PARAMETER (NC    = 4)
               PARAMETER (LJACO = 2)
               PARAMETER (LTOR  = LJACO + 4)
               PARAMETER (LQSI  = LTOR  + 1)
               PARAMETER (LETA  = LQSI + NPG + NNO + 2*NC)
               PARAMETER (LWGT  = LETA + NPG + NNO + 2*NC)
               PARAMETER (LXYC  = LWGT + NPG)
               PARAMETER (LCOTE = LXYC + 2*NC)
               PARAMETER (LCOS  = LCOTE + NC)
               PARAMETER (LSIN  = LCOS + NC)
C     ------------------------------------------------------------------
      QSI = R(LQSI+INT-1)
      ETA = R(LETA+INT-1)
      VJ11 = R(LJACO)
      VJ12 = R(LJACO+1)
      VJ21 = R(LJACO+2)
      VJ22 = R(LJACO+3)
      C5   = R(LCOS)
      C6   = R(LCOS+1)
      C7   = R(LCOS+2)
      C8   = R(LCOS+3)
      S5   = R(LSIN)
      S6   = R(LSIN+1)
      S7   = R(LSIN+2)
      S8   = R(LSIN+3)
      L5   = R(LCOTE)
      L6   = R(LCOTE+1)
      L7   = R(LCOTE+2)
      L8   = R(LCOTE+3)
C
      PETA = (1.D0 + ETA) / 4.D0
      META = (1.D0 - ETA) / 4.D0
      PQSI = (1.D0 + QSI) / 4.D0
      MQSI = (1.D0 - QSI) / 4.D0
      ETAC = (1.D0 - ETA * ETA) / 8.D0
      QSIC = (1.D0 - QSI * QSI) / 8.D0
C
      CU5 = 3.D0 * C5 * C5
      CU6 = 3.D0 * C6 * C6
      CU7 = 3.D0 * C7 * C7
      CU8 = 3.D0 * C8 * C8
      SU5 = 3.D0 * S5 * S5
      SU6 = 3.D0 * S6 * S6
      SU7 = 3.D0 * S7 * S7
      SU8 = 3.D0 * S8 * S8
      CS5 = 3.D0 * C5 * S5
      CS6 = 3.D0 * C6 * S6
      CS7 = 3.D0 * C7 * S7
      CS8 = 3.D0 * C8 * S8
      CL5 = 6.D0 * C5 / L5
      CL6 = 6.D0 * C6 / L6
      CL7 = 6.D0 * C7 / L7
      CL8 = 6.D0 * C8 / L8
      SL5 = 6.D0 * S5 / L5
      SL6 = 6.D0 * S6 / L6
      SL7 = 6.D0 * S7 / L7
      SL8 = 6.D0 * S8 / L8
C
C     --------- DERIVEE DE BETAX(I) / QSI ------------
      BXQ(1)  =        - QSI * META * CL5 + ETAC * CL8
      BXQ(2)  = - META + QSI * META * CU5 + ETAC * CU8
      BXQ(3)  =          QSI * META * CS5 + ETAC * CS8
      BXQ(4)  =          QSI * META * CL5 + ETAC * CL6
      BXQ(5)  =   META + QSI * META * CU5 - ETAC * CU6
      BXQ(6)  =          QSI * META * CS5 - ETAC * CS6
      BXQ(7)  =        - QSI * PETA * CL7 - ETAC * CL6
      BXQ(8)  =   PETA + QSI * PETA * CU7 - ETAC * CU6
      BXQ(9)  =          QSI * PETA * CS7 - ETAC * CS6
      BXQ(10) =          QSI * PETA * CL7 - ETAC * CL8
      BXQ(11) = - PETA + QSI * PETA * CU7 + ETAC * CU8
      BXQ(12) =          QSI * PETA * CS7 + ETAC * CS8
C     --------- DERIVEE DE BETAY(I) / QSI ------------
      BYQ(1)  =        - QSI * META * SL5 + ETAC * SL8
      BYQ(2)  = BXQ(3)
      BYQ(3)  = - META + QSI * META * SU5 + ETAC * SU8
      BYQ(4)  =          QSI * META * SL5 + ETAC * SL6
      BYQ(5)  = BXQ(6)
      BYQ(6)  =   META + QSI * META * SU5 - ETAC * SU6
      BYQ(7)  =        - QSI * PETA * SL7 - ETAC * SL6
      BYQ(8)  = BXQ(9)
      BYQ(9)  =   PETA + QSI * PETA * SU7 - ETAC * SU6
      BYQ(10) =          QSI * PETA * SL7 - ETAC * SL8
      BYQ(11) = BXQ(12)
      BYQ(12) = - PETA + QSI * PETA * SU7 + ETAC * SU8
C     --------- DERIVEE DE BETAX(I) / ETA ------------
      BXE(1)  =          ETA * MQSI * CL8 - QSIC * CL5
      BXE(2)  = - MQSI + ETA * MQSI * CU8 + QSIC * CU5
      BXE(3)  =          ETA * MQSI * CS8 + QSIC * CS5
      BXE(4)  =        - ETA * PQSI * CL6 + QSIC * CL5
      BXE(5)  = - PQSI + ETA * PQSI * CU6 + QSIC * CU5
      BXE(6)  =          ETA * PQSI * CS6 + QSIC * CS5
      BXE(7)  =          ETA * PQSI * CL6 + QSIC * CL7
      BXE(8)  =   PQSI + ETA * PQSI * CU6 - QSIC * CU7
      BXE(9)  =          ETA * PQSI * CS6 - QSIC * CS7
      BXE(10) =        - ETA * MQSI * CL8 - QSIC * CL7
      BXE(11) =   MQSI + ETA * MQSI * CU8 - QSIC * CU7
      BXE(12) =          ETA * MQSI * CS8 - QSIC * CS7
C     --------- DERIVEE DE BETAY(I) / ETA ------------
      BYE(1)  =          ETA * MQSI * SL8 - QSIC * SL5
      BYE(2)  = BXE(3)
      BYE(3)  = - MQSI + ETA * MQSI * SU8 + QSIC * SU5
      BYE(4)  =        - ETA * PQSI * SL6 + QSIC * SL5
      BYE(5)  = BXE(6)
      BYE(6)  = - PQSI + ETA * PQSI * SU6 + QSIC * SU5
      BYE(7)  =          ETA * PQSI * SL6 + QSIC * SL7
      BYE(8)  = BXE(9)
      BYE(9)  =   PQSI + ETA * PQSI * SU6 - QSIC * SU7
      BYE(10) =        - ETA * MQSI * SL8 - QSIC * SL7
      BYE(11) = BXE(12)
      BYE(12) =   MQSI + ETA * MQSI * SU8 - QSIC * SU7
C
C     --------------------- CALCUL DE B -------------------------------
      DO 100 I = 1, 12
        BF(1,I) = VJ11*BXQ(I) + VJ12*BXE(I)
        BF(2,I) = VJ21*BYQ(I) + VJ22*BYE(I)
        BF(3,I) = VJ11*BYQ(I) + VJ12*BYE(I) + VJ21*BXQ(I) + VJ22*BXE(I)
  100 CONTINUE
      END

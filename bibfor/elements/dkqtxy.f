      SUBROUTINE DKQTXY ( QSI, ETA, HFT2, DEPF, CODI, LCOT, VT )
      IMPLICIT  NONE
      REAL*8    QSI, ETA, HFT2(2,6), DEPF(12), CODI(*), LCOT(*), VT(2)
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
C     EFFORT TRANCHANT L'ELEMENT DE PLAQUE DKQ
C     ------------------------------------------------------------------
      INTEGER  I, J, K
      REAL*8   PQSI , MQSI , PETA , META
      REAL*8   CL(4) , SL(4) , CS(4) , CU(4) , SU(4)
      REAL*8   TKQ(6,12) , BC(2,12)
C     ------------------ PARAMETRAGE QUADRANGLE ------------------------
      INTEGER    NNO, NC
      PARAMETER (NNO = 4)
      PARAMETER (NC  = 4)
C     ------------------------------------------------------------------
C
      PETA = 1.D0 + ETA
      META = 1.D0 - ETA
      PQSI = 1.D0 + QSI
      MQSI = 1.D0 - QSI
      DO 100 I = 1, NC
         CL(I) = 1.50D0 * CODI(   I) / LCOT(I)
         SL(I) = 1.50D0 * CODI(NC+I) / LCOT(I)
         CS(I) = 0.75D0 * CODI(   I) * CODI(NC+I)
         CU(I) = 0.75D0 * CODI(   I) * CODI(   I)
         SU(I) = 0.75D0 * CODI(NC+I) * CODI(NC+I)
  100 CONTINUE
      TKQ(1,1 ) = - META * CL(1)
      TKQ(1,2 ) =   META * CU(1)
      TKQ(1,3 ) =   META * CS(1)
      TKQ(1,4 ) =   META * CL(1)
      TKQ(1,5 ) =   META * CU(1)
      TKQ(1,6 ) =   META * CS(1)
      TKQ(1,7 ) = - PETA * CL(3)
      TKQ(1,8 ) =   PETA * CU(3)
      TKQ(1,9 ) =   PETA * CS(3)
      TKQ(1,10) =   PETA * CL(3)
      TKQ(1,11) =   PETA * CU(3)
      TKQ(1,12) =   PETA * CS(3)
      TKQ(2,1 ) =   MQSI * CL(4)
      TKQ(2,2 ) =   MQSI * CU(4)
      TKQ(2,3 ) =   MQSI * CS(4)
      TKQ(2,4 ) = - PQSI * CL(2)
      TKQ(2,5 ) =   PQSI * CU(2)
      TKQ(2,6 ) =   PQSI * CS(2)
      TKQ(2,7 ) =   PQSI * CL(2)
      TKQ(2,8 ) =   PQSI * CU(2)
      TKQ(2,9 ) =   PQSI * CS(2)
      TKQ(2,10) = - MQSI * CL(4)
      TKQ(2,11) =   MQSI * CU(4)
      TKQ(2,12) =   MQSI * CS(4)
      TKQ(3,1 ) =   QSI * CL(1) - ETA * CL(4)
      TKQ(3,2 ) = - QSI * CU(1) - ETA * CU(4) + 0.25D0
      TKQ(3,3 ) = - QSI * CS(1) - ETA * CS(4)
      TKQ(3,4 ) = - QSI * CL(1) - ETA * CL(2)
      TKQ(3,5 ) = - QSI * CU(1) + ETA * CU(2) - 0.25D0
      TKQ(3,6 ) = - QSI * CS(1) + ETA * CS(2)
      TKQ(3,7 ) = - QSI * CL(3) + ETA * CL(2)
      TKQ(3,8 ) =   QSI * CU(3) + ETA * CU(2) + 0.25D0
      TKQ(3,9 ) =   QSI * CS(3) + ETA * CS(2)
      TKQ(3,10) =   QSI * CL(3) + ETA * CL(4)
      TKQ(3,11) =   QSI * CU(3) - ETA * CU(4) - 0.25D0
      TKQ(3,12) =   QSI * CS(3) - ETA * CS(4)
      TKQ(4,1 ) = - META * SL(1)
      TKQ(4,2 ) =   META * CS(1)
      TKQ(4,3 ) =   META * SU(1)
      TKQ(4,4 ) =   META * SL(1)
      TKQ(4,5 ) =   META * CS(1)
      TKQ(4,6 ) =   META * SU(1)
      TKQ(4,7 ) = - PETA * SL(3)
      TKQ(4,8 ) =   PETA * CS(3)
      TKQ(4,9 ) =   PETA * SU(3)
      TKQ(4,10) =   PETA * SL(3)
      TKQ(4,11) =   PETA * CS(3)
      TKQ(4,12) =   PETA * SU(3)
      TKQ(5,1 ) =   MQSI * SL(4)
      TKQ(5,2 ) =   MQSI * CS(4)
      TKQ(5,3 ) =   MQSI * SU(4)
      TKQ(5,4 ) = - PQSI * SL(2)
      TKQ(5,5 ) =   PQSI * CS(2)
      TKQ(5,6 ) =   PQSI * SU(2)
      TKQ(5,7 ) =   PQSI * SL(2)
      TKQ(5,8 ) =   PQSI * CS(2)
      TKQ(5,9 ) =   PQSI * SU(2)
      TKQ(5,10) = - MQSI * SL(4)
      TKQ(5,11) =   MQSI * CS(4)
      TKQ(5,12) =   MQSI * SU(4)
      TKQ(6,1 ) =   QSI * SL(1) - ETA * SL(4)
      TKQ(6,2 ) = - QSI * CS(1) - ETA * CS(4)
      TKQ(6,3 ) = - QSI * SU(1) - ETA * SU(4) + 0.25D0
      TKQ(6,4 ) = - QSI * SL(1) - ETA * SL(2)
      TKQ(6,5 ) = - QSI * CS(1) + ETA * CS(2)
      TKQ(6,6 ) = - QSI * SU(1) + ETA * SU(2) - 0.25D0
      TKQ(6,7 ) = - QSI * SL(3) + ETA * SL(2)
      TKQ(6,8 ) =   QSI * CS(3) + ETA * CS(2)
      TKQ(6,9 ) =   QSI * SU(3) + ETA * SU(2) + 0.25D0
      TKQ(6,10) =   QSI * SL(3) + ETA * SL(4)
      TKQ(6,11) =   QSI * CS(3) - ETA * CS(4)
      TKQ(6,12) =   QSI * SU(3) - ETA * SU(4) - 0.25D0
C
C     ------ VT = HFT2.TKQ.DEPF ------------------------------------
      DO 115 I = 1, 2
      DO 115 J = 1, 12
         BC(I,J) = 0.D0
  115 CONTINUE
      DO 130 I = 1, 2
         DO 120 J = 1, 3*NNO
            DO 110 K = 1, 6
               BC(I,J) = BC(I,J) + HFT2(I,K) * TKQ(K,J)
  110       CONTINUE
  120    CONTINUE
  130 CONTINUE
      VT(1) = 0.D0
      VT(2) = 0.D0
      DO 150 I = 1, 2
         DO 140 J = 1, 3*NNO
            VT(I) = VT(I) + BC(I,J) * DEPF(J)
  140    CONTINUE
  150 CONTINUE
C
      END

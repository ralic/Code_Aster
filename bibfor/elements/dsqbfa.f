      SUBROUTINE DSQBFA ( QSI, ETA , JACOB , CARAQ4 , BFA )
      IMPLICIT  NONE
      REAL*8    QSI, ETA, JACOB(*), CARAQ4(*), BFA(3,4)
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
C     -----------------------------------------------------
C     MATRICE BFA(3,4) AU POINT QSI ETA POUR L'ELEMENT DSQ
C     -----------------------------------------------------
      REAL*8  VJ11 , VJ12 , VJ21 , VJ22
      REAL*8  PETA, META, PQSI, MQSI, ETAC, QSIC
      REAL*8  C5,C6,C7,C8,S5,S6,S7,S8,PX1,PX2,PX3,PX4,PY1,PY2,PY3,PY4
C     ------------------------------------------------------------------
      VJ11 = JACOB(1)
      VJ12 = JACOB(2)
      VJ21 = JACOB(3)
      VJ22 = JACOB(4)
      C5   = CARAQ4(13)
      C6   = CARAQ4(14)
      C7   = CARAQ4(15)
      C8   = CARAQ4(16)
      S5   = CARAQ4(17)
      S6   = CARAQ4(18)
      S7   = CARAQ4(19)
      S8   = CARAQ4(20)
C
      PETA =  1.D0 + ETA
      META =  1.D0 - ETA
      PQSI =  1.D0 + QSI
      MQSI =  1.D0 - QSI
      ETAC = (1.D0 - ETA * ETA) / 2.D0
      QSIC = (1.D0 - QSI * QSI) / 2.D0
C
      PX1 = - QSI * META * VJ11 - QSIC * VJ12
      PX2 = - ETA * PQSI * VJ12 + ETAC * VJ11
      PX3 = - QSI * PETA * VJ11 + QSIC * VJ12
      PX4 = - ETA * MQSI * VJ12 - ETAC * VJ11
      PY1 = - QSI * META * VJ21 - QSIC * VJ22
      PY2 = - ETA * PQSI * VJ22 + ETAC * VJ21
      PY3 = - QSI * PETA * VJ21 + QSIC * VJ22
      PY4 = - ETA * MQSI * VJ22 - ETAC * VJ21
C
      BFA(1,1) = PX1 * C5
      BFA(1,2) = PX2 * C6
      BFA(1,3) = PX3 * C7
      BFA(1,4) = PX4 * C8
      BFA(2,1) = PY1 * S5
      BFA(2,2) = PY2 * S6
      BFA(2,3) = PY3 * S7
      BFA(2,4) = PY4 * S8
      BFA(3,1) = PY1 * C5 + PX1 * S5
      BFA(3,2) = PY2 * C6 + PX2 * S6
      BFA(3,3) = PY3 * C7 + PX3 * S7
      BFA(3,4) = PY4 * C8 + PX4 * S8
C
      END

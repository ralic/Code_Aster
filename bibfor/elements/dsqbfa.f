      SUBROUTINE DSQBFA ( QSI, ETA , R , BFA )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/04/2000   AUTEUR CIBHHGB G.BERTRAND 
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
      REAL*8   QSI,ETA
      REAL*8   R(*)
      REAL*8   BFA(3,4)
C     -----------------------------------------------------
C     MATRICE BFA(3,4) AU POINT QSI ETA POUR L'ELEMENT DSQ
C     -----------------------------------------------------
C
      REAL*8  VJ11 , VJ12 , VJ21 , VJ22
      REAL*8  PETA,META,PQSI,MQSI,ETAC,QSIC
      REAL*8  C5,C6,C7,C8 , S5,S6,S7,S8
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
      END

      SUBROUTINE DKTNIW ( QSI, ETA, CARAT3, WKT )
      IMPLICIT  NONE
      REAL*8    QSI, ETA, CARAT3(*), WKT(9)
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
C     FONCTIONS D'INTERPOLATION DE LA FLECHE POUR L'ELEMENT DKT
C     ------------------------------------------------------------------
      REAL*8  LBD , X4,X6 , Y4,Y6 , N(9)
C     ------------------------------------------------------------------
C
      X4  = CARAT3(1)
      X6  = CARAT3(3)
      Y4  = CARAT3(4)
      Y6  = CARAT3(6)
C
      LBD = 1.D0 - QSI - ETA
C
C ----- FONCTIONS D'INTERPOLATION DANS LE REPERE REDUIT ------------
      N(1) = LBD * LBD * (3.D0 - 2.D0*LBD) + QSI * ETA * LBD * 2.D0
      N(2) = LBD * LBD *       QSI         + QSI * ETA * LBD / 2.D0
      N(3) = LBD * LBD *       ETA         + QSI * ETA * LBD / 2.D0
      N(4) = QSI * QSI * (3.D0 - 2.D0*QSI) + QSI * ETA * LBD * 2.D0
      N(5) = QSI * QSI *   (-1.D0 + QSI)   - QSI * ETA * LBD
      N(6) = QSI * QSI *       ETA         + QSI * ETA * LBD / 2.D0
      N(7) = ETA * ETA * (3.D0 - 2.D0*ETA) + QSI * ETA * LBD * 2.D0
      N(8) = ETA * ETA *       QSI         + QSI * ETA * LBD / 2.D0
      N(9) = ETA * ETA *   (-1.D0 + ETA)   - QSI * ETA * LBD
C ----- FONCTIONS D'INTERPOLATION DANS LE REPERE LOCAL -------------
      WKT(1) = N(1)
      WKT(2) = - X4 * N(2) + X6 * N(3)
      WKT(3) = - Y4 * N(2) + Y6 * N(3)
      WKT(4) = N(4)
      WKT(5) = - X4 * N(5) + X6 * N(6)
      WKT(6) = - Y4 * N(5) + Y6 * N(6)
      WKT(7) = N(7)
      WKT(8) = - X4 * N(8) + X6 * N(9)
      WKT(9) = - Y4 * N(8) + Y6 * N(9)
C
      END

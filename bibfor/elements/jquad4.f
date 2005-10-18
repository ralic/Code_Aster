      SUBROUTINE JQUAD4 ( XYZL , QSI, ETA, JACOB )
      IMPLICIT  NONE
      REAL*8    XYZL(3,*), QSI, ETA, JACOB(*)
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
C     JACOBIEN ET LA MATRICE INVERSE AU POINT 'INT' SUR LE QUAD4
C     ------------------------------------------------------------------
      REAL*8  X21 , X32 , X43 , X14 , Y21 , Y32 , Y43 , Y14
      REAL*8  J11 , J12 , J21 , J22
C     ------------------------------------------------------------------
C     -------- PROJECTION DES COTES ------------------------------------
      X21 = XYZL(1,2) - XYZL(1,1)
      X32 = XYZL(1,3) - XYZL(1,2)
      X43 = XYZL(1,4) - XYZL(1,3)
      X14 = XYZL(1,1) - XYZL(1,4)
      Y21 = XYZL(2,2) - XYZL(2,1)
      Y32 = XYZL(2,3) - XYZL(2,2)
      Y43 = XYZL(2,4) - XYZL(2,3)
      Y14 = XYZL(2,1) - XYZL(2,4)
C     ----------- MATRICE JACOBIENNE ----------------------------------
      J11 = (X21 - X43 - ETA * (X43 + X21)) / 4.D0
      J12 = (Y21 - Y43 - ETA * (Y43 + Y21)) / 4.D0
      J21 = (X32 - X14 + QSI * (X32 + X14)) / 4.D0
      J22 = (Y32 - Y14 + QSI * (Y32 + Y14)) / 4.D0
C     -------------- JACOBIEN -----------------------------------------
      JACOB(1)  = J11 * J22 - J12 * J21
C     ------- MATRICE JACOBIENNE INVERSE ------------------------------
      JACOB(2)  =   J22 / JACOB(1)
      JACOB(3)  = - J12 / JACOB(1)
      JACOB(4)  = - J21 / JACOB(1)
      JACOB(5)  =   J11 / JACOB(1)
C
      END

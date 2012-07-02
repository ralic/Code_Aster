      SUBROUTINE I2CHAX (X1,Y1,X2,Y2,X3,Y3,XM,YM,XN,YN,
     +                   C,XNM,YNM,XNN,YNN)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C*****************************************************************
C
C         CALCUL DES AXES DANS LESQUELS LA PARABOLE
C         PASSANT PAR LES (XI,YI) EST REPRESENTEE PAR
C         LA COURBE D' EQUATION Y = C X**2
C
C         PUIS, CALCUL DES COORDONNEES DES POINTS M, N, ET P
C         DANS CE NOUVEAU SYSTEME D' AXES.
C
C*****************************************************************
C
      REAL*8 X1,Y1,X2,Y2,X3,Y3,XM,YM,XN,YN
      REAL*8 C,XNM,YNM,XNN,YNN
C
      REAL*8 A2,A1,A0,B2,B1,B0
      REAL*8 C1,C0,D2,D1,D0
      REAL*8 INVC1,INVC12,INVALF
      REAL*8 ALFA,BETA,GAMA
      REAL*8 TX,TY
      REAL*8 U,V
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      U = 0.0D0
      V = 0.0D0
C
      XNM = 0.0D0
      YNM = 0.0D0
      XNN = 0.0D0
      YNN = 0.0D0
C
      A2 =    X1 - 2*X2 + X3
      A2 =  2*A2
      A1 = -3*X1 + 4*X2 - X3
      A0 =    X1
      B2 =    Y1 - 2*Y2 + Y3
      B2 =  2*B2
      B1 = -3*Y1 + 4*Y2 - Y3
      B0 =    Y1
C
      C0 = A0*B2 - A2*B0
      C1 = A1*B2 - A2*B1
      D0 = A2*A0 + B2*B0
      D1 = A2*A1 + B2*B1
      D2 = A2*A2 + B2*B2
C
      INVC1  = 1.0D0/C1
      INVC12 = INVC1*INVC1
C
      ALFA = D2 *INVC12
      BETA = D1*INVC1
      GAMA = D0 - C0*BETA
      BETA = BETA - 2.0D0*C0*ALFA
      GAMA = GAMA + C0*C0*ALFA
C
      C = ALFA
C
      INVALF = 1.0D0/ALFA
C
      TX = 0.5D0*BETA*INVALF
      TY = 0.5D0*BETA*TX - GAMA
C
C_____________TRAITEMENT DE M--------------------------------
C
      U = B2*XM - A2*YM
      V = A2*XM + B2*YM
C
      XNM = U + TX
      YNM = V + TY
C_____________TRAITEMENT DE N--------------------------------
C
      U = B2*XN - A2*YN
      V = A2*XN + B2*YN
C
      XNN = U + TX
      YNN = V + TY
C
      END

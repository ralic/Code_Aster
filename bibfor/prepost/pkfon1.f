      SUBROUTINE PKFON1 ( COOR, VECNOR, NO1, NO2, NO3 )
      IMPLICIT   NONE
      INTEGER             NO1, NO2, NO3
      REAL*8              COOR(*), VECNOR(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 23/08/2000   AUTEUR CIBHHLV L.VIVAN 
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
C
C     OPERATEUR POST_K1_K2_K3 : TRAITEMENT DU MOT CLE "FOND_3D"
C     ------------------------------------------------------------------
      REAL*8       D, X1, X2, Y1, Y2, Z1, Z2
C DEB ------------------------------------------------------------------
C
      X1 = COOR(3*(NO1-1)+1)
      Y1 = COOR(3*(NO1-1)+2)
      Z1 = COOR(3*(NO1-1)+3)
C
      X2 = COOR(3*(NO2-1)+1)
      Y2 = COOR(3*(NO2-1)+2)
      Z2 = COOR(3*(NO2-1)+3)
C
      D = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
C
      VECNOR(1) = (X2-X1) / D
      VECNOR(2) = (Y2-Y1) / D
      VECNOR(3) = (Z2-Z1) / D
C
      X1 = COOR(3*(NO2-1)+1)
      Y1 = COOR(3*(NO2-1)+2)
      Z1 = COOR(3*(NO2-1)+3)
C
      X2 = COOR(3*(NO3-1)+1)
      Y2 = COOR(3*(NO3-1)+2)
      Z2 = COOR(3*(NO3-1)+3)
C
      D = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
C
      VECNOR(1) = ( VECNOR(1) + ( (X2-X1) / D )  ) / 2
      VECNOR(2) = ( VECNOR(2) + ( (Y2-Y1) / D )  ) / 2
      VECNOR(3) = ( VECNOR(3) + ( (Z2-Z1) / D )  ) / 2
C
      END

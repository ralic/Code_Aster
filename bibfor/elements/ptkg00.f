      SUBROUTINE PTKG00(ITERM,SF,A1,A2,XIZ,XIZ2,XIY,XIY2,XL,EY,EZ,DSM)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           ITERM
      REAL*8                     A1,A2,XIZ,XIZ2,XIY,XIY2,XL,EY,EZ
      REAL*8                  SF(*),                        DSM(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 24/01/96   AUTEUR JMBHH01 J.M.PROIX 
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
C     CALCUL DE LA MATRICE DE RAIDEUR GEOMETRIQUE  (POUTRE)
C     ------------------------------------------------------------------
C IN  ITERM  - INDICATEUR
C           0 : RIGIDITE GEOMETRIQUE
C           1 : FLAMBEMENT D'EULER
C IN  SF     - (12) COMPOSANTES EFFORTS STATIQUES DANS LES ELEMENTS
C IN  A1         - AIRE DE LA SECTION DROITE INITIALE
C IN  A2         - AIRE DE LA SECTION DROITE FINALE
C IN  XIZ        - MOMENT D INERTIE / Z PRINCIPAL  SECTION INITIALE
C IN  XIZ2       - MOMENT D INERTIE / Z PRINCIPAL  SECTION FINALE
C IN  XIY        - MOMENT D INERTIE / Y PRINCIPAL  SECTION INITIALE
C IN  XIY2       - MOMENT D INERTIE / Y PRINCIPAL  SECTION FINALE
C IN  XL         - LONGUEUR DE L ELEMENT
C IN  EY         - COMPOSANTE SUIVANT Y PRINCIPAL DE GT.
C IN  EZ         - COMPOSANTE SUIVANT Z PRINCIPAL DE GT.
C OUT DSM    - (78) MATRICE DE RIGIDITE GEOMETRIQUE
C     ------------------------------------------------------------------
      REAL*8  Z1,UN2,DEUX,SIX,QUINZE,TRENTE
      REAL*8  FX, A, XIA1, XIA2, XIA, MY1, MY2, MZ1, MZ2
      INTEGER IP(12)
      DATA             IP/ 0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66 /
C     ------------------------------------------------------------------
      Z1     =  0.1D0
      UN2    =  1.2D0
      DEUX   =  2.D0
      SIX    =  6.D0
      QUINZE = 15.D0
      TRENTE = 30.D0
C
      MY1=SF(5)
      MZ1=SF(6)
      MY2=SF(11)
      MZ2=SF(12)
C
C     --- TRIANGULAIRE SUPERIEURE DE LA MATRICE ---
C
      FX   = (SF(7)+SF(1))/DEUX
      A    =  A1+A2
      XIA1 = (XIZ+XIZ2)/A
      XIA2 = (XIY+XIY2)/A
      XIA  =  XIA1+XIA2
      DSM(2+IP(2))   =  UN2*FX/XL
      DSM(2+IP(4))   = -SF(5)/XL
      DSM(2+IP(6))   =  Z1*FX
      DSM(2+IP(8))   = -DSM(2+IP(2))
      DSM(2+IP(10))  =  SF(11)/XL
      DSM(2+IP(12))  =  DSM(2+IP(6))
      DSM(3+IP(3))   =  DSM(2+IP(2))
      DSM(3+IP(4))   = -SF(6)/XL
      DSM(3+IP(5))   = -DSM(2+IP(6))
      DSM(3+IP(9))   = -DSM(2+IP(2))
      DSM(3+IP(10))  =  SF(12)/XL
      DSM(3+IP(11))  =  DSM(3+IP(5))
      DSM(4+IP(4))   =  XIA*FX/XL
      DSM(4+IP(5))   = -XL*SF(8)/SIX
      DSM(4+IP(6))   = -XL*SF(9)/SIX
      DSM(4+IP(8))   = -DSM(2+IP(4))
      DSM(4+IP(9))   = -DSM(3+IP(4))
      DSM(4+IP(10))  = -DSM(4+IP(4))
      DSM(4+IP(11))  = -DSM(4+IP(5))
      DSM(4+IP(12))  = -DSM(4+IP(6))
      DSM(5+IP(5))   =  DEUX*XL*FX/QUINZE
      DSM(5+IP(9))   =  DSM(2+IP(6))
      DSM(5+IP(10))  =  DSM(4+IP(11))
      DSM(5+IP(11))  = -XL*FX/TRENTE
      DSM(6+IP(6))   =  DSM(5+IP(5))
      DSM(6+IP(8))   =  DSM(3+IP(5))
      DSM(6+IP(10))  =  DSM(4+IP(12))
      DSM(6+IP(12))  =  DSM(5+IP(11))
      DSM(8+IP(8))   =  DSM(2+IP(2))
      DSM(8+IP(10))  = -DSM(2+IP(10))
      DSM(8+IP(12))  =  DSM(3+IP(5))
      DSM(9+IP(9))   =  DSM(2+IP(2))
      DSM(9+IP(10))  = -DSM(3+IP(10))
      DSM(9+IP(11))  =  DSM(2+IP(6))
      DSM(10+IP(10)) =   DSM(4+IP(4))
      DSM(10+IP(11)) =   DSM(4+IP(5))
      DSM(10+IP(12)) =   DSM(4+IP(6))
      DSM(11+IP(11)) =   DSM(5+IP(5))
      DSM(12+IP(12)) =   DSM(5+IP(5))
C
C     TERMES DUS AU ROTATIONS MODEREES
C
      DSM(4 +IP(5 )) = DSM(4 +IP(5 )) + MZ1/DEUX
      DSM(10+IP(11)) = DSM(10+IP(11)) - MZ2/DEUX
C
      DSM(4 +IP(6 )) = DSM(4 +IP(6 )) - MY1/DEUX
      DSM(10+IP(12)) = DSM(10+IP(12)) + MY2/DEUX
C
C     TERMES DUS A L'EXCENTRICITE DU CENTRE DE TORSION
C
      DSM(2 +IP(4 )) = DSM(2 +IP(4 )) + FX * EZ / XL
      DSM(2 +IP(10)) = DSM(2 +IP(10)) - FX * EZ / XL
      DSM(3 +IP(4 )) = DSM(3 +IP(4 )) - FX * EY / XL
      DSM(3 +IP(10)) = DSM(3 +IP(10)) + FX * EY / XL
      DSM(4 +IP(8 )) = DSM(4 +IP(8 )) - FX * EZ / XL
      DSM(4 +IP(9 )) = DSM(4 +IP(9 )) + FX * EY / XL
      DSM(8 +IP(10)) = DSM(8 +IP(10)) + FX * EZ / XL
      DSM(9 +IP(10)) = DSM(9 +IP(10)) - FX * EY / XL
C
      DSM(4 +IP(4 )) = DSM(4 +IP(4 ))+DEUX*MZ1*EY/XL+FX*EY*EY/XL
     &                               -DEUX*MY1*EZ/XL+FX*EZ*EZ/XL
C    &                               -DEUX*MY2*EZ/XL+FX*EZ*EZ/XL
      DSM(4 +IP(10)) = DSM(4 +IP(10))-(MZ1+MZ2)*EY/XL-FX*EY*EY/XL
     &                               +(MY1+MY2)*EZ/XL-FX*EZ*EZ/XL
      DSM(10+IP(10)) = DSM(10+IP(10))+DEUX*MZ2*EY/XL+FX*EY*EY/XL
     &                               -DEUX*MY2*EZ/XL+FX*EZ*EZ/XL
C    &                               -DEUX*MY1*EZ/XL+FX*EZ*EZ/XL
C
C     IL FAUT BIEN REPASSER DANS LE REPERE G,X,Y,Z
C        TERME INDUIT PAR L'EXCENTRICITE
C
      DSM(IP(4)+4)=DSM(IP(4)+4)+EZ*EZ*DSM(IP(2)+2)+EY*EY*DSM(IP(3)+3)
     &             -DEUX*EZ*DSM(IP(4)+2) + DEUX*EY*DSM(IP(4)+3)
      DSM(IP(4)+2) = DSM(IP(4)+2) - EZ*DSM(IP(2)+2)
      DSM(IP(4)+3) = DSM(IP(4)+3) + EY*DSM(IP(3)+3)
      DSM(IP(5)+4) = DSM(IP(5)+4) + EY*DSM(IP(5)+3)
      DSM(IP(6)+4) = DSM(IP(6)+4) - EZ*DSM(IP(6)+2)

      DSM(IP(10)+10)=DSM(IP(10)+10)
     &              +EZ*EZ*DSM(IP(8)+8)+EY*EY*DSM(IP(9)+9)
     &             -DEUX*EZ*DSM(IP(10)+8) + DEUX*EY*DSM(IP(10)+9)
      DSM(IP(10)+8) = DSM(IP(10)+8) - EZ*DSM(IP(8)+8)
      DSM(IP(10)+9) = DSM(IP(10)+9) + EY*DSM(IP(9)+9)
      DSM(IP(11)+10) = DSM(IP(11)+10) + EY*DSM(IP(11)+9)
      DSM(IP(12)+10) = DSM(IP(12)+10) - EZ*DSM(IP(12)+8)

      DSM(IP(10)+4)=DSM(IP(10)+4)
     &              +EZ*EZ*DSM(IP(8)+2)+EY*EY*DSM(IP(9)+3)
     &             - EZ*DSM(IP(10)+2) + EY*DSM(IP(10)+3)
     &             - EZ*DSM(IP(8)+4)  + EY*DSM(IP(9)+4)

      DSM(IP(8 )+ 4) = DSM(IP(8 )+ 4) - EZ*DSM(IP(8 )+2 )
      DSM(IP(9 )+ 4) = DSM(IP(9 )+ 4) + EY*DSM(IP(9 )+3 )
      DSM(IP(10)+ 2) = DSM(IP(10)+ 2) - EZ*DSM(IP(8 )+2 )
      DSM(IP(10)+ 3) = DSM(IP(10)+ 3) + EY*DSM(IP(9 )+3 )

      DSM(IP(10)+ 5) = DSM(IP(10)+ 5) + EY*DSM(IP(9 )+5 )
      DSM(IP(10)+ 6) = DSM(IP(10)+ 6) - EZ*DSM(IP(8 )+6 )
      DSM(IP(11)+ 4) = DSM(IP(11)+ 4) + EY*DSM(IP(11)+3 )
      DSM(IP(12)+ 4) = DSM(IP(12)+ 4) - EZ*DSM(IP(12)+2 )

      END

      SUBROUTINE REFLTH(ANG,LI,LR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C.......................................................................
      IMPLICIT NONE
C .                                                                    .
C .  - FONCTION REALISEE : CALCULE LE PASSAGE DES TERMES DE CONDUCTIVITE
C .                        DU REPERE DE REFERENCE AU REPERE DE L'ELEMENT
C .  - ARGUMENTS :                                                     .
C .                                                                    .
C .      ENTREE :                                                      .
C .                   ANG --> COSINUS ET SINUS DE LA MATRICE DE PASSAGE.
C .                   LI  --> DILATATION ELEMENTAIRE REPERE DE REFERENCE
C .      SORTIE :                                                      .
C .                   LR  <-- DILATATION ELEMENTAIRE REPERE DE L'ELEMENT
C .                                                                    .
C.......................................................................
      REAL*8 ANG(2),LI(3),LR(3)
      REAL*8 F,C,S,C2,S2
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      F=(ANG(1)**2+ANG(2)**2)**0.5D0
      C=ANG(1)/F
      S=ANG(2)/F
      C2=C**2
      S2=S**2
      LR(1)=C2*LI(1)+S2*LI(2)-2.D0*C*S*LI(3)
      LR(2)=S2*LI(1)+C2*LI(2)+2.D0*C*S*LI(3)
      LR(3)=C*S*LI(1)-C*S*LI(2)+(C2-S2)*LI(3)
      END

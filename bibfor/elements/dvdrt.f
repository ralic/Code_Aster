      SUBROUTINE DVDRT ( NPT,ETA,KSI,VF,DFDE,DFDK )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
      INTEGER            NPT
      REAL*8                 ETA,KSI,VF(*),DFDE(*),DFDK(*)
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES FONCTIONS DE FORME ET DE SES
C       RIVEES POUR UN TRIANGLE DE REFERENCE AUETA POINTS DE GAUSS
C
C    - ARGUMENTS:
C        DONNEES:          NPT      -->  NOMBRE DE POINTS DU TRIANGLE
C
C         RESULTATS:
C                          ETA,KSI  <--  COORDONNEES DU POINT
C                          VF       <--  VALEUR DES FONCTIONS
C                          DFDE     <--  DERIVEE  PAR RAPPORT A ETA
C                          DFDK     <--  DERIVEE  PAR RAPPORT A KSI
C
C     VALEUR & DERIVEE SUIVANT ETA ET KSI DES FONCTIONS DE FORM POUR P1
C     OU P2
C
C     DESCRIPTION DE LA NUMEROTATION DU TRIANGLE
C
C    +1              +1               +1
C    +  +            +  +             +  +
C    +    +          +    +           +    +
C    +      +        +4     +6        +4  7  +6
C    +        +      +        +       +        +
C    +2         +3   +2    5    +3    +2    5    +3
C    +++++++++++++   +++++++++++++    +++++++++++++
C ......................................................................
      IF(NPT.EQ.3) THEN
         VF(1)=(1.D0+KSI)/2.D0
         VF(2)=-(ETA+KSI)/2.D0
         VF(3)=(1.D0+ETA)/2.D0
         DFDE(1)=0.D0
         DFDE(2)=-0.5D0
         DFDE(3)=+0.5D0
         DFDK(1)=+0.5D0
         DFDK(2)=-0.5D0
         DFDK(3)=0.D0
      ELSE IF(NPT.EQ.6) THEN
         VF(1)=KSI*(KSI+1.D0)/2.D0
         VF(2)=(ETA+KSI)*(ETA+KSI+1.D0)/2.D0
         VF(3)=ETA*(ETA+1.D0)/2.D0
         VF(4)=-(1.D0+KSI)*(ETA+KSI)
         VF(5)=-(1.D0+ETA)*(ETA+KSI)
         VF(6)=(ETA+1.D0)*(KSI+1.D0)
         DFDE(1)=0.D0
         DFDE(2)=ETA+KSI+0.5D0
         DFDE(3)=ETA+0.5D0
         DFDE(4)=-(1.D0+KSI)
         DFDE(5)=-2.D0*ETA-KSI-1.D0
         DFDE(6)=KSI+1.D0
         DFDK(1)=KSI+0.5D0
         DFDK(2)=ETA+KSI+0.5D0
         DFDK(3)=0.D0
         DFDK(4)=-2.D0*KSI-ETA-1.D0
         DFDK(5)=-(1.D0+ETA)
         DFDK(6)=ETA+1.D0
      ELSE IF(NPT.EQ.7) THEN
         VF(1)=KSI*(KSI+1.D0)/2.D0   - 3.D0*(KSI+1.D0)*
     &                                 (ETA+1.D0)*(ETA+KSI)/8.D0
         VF(2)=(ETA+KSI)*(ETA+KSI+1.D0)/2.D0
     &                           - 3.D0*(KSI+1.D0)*
     &                           (ETA+1.D0)*(ETA+KSI)/8.D0
         VF(3)=ETA*(ETA+1.D0)/2.D0   - 3.D0*(KSI+1.D0)*
     &                           (ETA+1.D0)*(ETA+KSI)/8.D0
         VF(4)=-(1.D0+KSI)*(ETA+KSI) +12.D0*(KSI+1.D0)*
     &                           (ETA+1.D0)*(ETA+KSI)/8.D0
         VF(5)=-(1.D0+ETA)*(ETA+KSI) +12.D0*(KSI+1.D0)*
     &                           (ETA+1.D0)*(ETA+KSI)/8.D0
         VF(6)=(ETA+1.D0)*(KSI+1.D0) +12.D0*(KSI+1.D0)*
     &                           (ETA+1.D0)*(ETA+KSI)/8.D0
         VF(7)=                  -27.D0*(KSI+1.D0)*(ETA+1.D0)*
     &                           (ETA+KSI)/8.D0
         DFDE(1)=0.D0        - 3.D0*(KSI+1.D0)*(2.D0*ETA+KSI+1.D0)/8.D0
         DFDE(2)=ETA+KSI+0.5D0 - 3.D0*(KSI+1.D0)*
     &                           (2.D0*ETA+KSI+1.D0)/8.D0
         DFDE(3)=ETA+0.5D0   - 3.D0*(KSI+1.D0)*(2.D0*ETA+KSI+1.D0)/8.D0
         DFDE(4)=-(1.D0+KSI) +12.D0*(KSI+1.D0)*(2.D0*ETA+KSI+1.D0)/8.D0
         DFDE(5)=-2.D0*ETA-KSI-1.D0 +12.D0*(KSI+1.D0)*
     &                           (2.D0*ETA+KSI+1.D0)/8.D0
         DFDE(6)=KSI+1.D0    +12.D0*(KSI+1.D0)*(2.D0*ETA+KSI+1.D0)/8.D0
         DFDE(7)=          -27.D0*(KSI+1.D0)*(2.D0*ETA+KSI+1.D0)/8.D0
         DFDK(1)=KSI+0.5D0   - 3.D0*(ETA+1.D0)*(2.D0*KSI+ETA+1.D0)/8.D0
         DFDK(2)=ETA+KSI+0.5D0 - 3.D0*(ETA+1.D0)*
     &                           (2.D0*KSI+ETA+1.D0)/8.D0
         DFDK(3)=0.D0        - 3.D0*(ETA+1.D0)*(2.D0*KSI+ETA+1.D0)/8.D0
         DFDK(4)=-2.D0*KSI-ETA-1.D0 +12.D0*(ETA+1.D0)*
     &                           (2.D0*KSI+ETA+1.D0)/8.D0
         DFDK(5)=-(1.D0+ETA) +12.D0*(ETA+1.D0)*(2.D0*KSI+ETA+1.D0)/8.D0
         DFDK(6)=ETA+1.D0    +12.D0*(ETA+1.D0)*(2.D0*KSI+ETA+1.D0)/8.D0
         DFDK(7)=          -27.D0*(ETA+1.D0)*(2.D0*KSI+ETA+1.D0)/8.D0
      ENDIF
      END

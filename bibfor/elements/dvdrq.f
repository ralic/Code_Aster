      SUBROUTINE DVDRQ ( NPT,ETA,KSI,VF,DFDE,DFDK )
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
C          DERIVEES POUR UN QUADRANGLE DE REFERENCE AUX POINTS DE GAUSS
C
C    - ARGUMENTS:
C        DONNEES:          NPT      -->  NOMBRE DE POINTS DU QUADRANGLE
C
C        RESULTATS:
C                          ETA,KSI  <--  COORDONNEES DU POINT
C                          VF       <--  VALEUR DES FONCTIONS
C                          DFDE     <--  DERIVEE  PAR RAPPORT A ETA
C                          DFDK     <--  DERIVEE  PAR RAPPORT A KSI
C
C     VALEUR & DERI SUIVANT ETA ET KSI DES FONCTIONS DE FORM POUR LE Q1,
C LE SERENDIP ET LE Q2.
C
C     DESCRIPTION DE LA NUMEROTATION DU RECTANGLE
C
C       +-----------------+        +-----------------+
C       +1              4 +        +1      8       4 +
C       +                 +        +                 +
C       +                 +        +5      9       7 +
C       +                 +        +                 +
C       +2              3 +        +2      6       3 +
C       -------------------        -------------------
C ......................................................................
      IF(NPT.EQ.4) THEN
         VF(1)=(1.D0+KSI)*(1.D0-ETA)/4.D0
         VF(2)=(1.D0-ETA)*(1.D0-KSI)/4.D0
         VF(3)=(1.D0+ETA)*(1.D0-KSI)/4.D0
         VF(4)=(1.D0+ETA)*(1.D0+KSI)/4.D0
         DFDE(1)=-(1.D0+KSI)/4.D0
         DFDE(2)=-(1.D0-KSI)/4.D0
         DFDE(3)=(1.D0-KSI)/4.D0
         DFDE(4)=(1.D0+KSI)/4.D0
         DFDK(1)=(1.D0-ETA)/4.D0
         DFDK(2)=-(1.D0-ETA)/4.D0
         DFDK(3)=-(1.D0+ETA)/4.D0
         DFDK(4)=(1.D0+ETA)/4.D0
      ELSE IF(NPT.EQ.8) THEN
         VF(1)=0.25D0*(1.D0+KSI)*(1.D0-ETA)*( KSI-ETA-1.D0)
         VF(2)=0.25D0*(1.D0-KSI)*(1.D0-ETA)*(-KSI-ETA-1.D0)
         VF(3)=0.25D0*(1.D0-KSI)*(1.D0+ETA)*(-KSI+ETA-1.D0)
         VF(4)=0.25D0*(1.D0+KSI)*(1.D0+ETA)*( KSI+ETA-1.D0)
         VF(5)=0.5D0*(1.D0-ETA)*(1.D0-KSI**2)
         VF(6)=0.5D0*(1.D0-KSI)*(1.D0-ETA**2)
         VF(7)=0.5D0*(1.D0+ETA)*(1.D0-KSI**2)
         VF(8)=0.5D0*(1.D0+KSI)*(1.D0-ETA**2)
         DFDE(1)=0.25D0*(1.D0+KSI)*(-KSI+2.D0*ETA)
         DFDE(2)=0.25D0*(1.D0-KSI)*( KSI+2.D0*ETA)
         DFDE(3)=0.25D0*(1.D0-KSI)*(-KSI+2.D0*ETA)
         DFDE(4)=0.25D0*(1.D0+KSI)*( KSI+2.D0*ETA)
         DFDE(5)=-0.5D0*(1.D0-KSI**2)
         DFDE(6)=-ETA*(1.D0-KSI)
         DFDE(7)= 0.5D0*(1.D0-KSI**2)
         DFDE(8)=-ETA*(1.D0+KSI)
         DFDK(1)=0.25D0*(1.D0-ETA)*(-ETA+2.D0*KSI)
         DFDK(2)=0.25D0*(1.D0-ETA)*( ETA+2.D0*KSI)
         DFDK(3)=0.25D0*(1.D0+ETA)*(-ETA+2.D0*KSI)
         DFDK(4)=0.25D0*(1.D0+ETA)*( ETA+2.D0*KSI)
         DFDK(5)=-KSI*(1.D0-ETA)
         DFDK(6)=-0.5D0*(1.D0-ETA**2)
         DFDK(7)=-KSI*(1.D0+ETA)
         DFDK(8)= 0.5D0*(1.D0-ETA**2)
      ELSE IF(NPT.EQ.9) THEN
         VF(1)=ETA*KSI*(KSI+1.D0)*(ETA-1.D0)/4.D0
         VF(2)=ETA*KSI*(KSI-1.D0)*(ETA-1.D0)/4.D0
         VF(3)=ETA*KSI*(KSI-1.D0)*(ETA+1.D0)/4.D0
         VF(4)=ETA*KSI*(KSI+1.D0)*(ETA+1.D0)/4.D0
         VF(5)=ETA*(KSI-1.D0)*(ETA-1.D0)*(KSI+1.D0)/(-2.D0)
         VF(6)=KSI*(KSI-1.D0)*(ETA+1.D0)*(ETA-1.D0)/(-2.D0)
         VF(7)=ETA*(KSI-1.D0)*(ETA+1.D0)*(KSI+1.D0)/(-2.D0)
         VF(8)=KSI*(KSI+1.D0)*(ETA-1.D0)*(ETA+1.D0)/(-2.D0)
         VF(9)=(KSI+1.D0)*(ETA+1.D0)*(ETA-1.D0)*(KSI-1.D0)
         DFDE(1)=KSI*(KSI+1.D0)*(2.D0*ETA-1.D0)/4.D0
         DFDE(2)=KSI*(KSI-1.D0)*(2.D0*ETA-1.D0)/4.D0
         DFDE(3)=KSI*(KSI-1.D0)*(2.D0*ETA+1.D0)/4.D0
         DFDE(4)=KSI*(KSI+1.D0)*(2.D0*ETA+1.D0)/4.D0
         DFDE(5)=(KSI+1.D0)*(KSI-1.D0)*(2.D0*ETA-1.D0)/(-2.D0)
         DFDE(6)=-KSI*(KSI-1.D0)*ETA
         DFDE(7)=(KSI-1.D0)*(KSI+1.D0)*(2.D0*ETA+1.D0)/(-2.D0)
         DFDE(8)=-KSI*(KSI+1.D0)*ETA
         DFDE(9)=2.D0*ETA*(KSI-1.D0)*(KSI+1.D0)
         DFDK(1)=ETA*(ETA-1.D0)*(2.D0*KSI+1.D0)/4.D0
         DFDK(2)=ETA*(ETA-1.D0)*(2.D0*KSI-1.D0)/4.D0
         DFDK(3)=ETA*(ETA+1.D0)*(2.D0*KSI-1.D0)/4.D0
         DFDK(4)=ETA*(ETA+1.D0)*(2.D0*KSI+1.D0)/4.D0
         DFDK(5)=-(ETA-1.D0)*ETA*KSI
         DFDK(6)=-(ETA-1.D0)*(ETA+1.D0)*(2*KSI-1.D0)/2.D0
         DFDK(7)=-(ETA+1.D0)*ETA*KSI
         DFDK(8)=-(ETA-1.D0)*(ETA+1.D0)*(2*KSI+1.D0)/2.D0
         DFDK(9)=2.D0*KSI*(ETA-1.D0)*(ETA+1.D0)
      ENDIF
      END

      SUBROUTINE PJ2DFF(NOTM,INO,KSI,ETA,VF)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 KSI,ETA,VF
      INTEGER INO
      CHARACTER*8 NOTM
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 10/02/98   AUTEUR VABHHTS J.PELLET 
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
C     BUT :
C       CALCULER LA VALEUR DE LA FONCTION DE FORME ASSOCIEE
C       AU NOEUD INO POUR LE POINT DE COORDONNEES (KSI,ETA)
C       DANS L'ELEMENT DE REFERENCE DE TYPE NOTM
C
C  IN  NOTM    K8 : NOM DU TYPE DE MAILLE : TRIA3/.../QUAD9
C  IN  INO      I : NUMERO DU NOEUD
C  IN  KSI      R : ABSCISSE DU POINT DANS L'ELEMENT DE REFERENCE
C  IN  ETA      R : ORDONNEE DU POINT DANS L'ELEMENT DE REFERENCE
C  OUT VF       R : VALEUR DE LA FONCTION DE FORME
C ----------------------------------------------------------------------


      IF (NOTM.EQ.'TRIA3') THEN
C     -----------------------------------------------
         IF (INO.EQ.1) VF=(1.D0+ETA)/2.D0
         IF (INO.EQ.2) VF=-(KSI+ETA)/2.D0
         IF (INO.EQ.3) VF=(1.D0+KSI)/2.D0


      ELSE IF (NOTM.EQ.'TRIA6') THEN
C     -----------------------------------------------
         IF (INO.EQ.1) VF=ETA*(ETA+1.D0)/2.D0
         IF (INO.EQ.2) VF=(KSI+ETA)*(KSI+ETA+1.D0)/2.D0
         IF (INO.EQ.3) VF=KSI*(KSI+1.D0)/2.D0
         IF (INO.EQ.4) VF=-(1.D0+ETA)*(KSI+ETA)
         IF (INO.EQ.5) VF=-(1.D0+KSI)*(KSI+ETA)
         IF (INO.EQ.6) VF=(KSI+1.D0)*(ETA+1.D0)


      ELSE IF (NOTM.EQ.'QUAD4') THEN
C     -----------------------------------------------
        IF (INO.EQ.1)  VF=(1.D0+ETA)*(1.D0-KSI)/4.D0
        IF (INO.EQ.2)  VF=(1.D0-KSI)*(1.D0-ETA)/4.D0
        IF (INO.EQ.3)  VF=(1.D0+KSI)*(1.D0-ETA)/4.D0
        IF (INO.EQ.4)  VF=(1.D0+KSI)*(1.D0+ETA)/4.D0


      ELSE IF (NOTM.EQ.'QUAD8') THEN
C     -----------------------------------------------
        IF (INO.EQ.1)  VF=0.25D0*(1.D0+ETA)*(1.D0-KSI)*( ETA-KSI-1.D0)
        IF (INO.EQ.2)  VF=0.25D0*(1.D0-ETA)*(1.D0-KSI)*(-ETA-KSI-1.D0)
        IF (INO.EQ.3)  VF=0.25D0*(1.D0-ETA)*(1.D0+KSI)*(-ETA+KSI-1.D0)
        IF (INO.EQ.4)  VF=0.25D0*(1.D0+ETA)*(1.D0+KSI)*( ETA+KSI-1.D0)
        IF (INO.EQ.5)  VF=0.5D0*(1.D0-KSI)*(1.D0-ETA**2)
        IF (INO.EQ.6)  VF=0.5D0*(1.D0-ETA)*(1.D0-KSI**2)
        IF (INO.EQ.7)  VF=0.5D0*(1.D0+KSI)*(1.D0-ETA**2)
        IF (INO.EQ.8)  VF=0.5D0*(1.D0+ETA)*(1.D0-KSI**2)


      ELSE IF (NOTM.EQ.'QUAD9') THEN
C     -----------------------------------------------
        IF (INO.EQ.1)  VF=KSI*ETA*(ETA+1.D0)*(KSI-1.D0)/4.D0
        IF (INO.EQ.2)  VF=KSI*ETA*(ETA-1.D0)*(KSI-1.D0)/4.D0
        IF (INO.EQ.3)  VF=KSI*ETA*(ETA-1.D0)*(KSI+1.D0)/4.D0
        IF (INO.EQ.4)  VF=KSI*ETA*(ETA+1.D0)*(KSI+1.D0)/4.D0
        IF (INO.EQ.5)  VF=KSI*(ETA-1.D0)*(KSI-1.D0)*(ETA+1.D0)/(-2.D0)
        IF (INO.EQ.6)  VF=ETA*(ETA-1.D0)*(KSI+1.D0)*(KSI-1.D0)/(-2.D0)
        IF (INO.EQ.7)  VF=KSI*(ETA-1.D0)*(KSI+1.D0)*(ETA+1.D0)/(-2.D0)
        IF (INO.EQ.8)  VF=ETA*(ETA+1.D0)*(KSI-1.D0)*(KSI+1.D0)/(-2.D0)
        IF (INO.EQ.9)  VF=(ETA+1.D0)*(KSI+1.D0)*(KSI-1.D0)*(ETA-1.D0)


      ELSE
        CALL UTMESS('F','PJ2DFF','STOP 1')
      END IF


      END

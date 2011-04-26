      SUBROUTINE  PJ2DA1(INO2,GEOM2,I,GEOM1,TRIA3,
     &              COBAR2,OK)
      IMPLICIT NONE
      REAL*8  COBAR2(3),GEOM1(*),GEOM2(*)
      INTEGER I,TRIA3(*),INO2
      LOGICAL OK
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       DETERMINER SI LE TRIA3 I CONTIENT LE NOEUD INO2
C       SI OUI :
C       DETERMINER LES COORDONNEES BARYCENTRIQUES DE INO2 DANS CE TRIA3
C
C  IN   INO2       I  : NUMERO DU NOEUD DE M2 CHERCHE
C  IN   GEOM2(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M2
C  IN   GEOM1(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M1
C  IN   I          I  : NUMERO DU TRIA3 CANDIDAT
C  IN   TRIA3(*)   I  : OBJET '&&PJXXCO.TRIA3'
C  OUT  COBAR2(3)  R  : COORDONNEES BARYCENTRIQUES DE INO2 DANS I
C  OUT  OK         L  : .TRUE. : INO2 APPARTIENT AU TRIA3 I


C ----------------------------------------------------------------------
      REAL*8 X1,Y1,X2,Y2,X3,Y3,XP,YP
      REAL*8 L1,L2,L3,S
      REAL*8 V2(2),V3(2),P(2)
C DEB ------------------------------------------------------------------
      XP=GEOM2(3*(INO2-1)+1)
      YP=GEOM2(3*(INO2-1)+2)

      X1=GEOM1(3*(TRIA3(1+4*(I-1)+1)-1)+1)
      Y1=GEOM1(3*(TRIA3(1+4*(I-1)+1)-1)+2)
      X2=GEOM1(3*(TRIA3(1+4*(I-1)+2)-1)+1)
      Y2=GEOM1(3*(TRIA3(1+4*(I-1)+2)-1)+2)
      X3=GEOM1(3*(TRIA3(1+4*(I-1)+3)-1)+1)
      Y3=GEOM1(3*(TRIA3(1+4*(I-1)+3)-1)+2)

      V2(1)=X2-X1
      V2(2)=Y2-Y1
      V3(1)=X3-X1
      V3(2)=Y3-Y1
      S=V2(1)*V3(2)-V2(2)*V3(1)
      IF (S.EQ.0) THEN
        OK=.FALSE.
        GO TO 9999
      END IF

      P(1)=XP-X1
      P(2)=YP-Y1
      L3=(V2(1)*P(2)-V2(2)*P(1))/S
      L2=(P(1)*V3(2)-P(2)*V3(1))/S
      L1=1.D0-L2-L3

      IF (      (L1.GE.0.D0).AND.(L1.LE.1.D0)
     &     .AND.(L2.GE.0.D0).AND.(L2.LE.1.D0)
     &     .AND.(L3.GE.0.D0).AND.(L3.LE.1.D0) ) THEN
         OK=.TRUE.
         COBAR2(1)=L1
         COBAR2(2)=L2
         COBAR2(3)=L3
      ELSE
         OK=.FALSE.
      END IF


9999  CONTINUE
      END

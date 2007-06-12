      SUBROUTINE  PJ4DA2(INO2,GEOM2,I,GEOM1,TRIA3,COBARY,D2,SURF)
      IMPLICIT NONE
      REAL*8  COBARY(3),GEOM1(*),GEOM2(*),D2,SURF
      INTEGER INO2,I,TRIA3(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 12/05/99   AUTEUR VABHHTS J.PELLET 
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
C       DETERMINER LA DISTANCE D ENTRE LE NOEUD INO2 ET LE TRIA3 I.
C       DETERMINER LES COORDONNEES BARYCENTRIQUES
C       DU POINT DE I LE PLUS PROCHE DE INO2.
C
C  IN   INO2       I  : NUMERO DU NOEUD DE M2 CHERCHE
C  IN   GEOM2(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M2
C  IN   GEOM1(*)   R  : COORDONNEES DES NOEUDS DU MAILLAGE M1
C  IN   I          I  : NUMERO DU TRIA3 CANDIDAT
C  IN   TRIA3(*)   I  : OBJET '&&PJXXCO.TRIA3'
C  OUT  COBARY(3)  R  : COORDONNEES BARYCENTRIQUES DE INO2 PROJETE SUR I
C  OUT  D2         R  : CARRE DE LA DISTANCE ENTRE I ET INO2
C  OUT  SURF       R  : SURFACE DU TRIA3 I


C ----------------------------------------------------------------------
      INTEGER K
      LOGICAL OK
      REAL*8 R8MAEM ,DP,L1,L2,L3,LA,LB,LC
      REAL*8 A(3),B(3),C(3),M(3),AB(3),AC(3),V(3)
C DEB ------------------------------------------------------------------

      DO 1,K=1,3
        M(K)=GEOM2(3*(INO2-1)+K)
        A(K)=GEOM1(3*(TRIA3(1+4*(I-1)+1)-1)+K)
        B(K)=GEOM1(3*(TRIA3(1+4*(I-1)+2)-1)+K)
        C(K)=GEOM1(3*(TRIA3(1+4*(I-1)+3)-1)+K)
        AB(K)=B(K)-A(K)
        AC(K)=C(K)-A(K)
1     CONTINUE

      D2=R8MAEM()
      DP=R8MAEM()


C     1. ON CHERCHE LE POINT LE PLUS PROCHE A L'INTERIEUR DU TRIA3
C     -------------------------------------------------------------
      CALL  PJ3DA3(M,A,B,C,OK,L1,L2,L3,DP)
      IF ((OK).AND.(DP.LT.D2)) THEN
        D2=DP
        LA=L1
        LB=L2
        LC=L3
      END IF


C     2. ON BOUCLE SUR LES 3 ARRETES DU TRIA3 :
C     -----------------------------------------
      CALL  PJ3DA4(M,A,B,L1,L2,DP)
      IF (DP.LT.D2) THEN
        D2=DP
        LA=L1
        LB=L2
        LC=0.D0
      END IF

      CALL  PJ3DA4(M,B,C,L1,L2,DP)
      IF (DP.LT.D2) THEN
        D2=DP
        LB=L1
        LC=L2
        LA=0.D0
      END IF

      CALL  PJ3DA4(M,A,C,L1,L2,DP)
      IF (DP.LT.D2) THEN
        D2=DP
        LA=L1
        LC=L2
        LB=0.D0
      END IF


C     3. ON CALCULE SURF :
C     --------------------
      V(1)=AB(2)*AC(3)-AB(3)*AC(2)
      V(2)=AB(3)*AC(1)-AB(1)*AC(3)
      V(3)=AB(1)*AC(2)-AB(2)*AC(1)
      SURF=SQRT(V(1)*V(1)+V(2)*V(2)+V(3)*V(3))



      COBARY(1)=LA
      COBARY(2)=LB
      COBARY(3)=LC

      END

      SUBROUTINE  PJ3DA3(M,A,B,C,OK,LA,LB,LC,D2)
      IMPLICIT NONE
      REAL*8  M(3),A(3),B(3),C(3),D2,LA,LB,LC
      LOGICAL OK
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 06/04/98   AUTEUR VABHHTS J.PELLET 
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
C BUT :
C   TROUVER LES COORDONNEES BARYCENTRIQUES (LA,LB,LC) DE LA PROJECTION P
C   D'UN POINT M SUR UN TRIANGLE (A,B,C) .
C
C  IN   M(3)    R : COORDONNEES DE M
C  IN   A(3)    R : COORDONNEES DE A
C  IN   B(3)    R : COORDONNEES DE B
C  IN   C(3)    R : COORDONNEES DE C

C  OUT  OK         L  :/.TRUE.   : P EST INTERIEUR AU TRIANGLE ABC
C                      /.FALSE.  : P EST EXTERIEUR AU TRIANGLE
C                       (SI .FALSE.   D2 N'EST PAS CALCULE)
C  OUT  D2         R  : CARRE DE LA DISTANCE ENTRE M ET P
C  OUT  LA,LB,LC   R  : COORDONNEES BARYCENTRIQUES DE P DANS ABC


C ----------------------------------------------------------------------
      INTEGER K
      REAL*8 DELTA,P(3)
      REAL*8 AB(3),AC(3),AM(3),A11,A22,A12,B1,B2
C DEB ------------------------------------------------------------------
      DO 1,K=1,3
        AB(K)=B(K)-A(K)
        AC(K)=C(K)-A(K)
        AM(K)=M(K)-A(K)
1     CONTINUE

      A11=AB(1)*AB(1)+AB(2)*AB(2)+AB(3)*AB(3)
      A22=AC(1)*AC(1)+AC(2)*AC(2)+AC(3)*AC(3)
      A12=AB(1)*AC(1)+AB(2)*AC(2)+AB(3)*AC(3)

      B1=AB(1)*AM(1)+AB(2)*AM(2)+AB(3)*AM(3)
      B2=AC(1)*AM(1)+AC(2)*AM(2)+AC(3)*AM(3)

      DELTA=A11*A22-A12*A12
      LB=(A22*B1-A12*B2)/DELTA
      LC=(A11*B2-A12*B1)/DELTA
      LA=1.D0-LB-LC

      IF     ((LA.GE.0.D0).AND.(LA.LE.1.D0)
     &   .AND.(LB.GE.0.D0).AND.(LB.LE.1.D0)
     &   .AND.(LC.GE.0.D0).AND.(LC.LE.1.D0)) THEN
        OK=.TRUE.
        DO 2,K=1,3
          P(K)=LA*A(K)+LB*B(K)+LC*C(K)
          P(K)=M(K)-P(K)
2       CONTINUE
        D2=P(1)*P(1)+P(2)*P(2)+P(3)*P(3)
      ELSE
        OK=.FALSE.
      END IF

      END

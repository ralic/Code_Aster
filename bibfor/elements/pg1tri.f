      SUBROUTINE PG1TRI ( NPGT,POG,COG )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 10/03/98   AUTEUR VABHHTS J.PELLET 
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
      INTEGER             NPGT
      REAL*8                   POG(*),COG(2,*)
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES POIDS DE GAUSS ET DES COORDONNEES
C                          DES POINTS DE GAUSS POUR UN TRIANGLE
C
C    - ARGUMENTS:
C        DONNEES:          NPGQ     -->  NOMBRE DE POINTS DE GAUSS
C         RESULTATS:       POG      <--  POIDS DE GAUSS
C                          COG      <--  COORDONNEES DES POINTS DE G.
C ......................................................................
      REAL*8             X,A,B,C,D,P1,P2,P3,T
      T(X)=2.D0*X-1.D0
C
      IF(NPGT.EQ.1) THEN
          POG(1)=2.D0
          COG(1,1)=-1.D0/3D0
          COG(2,1)=COG(1,1)
      ELSE IF(NPGT.EQ.3) THEN
          POG(1)=2.D0/3.D0
          POG(2)=POG(1)
          POG(3)=POG(1)
          COG(1,1)=-POG(1)
          COG(2,1)=1.D0/3.D0
          COG(1,2)=COG(1,1)
          COG(2,2)=COG(1,1)
          COG(1,3)=COG(2,1)
          COG(2,3)=COG(1,1)
      ELSE IF(NPGT.EQ.4) THEN
          POG(1)= 25.D0/24.D0
          POG(2)= 25.D0/24.D0
          POG(3)= 25.D0/24.D0
          POG(4)=-27.D0/24.D0
          COG(1,1)=-3.D0/5.D0
          COG(2,1)= 1.D0/5.D0
          COG(1,2)=-3.D0/5.D0
          COG(2,2)=-3.D0/5.D0
          COG(1,3)= 1.D0/5.D0
          COG(2,3)=-3.D0/5.D0
          COG(1,4)=-1.D0/3.D0
          COG(2,4)=-1.D0/3.D0
      ELSE IF(NPGT.EQ.6) THEN
          P1=0.111690794839005D0
          P2=0.054975871827661D0
          POG(6)=4.D0*P1
          POG(4)=4.D0*P1
          POG(5)=4.D0*P1
          POG(2)=4.D0*P2
          POG(3)=4.D0*P2
          POG(1)=4.D0*P2
          A=0.445948490915965D0
          B=0.091576213509771D0
          COG(1,6)=T(A)
          COG(1,4)=T(1.D0-2.D0*A)
          COG(1,5)=T(A)
          COG(1,2)=T(B)
          COG(1,3)=T(1.D0-2.D0*B)
          COG(1,1)=T(B)
          COG(2,6)=T(A)
          COG(2,4)=T(A)
          COG(2,5)=T(1.D0-2.D0*A)
          COG(2,2)=T(B)
          COG(2,3)=T(B)
          COG(2,1)=T(1.D0-2.D0*B)
      ELSE IF(NPGT.EQ.12) THEN
          P1=0.025422453185103D0
          P2=0.058393137863189D0
          P3=0.041425537809187D0
          POG(1) =4.D0*P1
          POG(2) =4.D0*P1
          POG(3) =4.D0*P1
          POG(4) =4.D0*P2
          POG(5) =4.D0*P2
          POG(6) =4.D0*P2
          POG(7) =4.D0*P3
          POG(8) =4.D0*P3
          POG(9) =4.D0*P3
          POG(10)=4.D0*P3
          POG(11)=4.D0*P3
          POG(12)=4.D0*P3
          A=0.063089014491502D0
          B=0.249286745170910D0
          C=0.310352451033785D0
          D=0.053145049844816D0
          COG(1,1) =T(A)
          COG(1,2) =T(1.D0-2.D0*A)
          COG(1,3) =T(A)
          COG(1,4) =T(B)
          COG(1,5) =T(1.D0-2.D0*B)
          COG(1,6) =T(B)
          COG(1,7) =T(C)
          COG(1,8) =T(D)
          COG(1,9) =T(1.D0-(C+D))
          COG(1,10)=T(1.D0-(C+D))
          COG(1,11)=T(C)
          COG(1,12)=T(D)
          COG(2,1) =T(A)
          COG(2,2) =T(A)
          COG(2,3) =T(1.D0-2.D0*A)
          COG(2,4) =T(B)
          COG(2,5) =T(B)
          COG(2,6) =T(1.D0-2.D0*B)
          COG(2,7) =T(D)
          COG(2,8) =T(C)
          COG(2,9) =T(C)
          COG(2,10)=T(D)
          COG(2,11)=T(1.D0-(C+D))
          COG(2,12)=T(1.D0-(C+D))
      ENDIF
      END

      SUBROUTINE PG2QUA ( NPGQ,POG,COG )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 17/02/95   AUTEUR D6BHHJP J.P.LEFEBVRE 
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

      INTEGER             NPGQ
      REAL*8                   POG(*),COG(2,*)
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES POIDS DE GAUSS ET DES COORDONNEES
C                          DES POINTS DE GAUSS POUR UN QUADRANGLE
C                            (LES POINTS DE GAUSS SONT LES NOEUDS)
C
C    - ARGUMENTS:
C        DONNEES:          NPGQ     -->  NOMBRE DE POINTS DE GAUSS
C         RESULTATS:       POG      <--  POIDS DE GAUSS
C                          COG      <--  COORDONNEES DES POINTS DE GAUSS
C ......................................................................
C
      IF(NPGQ.EQ.4) THEN
         DO 1 K=1,NPGQ
            POG(K) = 1.D0
1        CONTINUE
         COG(1,1)=-1.D0
         COG(2,1)=-COG(1,1)
         COG(1,2)= COG(1,1)
         COG(2,2)=-COG(2,1)
         COG(1,3)=-COG(1,2)
         COG(2,3)= COG(2,2)
         COG(1,4)= COG(1,3)
         COG(2,4)=-COG(2,3)
      ELSE IF(NPGQ.EQ.8) THEN
         DO 2 K=1,4
            POG(K) = -1.D0/3.D0
2        CONTINUE
         DO 3 K=5,NPGQ
            POG(K) = 4.D0/3.D0
3        CONTINUE
         COG(1,1)=-1.D0
         COG(2,1)=-COG(1,1)
         COG(1,2)= COG(1,1)
         COG(2,2)=-COG(2,1)
         COG(1,3)=-COG(1,2)
         COG(2,3)= COG(2,2)
         COG(1,4)= COG(1,3)
         COG(2,4)=-COG(2,3)
         COG(1,5)= COG(1,1)
         COG(2,5)= 0.D0
         COG(1,6)= 0.D0
         COG(2,6)= COG(2,2)
         COG(1,7)= COG(1,3)
         COG(2,7)= 0.D0
         COG(1,8)= 0.D0
         COG(2,8)= COG(2,4)
      ELSE IF(NPGQ.EQ.9) THEN
         DO 4 K=1,4
            POG(K) = 1.D0/9.D0
4        CONTINUE
         DO 5 K=5,8
            POG(K) = 4.D0/9.D0
5        CONTINUE
         POG(9) = 16.D0/9.D0
         COG(1,1)=-1.D0
         COG(2,1)=-COG(1,1)
         COG(1,2)= COG(1,1)
         COG(2,2)=-COG(2,1)
         COG(1,3)=-COG(1,2)
         COG(2,3)= COG(2,2)
         COG(1,4)= COG(1,3)
         COG(2,4)=-COG(2,3)
         COG(1,5)= COG(1,1)
         COG(2,5)= 0.D0
         COG(1,6)= 0.D0
         COG(2,6)= COG(2,2)
         COG(1,7)= COG(1,3)
         COG(2,7)= 0.D0
         COG(1,8)= 0.D0
         COG(2,8)= COG(2,4)
         COG(1,9)= 0.D0
         COG(2,9)= 0.D0
      ENDIF
      END

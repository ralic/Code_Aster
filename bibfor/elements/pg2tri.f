      SUBROUTINE PG2TRI ( NPGT,POG,COG )
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

      INTEGER            NPGT
      REAL*8                  POG(*),COG(2,*)
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES POIDS DE GAUSS ET DES COORDONNEES
C                          DES POINTS DE GAUSS POUR UN TRIANGLE
C                            (LES POINTS DE GAUSS SONT LES NOEUDS)
C
C    - ARGUMENTS:
C        DONNEES:          NPGQ     -->  NOMBRE DE POINTS DE GAUSS
C         RESULTATS:       POG      <--  POIDS DE GAUSS
C                          COG      <--  COORDONNEES DES POINTS DE GAUSS
C ......................................................................
C
      IF(NPGT.EQ.3) THEN
         DO 1 K=1,NPGT
            POG(K) = 2.D0/3.D0
1        CONTINUE
         COG(1,1)=-1.D0
         COG(2,1)= 1.D0
         COG(1,2)=-1.D0
         COG(2,2)=-1.D0
         COG(1,3)= 1.D0
         COG(2,3)=-1.D0
      ELSE IF(NPGT.EQ.6) THEN
C
C   FORMULE A 3 POINTS (LES 3 MILIEUX)
C
         DO 2 K=1,3
            POG(K) = 0.D0
2        CONTINUE
         DO 3 K=4,NPGT
            POG(K) = 2.D0/3.D0
3        CONTINUE
         COG(1,1)=-1.D0
         COG(2,1)= 1.D0
         COG(1,2)=-1.D0
         COG(2,2)=-1.D0
         COG(1,3)= 1.D0
         COG(2,3)=-1.D0
         COG(1,4)=-1.D0
         COG(2,4)= 0.D0
         COG(1,5)= 0.D0
         COG(2,5)=-1.D0
         COG(1,6)= 0.D0
         COG(2,6)= 0.D0
      ENDIF
      END

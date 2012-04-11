      REAL*8 FUNCTION DIS2NO(GEOM, INO1, INO2)
      IMPLICIT  NONE
      INTEGER          INO1,INO2
      REAL*8           GEOM(*)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 11/04/2012   AUTEUR LADIER A.LADIER 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C
C     BUT : CALCULER LA DISTANCE ENTRE 2 NOEUDS
C           A PARTIR DES NUMEROS DES NOEUDS
C
C IN   GEOM   : VALEURS DES COORDONNEES DES NOEUDS DU MAILLAGE
C IN   INO1   : INDICE DU PREMIER NOEUD
C IN   INO2   : INDICE DU DEUXIEME NOEUDS
C OUT  DIS2NO : DISTANCE ENTRE LES 2 NOEUDS
C
C---------------------------------------------------------------------
C
      REAL*8  A(3),B(3),PADIST
      INTEGER I
C
C---------------------------------------------------------------------
      
      DO 10 I = 1,3
        A(I) = GEOM(3*(INO1-1)+I)
        B(I) = GEOM(3*(INO2-1)+I)
 10   CONTINUE
      DIS2NO = PADIST(3,A,B)
C
      END

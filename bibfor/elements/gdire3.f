          SUBROUTINE GDIRE3(COORD,A,B,C,M)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 19/11/1999   AUTEUR DURAND C.DURAND 
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
C
C ----------------------------------------------------------------
C
C FONCTION REALISEE:
C
C         CALCUL DES COMPOSANTES DU VECTEUR UNITAIRE APPARTENANT
C         AU PLAN TANGENT A L'ELEMENT FORME DE 3 NOEUDS SOMMETS
C         ET QUI EST NORMAL A L'ARETE FORMEE PAR LES NOEUDS 1 ET 2
C-----------------------------------------------------------------
C ENTREE:
C         COORD: COORDONNEES DES 3 NOEUDS
C         M    : 1 SI TRIA
C                2 SI QUAD
C
C SORTIE:
C         A,B,C: COMPOSANTES DU VECTEUR NORME
C
C-----------------------------------------------------------------
C
          REAL*8    COORD(3,*),A,B,C,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,EPS
          REAL*8    X12,Y12,Z12,X13,Y13,Z13,NORM1,NORM2,PSCAL,PROD
C
          INTEGER   M
C
          EPS = 1.D-06
          X1 = COORD(1,1)
          Y1 = COORD(2,1)
          Z1 = COORD(3,1)
          X2 = COORD(1,2)
          Y2 = COORD(2,2)
          Z2 = COORD(3,2)
          IF(M.EQ.1) THEN
             X3 = COORD(1,3)
             Y3 = COORD(2,3)
             Z3 = COORD(3,3)
          ELSE
             X3 = COORD(1,4)
             Y3 = COORD(2,4)
             Z3 = COORD(3,4)
          ENDIF
C
          X12 = X2 - X1
          Y12 = Y2 - Y1
          Z12 = Z2 - Z1
          X13 = X3 - X1
          Y13 = Y3 - Y1
          Z13 = Z3 - Z1
          NORM2 = X12*X12 + Y12*Y12 + Z12*Z12
          PSCAL = X12*X13 + Y12*Y13 + Z12*Z13
          A = X13 - PSCAL*X12/NORM2
          B = Y13 - PSCAL*Y12/NORM2
          C = Z13 - PSCAL*Z12/NORM2
          NORM1 = SQRT(A*A + B*B + C*C)
          A = A/NORM1
          B = B/NORM1
          C = C/NORM1
C
C ORIENTATION DU VECTEUR OBTENU
C
          PROD = X13*A + Y13*B + Z13*C
          IF(PROD.GE.EPS) THEN
             A = -A
             B = -B
             C = -C
          ENDIF
C
      END

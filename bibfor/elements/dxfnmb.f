      SUBROUTINE DXFNMB (NOMTE, X, Y, N)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/11/2003   AUTEUR LEBOUVIE F.LEBOUVIER 
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
      CHARACTER*16 NOMTE
      REAL*8       N(1) 
C     ------------------------------------------------------------------
C --- CALCUL DES FONCTION DE FORME DE MEMBRANE N DES ELEMENTS
C --- TRIANGLE A 3 NOEUDS ET QUADRANGLE A 4 NOEUDS AU POINT (X,Y)
C     ------------------------------------------------------------------
C     IN  NOMTE  : NOM DU TYPE D'ELEMENT
C     IN  X      : ABSCISSE DU POINT DE CALCUL
C     IN  Y      : ORDONNEE DU POINT DE CALCUL
C     OUT N(1)   : FONCTIONS DE FORME MEMBRANE AU POINT (X,Y)
C     ------------------------------------------------------------------
C
C --- INITIALISATIONS :
C     -----------------
      UNQUAR = 0.25D0
      UN     = 1.0D0
C
C
      IF (NOMTE(1:8) .EQ.'MEDKTR3 '.OR.NOMTE(1:8) .EQ.'MEDSTR3 ' .OR.
     +    NOMTE(1:8) .EQ.'MEGRDKT' .OR.NOMTE(1:8).EQ.'MEDKTG3 ') THEN
C
          N(1) = UN - X - Y
          N(2) =      X
          N(3) =          Y
C
      ELSEIF (NOMTE(1:8) .EQ.'MEDKQU4 '.OR.NOMTE(1:8) .EQ.'MEDSQU4 '
     +        .OR.NOMTE(1:8) .EQ.'MEQ4QU4 ' 
     +        .OR.NOMTE(1:8) .EQ.'MEDKQG4 ') THEN
          N(1) = UNQUAR*(UN-X)*(UN-Y) 
          N(2) = UNQUAR*(UN+X)*(UN-Y) 
          N(3) = UNQUAR*(UN+X)*(UN+Y) 
          N(4) = UNQUAR*(UN-X)*(UN+Y) 
C
      ELSE
         CALL UTMESS('F','DXFNMB','LE TYPE D''ELEMENT : '//NOMTE(1:8)
     +               //'N''EST PAS PREVU.')
      ENDIF
C
      END

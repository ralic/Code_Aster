      SUBROUTINE ARLAS1(DD,NN1,NN2,IJ,B0,B)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 02/04/2002   AUTEUR RATEAU G.RATEAU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C ----------------------------------------------------------------------
C ASSEMBLAGE MATRICE ELEMENTAIRE ARLEQUIN MAILLE SOLIDE / MAILLE SOLIDE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C INTEGER           DD          : LONGUEUR MATRICE NOEUD (CF ARLFAC)
C INTEGER           NN1         : NOMBRE DE COLONNES DE B0
C INTEGER           NN2         : NOMBRE DE LIGNES DE B0             
C INTEGER           IJ(NN2,NN1) : POINTEURS DANS B (CF ARLAS0)
C
C VARIABLE D'ENTREE/SORTIE
C REAL*8            B0(NN2,NN1) : MATRICE ELEMENTAIRE ARLEQUIN
C REAL*8            B(*)        : MATRICE ARLEQUIN MORSE (CF ARLFAC)
C ----------------------------------------------------------------------
C                     ATTENTION : B0 EST REMISE A ZERO
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      INTEGER DD,NN1,NN2,IJ(NN2,*),I,J,Q
      REAL*8  B0(NN2,*),B(*)

C --- ASSEMBLAGE

      DO 10 I = 1, NN1

        DO 10 J = 1, NN2

          Q = 1 + DD*(IJ(J,I)-1)
          B(Q) = B(Q) + B0(J,I)
          B0(J,I) = 0.D0

 10   CONTINUE

      END

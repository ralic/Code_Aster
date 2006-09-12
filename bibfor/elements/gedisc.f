      SUBROUTINE GEDISC(NDIM,NNO,NPG,VFF,GEOM,PG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 12/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT NONE
      INTEGER NDIM,NNO,NPG
      REAL*8  VFF(NNO,NPG), GEOM(NDIM,NNO),PG(NDIM,NPG)
C ----------------------------------------------------------------------
C             CALCUL DES COORDONNEES DES POINTS DE GAUSS
C ----------------------------------------------------------------------
C IN  NDIM   DIMENSION DE L'ESPACE
C IN  NNO    NOMBRE DE NOEUDS
C IN  NPG    NOMBRE DE POINTS DE GAUSS
C IN  VFF    VALEUR DES FONCTIONS DE FORME
C IN  GEOM   COORDONNEES DES NOEUDS
C OUT PG     COORDONNEES DES POINTS DE GAUSS
C ----------------------------------------------------------------------
      INTEGER G,I
      REAL*8  DDOT
C ----------------------------------------------------------------------
      DO 10 G = 1,NPG
        DO 20 I = 1,NDIM
          PG(I,G) = DDOT(NNO,GEOM(I,1),NDIM,VFF(1,G),1)
 20     CONTINUE
 10   CONTINUE
      END

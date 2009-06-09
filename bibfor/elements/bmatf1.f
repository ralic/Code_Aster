      SUBROUTINE  BMATF1(NDIM  ,NBSIGM,NNO   ,
     &                   DFDX  ,DFDY  ,DFDZ  ,B     )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/06/2009   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT     NONE
      INTEGER      NDIM,NBSIGM,NNO
      REAL*8       DFDX(NNO),DFDY(NNO),DFDZ(NNO)
      REAL*8       B(NBSIGM,NNO*NDIM)
C
C ----------------------------------------------------------------------
C
C
C CALCUL DE LA MATRICE B RELIANT LES DEFORMATIONS
C U PREMIER ORDRE AUX DEPLACEMENTS AU POINT
C                 D'INTEGRATION D'INDICE IGAU
C
C _DUPLICATA DE BMATMC MAIS AVEC PASSAGE EXPLICITE DU NOM DU TE _
C
C
C ----------------------------------------------------------------------
C
C
C IN  NDIM   : DIMENSION DE L'ESPACE
C IN  NBSIGM : NOMBRE DE CONTRAINTES ASSOCIEES
C IN  NNO    : NOMBRE DE NOEUDS DE L'ELEMENT
C IN  DFDX   : DERIVEE FCT. FORME/X AU POINT DE GAUSS COURANT
C IN  DFDY   : DERIVEE FCT. FORME/Y AU POINT  DE GAUSS COURANT
C IN  DFDZ   : DERIVEE FCT. FORME/Z AU POINT  DE GAUSS COURANT
C OUT B      : MATRICE (B) RELIANT LES DEFORMATIONS DU PREMIER
C              ORDRE AUX DEPLACEMENTS AU POINT D'INTEGRATION IGAU.
C                  TAILLE: (NBSIGM,NNO*NDIM)
C
C
C ----------------------------------------------------------------------
C
      INTEGER      I,J
C
C ----------------------------------------------------------------------
C
C --- CALCUL DE B
C
      IF (NBSIGM.EQ.4) THEN

        DO 10 I = 1, NNO
           J= 2*(I-1) + 1
           B(1,J)   = DFDX(I)
           B(1,J+1) = 0.D0
           B(2,J)   = 0.D0
           B(2,J+1) = DFDY(I)
           B(3,J)   = 0.D0
           B(3,J+1) = 0.D0
           B(4,J)   = SQRT(2.D0)/2.D0*DFDY(I)
           B(4,J+1) = SQRT(2.D0)/2.D0*DFDX(I)
 10     CONTINUE
C
      ELSEIF (NBSIGM.EQ.6) THEN

        DO 20 I = 1, NNO
          J= 3*(I-1) + 1
          B(1,J)   = DFDX(I)
          B(1,J+1) = 0.D0
          B(1,J+2) = 0.D0
          B(2,J)   = 0.D0
          B(2,J+1) = DFDY(I)
          B(2,J+2) = 0.D0
          B(3,J)   = 0.D0
          B(3,J+1) = 0.D0
          B(3,J+2) = DFDZ(I)
          B(4,J)   = SQRT(2.D0)/2.D0*DFDY(I)
          B(4,J+1) = SQRT(2.D0)/2.D0*DFDX(I)
          B(4,J+2) = 0.D0
          B(5,J)   = SQRT(2.D0)/2.D0*DFDZ(I)
          B(5,J+1) = 0.D0
          B(5,J+2) = SQRT(2.D0)/2.D0*DFDX(I)
          B(6,J)   = 0.D0
          B(6,J+1) = SQRT(2.D0)/2.D0*DFDZ(I)
          B(6,J+2) = SQRT(2.D0)/2.D0*DFDY(I)
C
 20      CONTINUE
C
      ENDIF

      END

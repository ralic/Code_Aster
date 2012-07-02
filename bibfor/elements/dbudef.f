      SUBROUTINE DBUDEF (DEPL, B, D, NBSIG, NBINCO, SIGMA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C.======================================================================
      IMPLICIT NONE
C
C      DBUDEF   -- CALCUL DU VECTEUR DES CONTRAINTES AUX POINTS
C                  D'INTEGRATION SUR L'ELEMENT COURANT
C                  EN FAISANT LE PRODUIT D*B*DEPL 
C
C   ARGUMENT        E/S  TYPE         ROLE
C    DEPL(1)        IN     R        VECTEUR DES DEPLACEMENTS SUR 
C                                   L'ELEMENT 
C    B(NBSIG,1)     IN     R        MATRICE (B) RELIANT LES 
C                                   DEFORMATIONS DU PREMIER ORDRE
C                                   AUX DEPLACEMENTS AU POINT
C                                   D'INTEGRATION COURANT
C    D(NBSIG,1)     IN     R        MATRICE DE HOOKE 
C    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
C                                   A L'ELEMENT
C    NBINCO         IN     I        NOMBRE D'INCONNUES DE L'ELEMENT
C    SIGMA(1)       OUT    R        CONTRAINTES AU POINT D'INTEGRATION
C                                   COURANT
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           REAL*8       DEPL(1), B(NBSIG,1), D(NBSIG,1), SIGMA(1)
C -----  VARIABLES LOCALES
           REAL*8       EPS(6)
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATION :
C     ----------------
C-----------------------------------------------------------------------
      INTEGER I ,J ,NBINCO ,NBSIG 
      REAL*8 S ,ZERO 
C-----------------------------------------------------------------------
      ZERO   = 0.0D0
C
C --- CALCUL DU VECTEUR DES COMPOSANTES DU TENSEUR DES DEFORMATIONS
C --- AU POINT D'INTEGRATION COURANT
C      -----------------------------
      DO 10 I = 1, NBSIG 
C
          S = ZERO
C
          DO 20 J = 1, NBINCO
             S = S + DEPL(J)*B(I,J)
  20      CONTINUE
C
          EPS(I) = S
  10  CONTINUE
C
C --- VECTEUR DES CONTRAINTES
C      ----------------------
      DO 30 I = 1, NBSIG 
C
          S = ZERO
C
          DO 40 J = 1, NBSIG
             S = S + EPS(J)*D(I,J)
  40      CONTINUE
C
          SIGMA(I) = S
  30  CONTINUE
C
C.============================ FIN DE LA ROUTINE ======================
      END

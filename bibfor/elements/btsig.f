      SUBROUTINE BTSIG (LONLIG, LONCOL, JACGAU, BMAT, SIGMA, BSIGMA)
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
      IMPLICIT NONE
      REAL*8       JACGAU, BMAT(LONCOL,1), SIGMA(1), BSIGMA(1)
      INTEGER I ,J ,LONCOL ,LONLIG
      REAL*8 VALBSI
C-----------------------------------------------------------------------
C --- CALCUL DU PRODUIT (BT)*(SIGMA) ,
C --- AVEC LES NOTATIONS DE LA ROUTINE , CA DONNE :
C ---       (BSIGMA) = (BMAT)*(SIGMA)*JACGAU
C     ------------------------------------------------------------------
C     IN  LONLIG  : LONGUEUR D'UNE LIGNE DE (B), SOIT NBNO*NBDDL
C     IN  LONCOL  : LONGUEUR D'UNE COLONNE DE (B), SOIT NBSIG
C     IN  JACGAU  : PRODUIT DU JACOBIEN PAR LE POIDS AU POINT
C                   D'INTEGRATION COURANT
C     IN  BMAT    : MATRICE (B) AU POINT D'INTEGRATION COURANT
C     IN  SIGMA   : VECTEUR DES CONTRAINTES AU POINT D'INTEGRATION
C                   COURANT
C     OUT BSIGMA  : VECTEUR (BT)*(SIGMA)*JACGAU
C     ------------------------------------------------------------------
C
      DO 10 I = 1, LONLIG
         VALBSI = 0.0D0
         DO 20 J = 1, LONCOL
            VALBSI = VALBSI + BMAT(J,I)*SIGMA(J)
  20     CONTINUE
C
         BSIGMA(I) = BSIGMA(I) + VALBSI*JACGAU
  10  CONTINUE
C
      END

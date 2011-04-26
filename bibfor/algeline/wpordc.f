      SUBROUTINE WPORDC( TYPE,SHIFT,VP,X,M,NEQ)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER    TYPE, NEQ,M
      COMPLEX*16 X(NEQ,M),SHIFT,VP(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C     TRI DES VALEURS (ET DES VECTEURS) PROPRES COMPLEXES
C     DEUX TYPE DE TRI :
C          - TRI DANS LE SPECTRE : SUIVANT ABS(SHIFT - VPQ)
C          - TRI DE PRESNTATION  : SUIVANT IM(VPQ) - IM(SHIFT)
C     ------------------------------------------------------------------
C IN  TYPE   : IS : TYPE DU TRI PAR ORDRE CROISSANT SUR LES VALEURS.
C                   * SI TYPE = 0  TRI DE PRESENTATION
C                   * SI TYPE = 1  TRI DANS LE SPECTRE
C IN  M      : IS : NOMBRE DE VALEUR PROPRE
C IN  SHIFT  : C8 : DECALAGE SPECTRAL
C VAR VP     : C8 : TABLEAU DES DES VALEURS PROPRES
C VAR X      : C8 : MATRICE DES VECTEURS PROPRES
C IN  NEQ    : IS : NOMBRE D'EQUATIONS
C                 SI NEQ < NBPRO ALORS ON NE TRIE PAS DE VECTEURS
C     ------------------------------------------------------------------
      INTEGER    I,J,K
      REAL*8     P,OM
      COMPLEX*16 C,Q
C
      OM = DIMAG(SHIFT)
      IF ( TYPE .EQ. 0 ) THEN
         DO 100, I = 1, M, 1
            K = I
            P = DIMAG(VP(I)) - OM
            DO 110, J = I+1, M
               IF ( (DIMAG(VP(J))-OM) .LT. P ) THEN
                  P = DIMAG(VP(J)) - OM
                  K = J
               ENDIF
110         CONTINUE
            IF ( K .NE. I ) THEN
               Q=VP(I)
               VP(I)=VP(K)
               VP(K)=Q
               DO 120, J = 1, NEQ, 1
                  C        = X(J,I)
                  X(J,I)   = X(J,K)
                  X(J,K)   = C
120            CONTINUE
            ENDIF
100      CONTINUE
      ELSE IF ( TYPE .EQ. 1 ) THEN
         DO 200, I = 1, M, 1
            K = I
            P = ABS(VP(I) - SHIFT)
            DO 210, J = I+1, M
               IF ( (ABS(VP(J)-SHIFT)) .LT. P ) THEN
                  P = ABS(VP(J) - SHIFT)
                  K = J
               ENDIF
210         CONTINUE
            IF ( K .NE. I ) THEN
               Q=VP(I)
               VP(I)=VP(K)
               VP(K)=Q
              DO 220, J = 1, NEQ, 1
                  C        = X(J,I)
                  X(J,I)   = X(J,K)
                  X(J,K)   = C
220            CONTINUE
            ENDIF
200      CONTINUE
      ELSE
         CALL U2MESS('F','ALGELINE3_97')
      ENDIF
      END

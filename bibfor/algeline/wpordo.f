      SUBROUTINE WPORDO( TYPE,SHIFT,VPR,VPI,X,M,NEQ)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER    TYPE, NEQ,M
      REAL*8     VPR(*),VPI(*)
      COMPLEX*16 X(NEQ,M),SHIFT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C     TRI DES VALEURS (ET DES VECTEURS) PROPRES
C     DEUX TYPE DE TRI :
C          - TRI DANS LE SPECTRE : SUIVANT ABS(SHIFT - VPQ)
C          - TRI DE PRESNTATION  : SUIVANT IM(VPQ) - IM(SHIFT)
C     ------------------------------------------------------------------
C IN  TYPE   : IS : TYPE DU TRI PAR ORDRE CROISSANT SUR LES VALEURS.
C                   * SI TYPE = 0  TRI DE PRESENTATION
C                   * SI TYPE = 1  TRI DANS LE SPECTRE
C IN  M      : IS : NOMBRE DE VALEUR PROPRE
C IN  SHIFT  : C8 : DECALAGE SPECTRAL
C VAR VPR    : R8 : TABLEAU DES PARTIES IMAGINAIRES DES VALEURS PROPRES
C VAR VPI    : R8 : TABLEAU DES PARTIES REELLES     DES VALEURS PROPRES
C VAR X      : C8 : MATRICE DES VECTEURS PROPRES
C IN  NEQ    : IS : NOMBRE D'EQUATIONS
C                 SI NEQ < NBPRO ALORS ON NE TRIE PAS DE VECTEURS
C     ------------------------------------------------------------------
      REAL*8     AIMAG
      INTEGER    I,J,K
      REAL*8     P,OM
      COMPLEX*16 C
C
      OM = DIMAG(SHIFT)
      IF ( TYPE .EQ. 0 ) THEN
         DO 100, I = 1, M, 1
            K = I
            P = VPI(I) - OM
            DO 110, J = I+1, M
               IF ( (VPI(J)-OM) .LT. P ) THEN
                  P = VPI(J) - OM
                  K = J
               ENDIF
110         CONTINUE
            IF ( K .NE. I ) THEN
               P        = VPI(I)
               VPI(I)   = VPI(K)
               VPI(K)   = P
               P        = VPR(I)
               VPR(I)   = VPR(K)
               VPR(K)   = P
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
            P = ABS(DCMPLX(VPR(I),VPI(I)) - SHIFT)
            DO 210, J = I+1, M
               IF ( (ABS(DCMPLX(VPR(J),VPI(J))-SHIFT)) .LT. P ) THEN
                  P = ABS(DCMPLX(VPR(J),VPI(J)) - SHIFT)
                  K = J
               ENDIF
210         CONTINUE
            IF ( K .NE. I ) THEN
               P        = VPI(I)
               VPI(I)   = VPI(K)
               VPI(K)   = P
               P        = VPR(I)
               VPR(I)   = VPR(K)
               VPR(K)   = P
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

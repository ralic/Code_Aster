      SUBROUTINE MTCROU(A,     B,  NMAX,    N,  NBSCMB,L,D )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 29/09/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
      REAL*8  A(NMAX,NMAX),B(NMAX,NBSCMB),L(N,N),D(N)
C     ------------------------------------------------------------------
C     RESOLUTION PAR LA METHODE DE CROUT D'UN SYSTEME LINEAIRE
C     ------------------------------------------------------------------
C VAR A      : R8 : MATRICE CARREE PLEINE
C VAR B      : R8 : TABLEAU BI-INDICES DE REELS
C               EN ENTREE : LES SECONDS MEMBRES
C               EN SORTIE : LES SOLUTIONS
C IN  NMAX   : IS : DIM MAXI DE LA MATRICE
C IN  N      : IS : ORDRE DE LA MATRICE
C IN  NBSCMB : IS : NOMBRE DE SECOND MEMBRE
C     ------------------------------------------------------------------
      REAL*8  ZERO,S
C
      ZERO = 0.D0
      DO 1 I=1,N
        DO 2 J=1,I-1
          S = ZERO
          DO 3 K=1,J-1
          S = S + L(I,K)*D(K)*L(J,K)
3         CONTINUE
          L(I,J) = (A(I,J)-S)/D(J)
2         CONTINUE
          S = ZERO
          DO 4 K=1,I-1
          S = S + L(I,K)*L(I,K)*D(K)
4         CONTINUE
          D(I) = A(I,I)-S
1     CONTINUE
C
C   BOUCLE SUR LES SECONDS MEMBRES
C
      DO 5 IS=1,NBSCMB
C
C  DESCENTE
C
      DO 6 I=1,N
        S = ZERO
        DO 7 K=1,I-1
          S = S + L(I,K)*B(K,IS)
7         CONTINUE
        B(I,IS) = B(I,IS)-S
6     CONTINUE
C
C  DIVISION PAR LA DIAGONALE
C
      DO 10 I=1,N
        B(I,IS) = B(I,IS)/D(I)
10    CONTINUE
C
C  REMONTEE
C
      DO 8 I=N,1,-1
          S = ZERO
          DO 9 K=I+1,N
            S = S + L(K,I)*B(K,IS)
9           CONTINUE
          B(I,IS) = B(I,IS)-S
8     CONTINUE
5     CONTINUE
      END

      SUBROUTINE LCSOLZ(A , B , NDIM , N , NBSCMB , IRET)
      IMPLICIT REAL*8 (A-H,O-Z)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 14/10/96   AUTEUR KXBADNG N.GAY 
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
C     ------------------------------------------------------------------
C     RESOLUTION PAR LA METHODE DE GAUSS D'UN SYSTEME LINEAIRE
C     A COEFFICIENTS COMPLEXES
C     ------------------------------------------------------------------
C       VAR A      : C16: MATRICE CARREE PLEINE
C       VAR B      : C16: TABLEAU BI-INDICE DE COMPLEXES
C                       EN ENTREE : LES SECONDS MEMBRES
C                       EN SORTIE : LES SOLUTIONS
C       IN  N      : IS : ORDRE DE LA MATRICE
C       IN  NDIM   : IS : DIMENSION DECLAREE DE LA MATRICE
C       IN  NBSCMB : IS : NOMBRE DE SECONDS MEMBRES
C      OUT  IRET   : IS : 0 OK
C                         1 PIVOT NUL
C     ------------------------------------------------------------------
      INTEGER     IPIVOT
      REAL*8      R8MIEM,DCABS1
      REAL*8      APIVOT,ZERO,RMIN
      COMPLEX*16  AK,BK
      COMPLEX*16  A(NDIM,NDIM),B(NDIM,NBSCMB)
C
      IRET = 0
      ZERO = 0.D0
      RMIN = 100.D0*R8MIEM()
      DO 1000 I=1,N-1
C
C        DETERMINATION DU MEILLEUR PIVOT SUR LA COLONNE
         APIVOT = DCABS1(A(I,I))
         IPIVOT = I
         DO 100 K=I+1,N
            IF ( APIVOT - DCABS1(A(K,I)) .LT. ZERO ) THEN
               APIVOT = DCABS1(A(K,I))
               IPIVOT = K
            ENDIF
  100    CONTINUE
         IF ( APIVOT .LT. RMIN ) THEN
           IRET = 1
           GOTO 9999
         ENDIF
C
C        PERMUTATION DES LIGNES DE LA MATRICE
         DO 200 J=1,N
            AK          = A(I,J)
            A(I,J)      = A(IPIVOT,J)
            A(IPIVOT,J) = AK
  200    CONTINUE
C
C        PERMUTATION DES LIGNES DES SECONDS MEMBRES
         DO 300 ISCMB = 1, NBSCMB
            BK              = B(I,ISCMB)
            B(I,ISCMB)      = B(IPIVOT,ISCMB)
            B(IPIVOT,ISCMB) = BK
  300    CONTINUE
C
C        CALCUL DES NOUVEAUX TERMES DE LA MATRICE ET DES SECONDS MEMBRES
         DO 600 IL=I+1,N
            DO 400 ISCMB = 1,NBSCMB
               B(IL,ISCMB) = B(IL,ISCMB) - A(IL,I)*B(I,ISCMB)/A(I,I)
  400       CONTINUE
            DO 500 IC = I+1,N
               A(IL,IC) = A(IL,IC) - A(IL,I)*A(I,IC)/A(I,I)
  500       CONTINUE
  600    CONTINUE
C
 1000 CONTINUE
C
C     RESOLUTION
      DO 1100 ISCMB = 1, NBSCMB
         B(N,ISCMB) = B(N,ISCMB)/A(N,N)
         DO 1200 I=N-1,1,-1
            DO  1300 J=I+1,N
                B(I,ISCMB) = B(I,ISCMB)-A(I,J)*B(J,ISCMB)
 1300       CONTINUE
            B(I,ISCMB) = B(I,ISCMB)/A(I,I)
 1200    CONTINUE
 1100 CONTINUE
C
 9999 CONTINUE
      END

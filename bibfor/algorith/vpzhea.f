      SUBROUTINE VPZHEA(N,K,L,A,IA,INTGER)
C
C**********************************************************************
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/11/2006   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C**********************************************************************
C     PROCEDURE DIRHES
C     (CF. WILKINSON & REINSCH LINEAR ALGEBRA PP.346-347)
C     ETANT DONNEE LA MATRICE NON SYMETRIQUE A, STOCKEE DANS LE TABLEAU
C     A(N,N), CETTE ROUTINE REDUIT SOUS LA FORME D UNE MATRICE DE  
C     HESSENBERG HH, LA SOUS-MATRICE D ORDRE L-K+1 DEBUTANT A L ELEMENT
C     A(K,K) ET FINISSANT A L ELEMENT A(L,L). 
C     CETTE REDUCTION EST REALISEE PAR LA METHODE DIRECTE (AN=NH).
C     LA MATRICE HH EST REECRITE SUR LA MATRICE A, AVEC LES DETAILS
C     CONCERNANT LES TRANSFORMATIONS (N), STOCKES DANS LE TRIANGLE
C     RESTANT SOUS LA MATRICE HH ET DANS LES ELEMENTS K A L DU TABLEAU
C     INTGER(N).
C
C --- DECLARATIONS
C
      IMPLICIT NONE
C
C ARGUMENTS
C ---------
      INTEGER N, K, L, IA
      REAL*8 A(IA,N)
      INTEGER INTGER(N)
C
C VARIABLES LOCALES
C -----------------
      REAL*8 X, Y
      INTEGER I, J, K1, M
C 
C**********************************************************************
C                     DEBUT DU CODE EXECUTABLE
C**********************************************************************
C
      K1 = K + 1
      IF (K1.GT.L) GO TO 9999
      DO 140 J = K1, N
         M = J
         X = 0.0D0
         IF (J.GT.L) GO TO 120
         DO 20 I = J, L
            IF (ABS(A(I,J-1)).GT.ABS(X)) THEN
            X = A(I,J-1)
            M = I
            ENDIF
   20    CONTINUE
         INTGER(J) = M
C
C --- INTERVERSION DES COLONNES ET DES RANGEES DE LA MATRICE A
         IF (M.NE.J) THEN
          DO 40 I = K, N
            Y = A(M,I)
            A(M,I) = A(J,I)
            A(J,I) = Y
   40     CONTINUE
          DO 60 I = 1, L
            Y = A(I,M)
            A(I,M) = A(I,J)
            A(I,J) = Y
   60     CONTINUE
         ENDIF
         IF (X.NE.0.0D0 .AND. J.LT.L) THEN
            DO 100 I = J + 1, L
               A(I,J-1) = A(I,J-1)/X
  100       CONTINUE
          CALL VPZTR1(L,L-J,IA,A(1,J+1),A(J+1,J-1),A(1,J),1.D0)
         END IF
  120    CONTINUE
         CALL VPZTR2(J-K,IA,A(K+1,K),A(K+1,J))
         IF (J.LT.L)THEN
          CALL VPZTR1(L-J,J-K,IA,A(J+1,K),A(K+1,J),A(J+1,J),-1.D0)
         ENDIF
  140 CONTINUE
C
 9999 CONTINUE
      END

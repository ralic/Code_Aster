      SUBROUTINE MGAUST(A,B,DIM,NORDRE,NB,DET,IRET)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 25/07/2001   AUTEUR RATEAU G.RATEAU 
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

      IMPLICIT NONE

C ----------------------------------------------------------------------
C     RESOLUTION PAR LA METHODE DE GAUSS D'UN SYSTEME LINEAIRE
C ----------------------------------------------------------------------
C     VARIABLES D'ENTREE
C     INTEGER     DIM              : DIMENSION DE A 
C     INTEGER     NORDRE           : ORDRE DE LA MATRICE
C     INTEGER     NB               : NOMBRE DE SECONDS MEMBRES
C     REAL*8      A(DIM, DIM)      : MATRICE CARREE PLEINE
C     REAL*8      B(DIM, NB)       : SECONDS MEMBRES
C     REAL*8      DET             : 0 = PAS DE CALCUL DU DETERMINANT
C     LOGICAL     IRET            : .TRUE. = TEST SINGULARITE   
C
C     VARIABLES DE SORTIE
C     REAL*8      B(DIM, NB)       : At-1 * B     
C     REAL*8      DET              : DETERMINANT DE A
C     LOGICAL     IRET             : .FALSE. SI A SINGULIERE
C
C ----------------------------------------------------------------------
C     ATTENTION : LA MATRICE A EST MODIFIEE
C ----------------------------------------------------------------------
C
C     PARAMETRE
      REAL*8    CONDMX
      PARAMETER (CONDMX = 1.D16)

C     VARIABLES
      INTEGER     DIM,NB,NORDRE,I,J,K
      REAL*8      A(DIM,DIM),B(DIM,NB),DET,C,D,CMIN,CMAX
      LOGICAL     IRET,FLAG,LDET

      IF (DET.EQ.0.D0) THEN
        LDET = .FALSE.
      ELSE
        LDET = .TRUE.
        DET = 1.D0
      ENDIF

      DO 10 I = 1, NORDRE

C ----- RECHERCHE DU MEILLEUR PIVOT
 
        J = I
        C = A(I,I)
        FLAG = .FALSE.

        DO 20 K = I+1, NORDRE
          D = A(I,K)
          IF (ABS(C) .LT. ABS(D)) THEN
            C = D
            J = K
            FLAG = .TRUE.
          ENDIF
 20     CONTINUE

C ----- DETERMINANT

        IF (LDET) DET = DET * C 

C ----- ESTIMATION GROSSIERE DU CONDITIONNEMENT

        IF (IRET) THEN
          IF (I .EQ. 1) THEN
            CMIN = ABS(C)
            CMAX = CMIN
          ELSE
            IF (ABS(C) .LT. CMIN) THEN
              CMIN = ABS(C)
              IF (CMAX .GT. CONDMX*CMIN) THEN
                IRET = .FALSE.
                GOTO 100 
              ENDIF
              GOTO 30
            ENDIF
            IF (ABS(C) .GT. CMAX) THEN
              CMAX = ABS(C)
              IF (CMAX .GT. CONDMX*CMIN) THEN
                IRET = .FALSE.
                GOTO 100
              ENDIF
            ENDIF
          ENDIF
        ENDIF

 30     CONTINUE

C ----- PERMUTATION

        IF (FLAG) THEN

          DO 40 K = I, NORDRE
            D = A(K,I)
            A(K,I) = A(K,J)
            A(K,J) = D
 40       CONTINUE

          DO 50 K = 1, NB
            D = B(I,K)
            B(I,K) = B(J,K)
            B(J,K) = D
 50       CONTINUE

        ENDIF

C ----- ELIMINATION

        DO 10 J = I+1, NORDRE

          IF (A(I,J) .NE. 0.D0) THEN

            D = A(I,J)/C 

            DO 60 K = 1, NB
              B(J,K) = B(J,K) - D*B(I,K)
 60         CONTINUE
    
            DO 70 K = I+1, NORDRE
              A(K,J) = A(K,J) - D*A(K,I)
 70         CONTINUE     

          ENDIF

 10   CONTINUE

C --- RESOLUTION

      DO 80 K = 1, NB
        B(NORDRE,K) = B(NORDRE,K)/C

        DO 80 I = NORDRE-1, 1, -1
          D = 0.D0
          DO 90 J = I+1, NORDRE
            D = D + A(J,I) * B(J, K)
 90       CONTINUE

          B(I,K) = (B(I,K) - D) / A(I,I)

 80   CONTINUE

 100  CONTINUE

      END

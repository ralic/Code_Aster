      SUBROUTINE MMPROD(A,DIMA,I1A,N1A,I2A,N2A,B,DIMB,I1B,I2B,N2B,C)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 12/02/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C A_UTIL
C ----------------------------------------------------------------------
C        CALCUL DU PRODUIT DE MATRICES C = A(I1A,I2A) * B(I1B,I2B)
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C REAL*8   A(DIMA,*)  : MATRICE A
C INTEGER  DIMA       : DIMENSION DE A
C INTEGER  I1A(N1A)   : LISTE D'INDICES POUR LES LIGNES DE A
C                       I1A = 0 : TOUTES LES LIGNES
C INTEGER  N1A        : NOMBRE D'INDICES DANS I1A OU NOMBRE DE LIGNES
C INTEGER  I2A(N2A)   : LISTE D'INDICES POUR LES COLONNES DE A
C                       I2A = 0 : TOUTES LES COLONNES
C INTEGER  N2A        : NOMBRE D'INDICES DANS I2A OU NOMBRE DE COLONNES
C REAL*8   B(DIMB,*)  : MATRICE B
C INTEGER  DIMB       : DIMENSION DE B
C INTEGER  I1B(N2A)   : LISTE D'INDICES POUR LES LIGNES DE B
C                       I1B = 0 : TOUTES LES LIGNES
C INTEGER  I2B(N2B)   : LISTE D'INDICES POUR LES COLONNES DE B
C                       I2B = 0 : TOUTES LES COLONNES
C INTEGER  N2B        : NOMBRE D'INDICES DANS I2B OU NOMBRE DE COLONNES
C
C VARIABLE DE SORTIE
C REAL*8   C(N1A,N2B) : A(I1A,I2A) * B(I1B,I2B)
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      INTEGER I1A(*),I2A(*),I1B(*),I2B(*),DIMA,DIMB,N1A,N2A,N2B
      INTEGER I,J,K,I0,J0
      REAL*8  A(DIMA,*),B(DIMB,*),C(N1A,*),R

      IF (I1A(1).EQ.0) THEN
        IF (I2A(1).EQ.0) THEN
          IF (I1B(1).EQ.0) THEN
            IF (I2B(1).EQ.0) THEN
              DO 10 I = 1, N1A
                DO 10 J = 1, N2B
                  R = 0.D0
                  DO 20 K = 1, N2A
                    R = R + A(I,K)*B(K,J)
 20               CONTINUE
                  C(I,J) = R
 10           CONTINUE
            ELSE
              DO 30 I = 1, N1A
                DO 30 J0 = 1, N2B
                  J = I2B(J0)
                  R = 0.D0
                  DO 40 K = 1, N2A
                    R = R + A(I,K)*B(K,J)
 40               CONTINUE
                  C(I,J0) = R
 30           CONTINUE
            ENDIF
          ELSE
            IF (I2B(1).EQ.0) THEN
              DO 50 I = 1, N1A
                DO 50 J = 1, N2B
                  R = 0.D0
                  DO 60 K = 1, N2A
                    R = R + A(I,K)*B(I1B(K),J)
 60               CONTINUE
                  C(I,J) = R
 50           CONTINUE
            ELSE
              DO 70 I = 1, N1A
                DO 70 J0 = 1, N2B
                  J = I2B(J0)
                  R = 0.D0
                  DO 80 K = 1, N2A
                    R = R + A(I,K)*B(I1B(K),J)
 80               CONTINUE
                  C(I,J0) = R
 70           CONTINUE
            ENDIF
          ENDIF
        ELSE
          IF (I1B(1).EQ.0) THEN
            IF (I2B(1).EQ.0) THEN
              DO 90 I = 1, N1A
                DO 90 J = 1, N2B
                  R = 0.D0
                  DO 100 K = 1, N2A
                    R = R + A(I,I2A(K))*B(K,J)
 100              CONTINUE
                  C(I,J) = R
 90           CONTINUE
            ELSE
              DO 110 I = 1, N1A
                DO 110 J0 = 1, N2B
                  J = I2B(J0)
                  R = 0.D0
                  DO 120 K = 1, N2A
                    R = R + A(I,I2A(K))*B(K,J)
 120              CONTINUE
                  C(I,J0) = R
 110          CONTINUE
            ENDIF
          ELSE
            IF (I2B(1).EQ.0) THEN
              DO 130 I = 1, N1A
                DO 130 J = 1, N2B
                  R = 0.D0
                  DO 140 K = 1, N2A
                    R = R + A(I,I2A(K))*B(I1B(K),J)
 140              CONTINUE
                  C(I,J) = R
 130          CONTINUE
            ELSE
              DO 150 I = 1, N1A
                DO 150 J0 = 1, N2B
                  J = I2B(J0)
                  R = 0.D0
                  DO 160 K = 1, N2A
                    R = R + A(I,I2A(K))*B(I1B(K),J)
 160              CONTINUE
                  C(I,J0) = R
 150          CONTINUE
            ENDIF
          ENDIF
        ENDIF
      ELSE
        IF (I2A(1).EQ.0) THEN
          IF (I1B(1).EQ.0) THEN
            IF (I2B(1).EQ.0) THEN
              DO 170 I0 = 1, N1A
                I = I1A(I0)
                DO 170 J = 1, N2B
                  R = 0.D0
                  DO 180 K = 1, N2A
                    R = R + A(I,K)*B(K,J)
 180              CONTINUE
                  C(I0,J) = R
 170          CONTINUE
            ELSE
              DO 190 I0 = 1, N1A
                I = I1A(I0)
                DO 190 J0 = 1, N2B
                  J = I2B(J0)
                  R = 0.D0
                  DO 200 K = 1, N2A
                    R = R + A(I,K)*B(K,J)
 200              CONTINUE
                  C(I0,J0) = R
 190          CONTINUE
            ENDIF
          ELSE
            IF (I2B(1).EQ.0) THEN
              DO 210 I0 = 1, N1A
                I = I1A(I0)
                DO 210 J = 1, N2B
                  R = 0.D0
                  DO 220 K = 1, N2A
                    R = R + A(I,K)*B(I1B(K),J)
 220              CONTINUE
                  C(I0,J) = R
 210          CONTINUE
            ELSE
              DO 230 I0 = 1, N1A
                I = I1A(I0)
                DO 230 J0 = 1, N2B
                  J = I2B(J0)
                  R = 0.D0
                  DO 240 K = 1, N2A
                    R = R + A(I,K)*B(I1B(K),J)
 240              CONTINUE
                  C(I0,J0) = R
 230          CONTINUE
            ENDIF
          ENDIF
        ELSE
          IF (I1B(1).EQ.0) THEN
            IF (I2B(1).EQ.0) THEN
              DO 250 I0 = 1, N1A
                I = I1A(I0)
                DO 250 J = 1, N2B
                  R = 0.D0
                  DO 260 K = 1, N2A
                    R = R + A(I,I2A(K))*B(K,J)
 260              CONTINUE
                  C(I0,J) = R
 250          CONTINUE
            ELSE
              DO 270 I0 = 1, N1A
                I = I1A(I0)
                DO 270 J0 = 1, N2B
                  J = I2B(J0)
                  R = 0.D0
                  DO 280 K = 1, N2A
                    R = R + A(I,I2A(K))*B(K,J)
 280              CONTINUE
                  C(I0,J0) = R
 270          CONTINUE
            ENDIF
          ELSE
            IF (I2B(1).EQ.0) THEN
              DO 290 I0 = 1, N1A
                I = I1A(I0)
                DO 290 J = 1, N2B
                  R = 0.D0
                  DO 300 K = 1, N2A
                    R = R + A(I,I2A(K))*B(I1B(K),J)
 300              CONTINUE
                  C(I0,J) = R
 290          CONTINUE
            ELSE
              DO 310 I0 = 1, N1A
                I = I1A(I0)
                DO 310 J0 = 1, N2B
                  J = I2B(J0)
                  R = 0.D0
                  DO 320 K = 1, N2A
                    R = R + A(I,I2A(K))*B(I1B(K),J)
 320              CONTINUE
                  C(I0,J0) = R
 310          CONTINUE
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      END

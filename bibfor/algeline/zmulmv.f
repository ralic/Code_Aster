      SUBROUTINE ZMULMV (TRANS, M, N, ALPHA, A, LDA, X, INCX, BETA, Y,
     &                  INCY)
      IMPLICIT NONE
      INTEGER    M, N, LDA, INCX, INCY
      COMPLEX    *16 ALPHA, BETA, X(*), Y(*)
      CHARACTER*(*) TRANS
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C-----------------------------------------------------------------------
C  CALCUL DU PRODUIT D'UNE MATRICE PAR UN VECTEUR (OPTION 'N' 'T' 'C').
C-----------------------------------------------------------------------
C IN  : TRANS: CARACTERE SPECIFIANT L'OPERATION A REALISER.
C                 TRANS               OPERATION
C              'N'             Y = ALPHA*A*X + BETA*Y
C              'T'             Y = ALPHA*A'*X + BETA*Y
C              'C'             Y = ALPHA*CONJG(A)'*X + BETA*Y
C     : M    : NOMBRE DE LIGNES DE A.
C     : N    : NOMBRE DE COLONNES DE A.
C     : ALPHA: SCALAIRE.
C     : A    : MATRICE COMPLEXE DE DIMENSION M*N
C     : LDA  : DIMENSION DE A
C     : X    : VECTEUR COMLEXE DE LONGUEUR (N-1)*IABS(INCX)+1 LORSQUE
C              TRANS EST EGAL A 'N' ET DE LONGUEUR (M-1)*IABS(INCX)+1
C              SINON.
C     : INCX : DEPLACEMENT ENTRE LES ELEMENTS DE X.
C     : BETA : COMPLEXE.'LORSQUE BETA EGAL ZERO, Y EST NON CALCULE.
C I/O : Y    :  (N-1)*IABS(INCY)+1
C               (M-1)*IABS(INCY)+1
C OUT : INCY : DEPLACEMENT ENTRE LES ELEMENTS DE Y.
C-----------------------------------------------------------------------
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IX, IY, KY, LENX, LENY
      COMPLEX    *16 A(*)
      INTEGER    KX
      COMPLEX    *16 ZDOTC, ZDOTU
C
      IF (M.EQ.0 .OR. N.EQ.0 .OR.
     &    ALPHA.EQ.(0.0D0,0.0D0) .AND. BETA.EQ.(1.0D0,0.0D0))
     &    GO TO 9000
C
      IF (TRANS(1:1).EQ.'N' .OR. TRANS(1:1).EQ.'n') THEN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
C
      IX = 1
      IY = 1
      IF (INCX .LT. 0) IX = (-LENX+1)*INCX + 1
      IF (INCY .LT. 0) IY = (-LENY+1)*INCY + 1
C
      IF (BETA .EQ. (1.0D0,0.0D0)) THEN
      ELSE IF (INCY .EQ. 0) THEN
         IF (BETA .EQ. (0.0D0,0.0D0)) THEN
            Y(1) = (0.0D0,0.0D0)
         ELSE
            Y(1) = BETA**LENY*Y(1)
         END IF
      ELSE IF (BETA .EQ. (0.0D0,0.0D0)) THEN
         CALL ZINIT (LENY, (0.0D0,0.0D0), Y, ABS(INCY))
      ELSE
         CALL ZMULT (LENY, BETA, Y, ABS(INCY))
      END IF
C
      IF (ALPHA .EQ. (0.0D0,0.0D0)) GO TO 9000
C
      IF (TRANS(1:1).EQ.'N' .OR. TRANS(1:1).EQ.'n') THEN
         KX = IX
         DO 10  I=1, N
            CALL ZAXPY (M, ALPHA*X(KX), A(LDA*(I-1)+1), 1, Y, INCY)
            KX = KX + INCX
   10    CONTINUE
      ELSE IF (TRANS(1:1).EQ.'T' .OR. TRANS(1:1).EQ.'t') THEN
C
         KY = IY
         DO 20  I=1, N
            Y(KY) = Y(KY) + ALPHA*ZDOTU(M,A(LDA*(I-1)+1),1,X,INCX)
            KY = KY + INCY
   20    CONTINUE
C
      ELSE
         KY = IY
         DO 30  I=1, N
            Y(KY) = Y(KY) + ALPHA*ZDOTC(M,A(LDA*(I-1)+1),1,X,INCX)
            KY = KY + INCY
   30    CONTINUE
      END IF
C
 9000 CONTINUE
      END

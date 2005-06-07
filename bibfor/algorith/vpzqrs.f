      SUBROUTINE VPZQRS(N,M,HH,IH,LOC,VALPI,VALPR,ZVPS,IZ,
     &                  B,IB,U,V,ACC,IFAIL)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/07/96   AUTEUR CIBHHLV L.VIVAN 
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
C TOLE  CRP_20
C-----------------------------------------------------------------------
C
C     PROCEDURE INVIT
C     (CF. WILKINSON & REINSCH LINEAR ALGEBRA PP.427-431)
C     RECHERCHE LES M VECTEURS PROPRES D'UNE MATRICE REELLE SUPERIEURE
C     DE TYPE HESSENBERG (STOCKEE DANS LE TABLEAU HH(N,N)), CONNAISSANT
C     LES PARTIES REELLES ET IMAGINAIRES DES VALEURS PROPRES DE CETTE
C     MATRICE (STOCKEES DANS LES TABLEAUX VALPR(N) ET VALPI(N))
C     LES VECTEURS PROPRES A RECHERCHER SONT SELECTIONNES DANS LE
C     TABLEAU LOGIQUE LOC(N)
C     LES VECTEURS PROPRES RESULTATS SONT PLACES DANS LE TABLEAU
C     ZVPS(N,M). UN SEUL VECTEUR PROPRE, CORRESPONDANT A LA VALEUR
C     PROPRE DE PARTIE IMAGINAIRE POSITIVE, EST STOCKE DANS LE CAS
C     DE VALEURS PROPRES COMPLEXES CONJUGUEES. UN VECTEUR PROPRE NON
C     SELECTIONNE EST MIS A ZERO.
C     ACC REPRESENTE LA PRECISION MACHINE RELATIVE.
C
C --- DECLARATIONS
C
      IMPLICIT NONE
C
C ARGUMENTS
      INTEGER N, M, IH, IZ, IB, IFAIL
      REAL*8 ACC
      REAL*8 HH(IH,N), VALPI(N), VALPR(N), ZVPS(IZ,M)
      REAL*8 B(IB,N), U(N), V(N)
      LOGICAL LOC(N)
C
C VARIABLES LOCALES
      INTEGER I, I1, I2, II, IS, ITS, J, J2, K, K1
      INTEGER LUK, LUK1, LUK2, M1, N1
      REAL*8 EPS3, ACCRTL, RILAM, RRLAM, RNORM, RNORMV
      REAL*8 W, X, Y
      REAL*8 A, H
      REAL*8 XLUK, XEPS3, ZR, ZI
      LOGICAL CONJ2
C
C**********************************************************************
C                        DEBUT DU CODE EXECUTABLE
C**********************************************************************
C
C======================================================================
C     INITIALISATION
C======================================================================
      IFAIL = 0
      J = 0
      CONJ2 = .FALSE.
C
C======================================================================
C      VERIFICATION DES DONNEES D ENTREE
C======================================================================
      DO 40 I = 1, N
         IF (LOC(I))  THEN
            J = J + 1
         ENDIF
         IF (VALPI(I) .EQ. 0.0D0) THEN
            GO TO 40
         ENDIF
         IF (CONJ2) THEN
            GO TO 20
         ENDIF
         IF  (((LOC(I)) .AND. (.NOT. LOC(I+1)))
     S   .OR. ((.NOT. LOC(I)) .AND. (LOC(I+1)))) THEN
            GO TO 60
         ENDIF
   20    CONTINUE
         CONJ2 = .NOT. CONJ2
   40 CONTINUE
      IF (J.LE.M) THEN
         GO TO 80
      ENDIF
C
C --- ERREUR : TAILLE DU TABLEAU LOC(TRUE) SUPERIEURE A M
C
      IFAIL = 1
      GO TO 9999
C
C --- ERREUR : VALEURS PROPRES COMPLEXES MAL SELECTIONNEES
C
   60 CONTINUE
      IFAIL = 2
      GO TO 9999
C
C=======================================================================
C     BOUCLE SUR LES N VALEURS PROPRES DE LA MATRICE HH
C=======================================================================
   80 CONTINUE
      LUK = 0
      IS = 1
C
      DO 9000 K = 1, N
C
C === TEST PUIS CALCUL SUR LES MODES SELECTIONNES
         IF (LOC(K)) THEN
C
C======================================================================
C -1- NORMALISATION DE LA SOUS-MATRICE DE DIMENSION LUK*LUK
C======================================================================
         IF (LUK.LT.K) THEN
           N1 = N - 1
             IF (K.LE.N1) THEN
                DO 100 LUK = K, N1
                 LUK1 = LUK + 1
                  IF (HH(LUK1,LUK) .EQ. 0.0D0) THEN
                    GO TO 120
                  ENDIF
  100           CONTINUE
             ENDIF
           LUK = N
  120      CONTINUE
           RNORM = 0.0D0
           M1 = 1
            DO 160 I = 1, LUK
             X = 0.0D0
              IF (M1.LE.LUK) THEN
                DO 140 J = M1, LUK
                 X = X + ABS(HH(I,J))
  140           CONTINUE
              ENDIF
              IF (X.GT.RNORM) THEN
               RNORM = X
              ENDIF
            M1 = I
  160      CONTINUE
C
C======================================================================
C -2- REMPLACEMENT PAR EPS3 DU PIVOT NUL DANS LA DECOMPOSITION
C     LES RACINES PROCHES SONT MODIFIEES PAR EPS3
C     (ACCRTL EST LE CRITERE D ACCROISSEMENT)
C======================================================================
         EPS3 = ACC*RNORM
         ACCRTL = (1.0D0/SQRT(DBLE(LUK)))/100.0D0
         ENDIF
C
C======================================================================
C -3- TEST SUR LA NORME DE LA SOUS-MATRICE DE DIMENSION LUK*LUK
C======================================================================
         IF (RNORM .EQ. 0.0D0) THEN
           DO 300 I = 1, N
            ZVPS(I,IS) = 0.0D0
  300      CONTINUE
            ZVPS(K,IS) = 1.0D0
            IS = IS + 1
            GO TO 9000
         ENDIF
         RRLAM = VALPR(K)
         RILAM = VALPI(K)
C
C======================================================================
C -4- PERTURBATION DE LA VALEUR PROPRE SI ELLE EST PROCHE
C     D UNE AUTRE VALEUR PROPRE
C======================================================================
  400    CONTINUE
         I = K
         K1 = K - 1
         IF (K1.GE.1) THEN
          DO 420 II = 1, K1
            I = I - 1
           IF ((LOC(I)) .AND. (ABS(VALPR(I)-RRLAM).LT.EPS3)
     &      .AND. (ABS(VALPI(I)-RILAM).LT.EPS3)) THEN
             RRLAM = RRLAM +EPS3
             GO TO 400
            ENDIF
  420     CONTINUE
         ENDIF
         VALPR(K) = RRLAM
C
C======================================================================
C -5- FORMATION DE LA MATRICE DE HESSENBERG SUPERIEURE B = HH - RRLAM*I
C     ET DU VECTEUR REEL INITIAL U
C======================================================================
         M1 = 1
         DO 520 I = 1, LUK
            IF (M1.LE.LUK) THEN
             DO 500 J = M1, LUK
               B(I,J) = HH(I,J)
  500        CONTINUE
            ENDIF
            B(I,I) = B(I,I) - RRLAM
            M1 = I
            U(I) = EPS3
  520    CONTINUE
         ITS = 0
C
C======================================================================
C -6- TEST SUR LA PARTIE IMAGINAIRE DE LA VALEUR PROPRE
C======================================================================
         IF (RILAM .EQ. 0.0D0) THEN
C
C***********************************************************************
C    TRAITEMENT DANS LE CAS D UNE VALEUR PROPRE REELLE
C***********************************************************************
          IF (LUK.GE.2) THEN
C======================================================================
C -7- TRANSFORMATIONS ET DECOMPOSITION
C======================================================================
         DO 760 I = 2, LUK
            M1 = I - 1
            IF (ABS(B(I,M1)).LE.ABS(B(M1,M1))) THEN
              GO TO 720
            ENDIF
            IF (M1.LE.LUK) THEN
             DO 700 J = M1, LUK
               Y = B(I,J)
               B(I,J) = B(M1,J)
               B(M1,J) = Y
  700        CONTINUE
            ENDIF
  720       CONTINUE
            IF (B(M1,M1) .EQ. 0.0D0) THEN
              B(M1,M1) = EPS3
            ENDIF
              X = B(I,M1)/B(M1,M1)
            IF (X .NE. 0.0D0) THEN
             DO 740 J = I, LUK
               B(I,J) = B(I,J) - X*B(M1,J)
  740        CONTINUE
            ENDIF
  760    CONTINUE
          ENDIF
C
C======================================================================
C -8- REMPLACEMENT DES PIVOTS NULS PAR EPS3
C======================================================================
         IF (B(LUK,LUK) .EQ. 0.0D0) THEN
          B(LUK,LUK) = EPS3
         ENDIF
C
C======================================================================
C -9- VECTEUR REEL INITIAL U
C======================================================================
  900     CONTINUE
          I = LUK + 1
          DO 940 II = 1, LUK
            I = I - 1
            Y = U(I)
            I1 = I + 1
            IF (I1.LE.LUK) THEN
             DO 920 J = I1, LUK
               Y = Y - B(I,J)*U(J)
  920        CONTINUE
            ENDIF
            U(I) = Y/B(I,I)
  940    CONTINUE
C
C======================================================================
C -10- CALCUL DE LA NORME
C======================================================================
         ITS = ITS + 1
         RNORM = 0.0D0
         RNORMV = 0.0D0
         DO 1000 I = 1, LUK
            X = ABS(U(I))
            IF (RNORMV.LT.X) THEN
            RNORMV = X
            J = I
            ENDIF
            RNORM = RNORM + X
 1000    CONTINUE
C
C======================================================================
C -11- VECTEUR PROPRE REEL
C======================================================================
         IF (RNORM.GE.ACCRTL) THEN
C --- ON CONSERVE LE VECTEUR CALCULE SI LA NORME EST SUPERIEURE A ACCRTL
          X = 1.0D0/U(J)
          IF (LUK.GE.1)  THEN
           DO 1100 I = 1, LUK
            ZVPS(I,IS) = U(I)*X
 1100      CONTINUE
          ENDIF
          J = LUK + 1
          GO TO 1140
         ENDIF
         IF (ITS.LT.LUK) THEN
            XLUK = SQRT(DBLE(LUK))
            XEPS3 = EPS3/(XLUK + 1.0D0)
            U(1) = EPS3
            IF (N .GE. 2) THEN
             DO 1120 I=2,N
              U(I)=XEPS3
 1120        CONTINUE
            ENDIF
            U(LUK-ITS+1)=U(LUK-ITS+1) - EPS3*XLUK
         GO TO 900
         ENDIF
C --- MISE A ZERO DU VECTEUR PROPRE
         J = 1
 1140    CONTINUE
         IF (J.LE.N) THEN
          DO 1160 I = J, N
            ZVPS(I,IS) = 0.0D0
 1160     CONTINUE
         ENDIF
         IS = IS + 1
         GO TO 9000
C
C======================================================================
C -6- TEST SUR LE SIGNE DE LA PARTIE IMAGINAIRE DE LA VALEUR PROPRE
C======================================================================
C
C***********************************************************************
C    TRAITEMENT DANS LE CAS D UNE VALEUR PROPRE COMPLEXE
C    AVEC PARTIE IMAGINAIRE POSITIVE
C***********************************************************************
         ELSE IF (RILAM.GT.0.0D0) THEN
         DO 610 I = 1, LUK
            V(I) = 0.0D0
  610    CONTINUE
C
C======================================================================
C -7-  DECOMPOSITION TRIANGULAIRE
C======================================================================
         B(3,1) = -RILAM
         LUK2 = LUK + 2
         I = LUK + 3
         IF (LUK2.GE.4) THEN
         DO 710 II = 4, LUK2
            I = I - 1
            B(I,1) = 0.0D0
  710    CONTINUE
         ENDIF
         IF (LUK.GE.2) THEN
          DO 770 I = 2, LUK
            M1 = I - 1
            W = B(I,M1)
            I1 = I + 1
            X = B(M1,M1)**2 + B(I1,M1)**2
            IF ((W**2).GT.X) THEN
             X = B(M1,M1)/W
             Y = B(I1,M1)/W
             B(M1,M1) = W
             B(I1,M1) = 0.0D0
              DO 730 J = I, LUK
               W = B(I,J)
               B(I,J) = B(M1,J) - X*W
               B(M1,J) = W
               J2 = J + 2
               B(J2,I) = B(J2,M1) - Y*W
               B(J2,M1) = 0.0D0
  730         CONTINUE
             I2 = I + 2
             B(I2,M1) = -RILAM
             B(I,I) = B(I,I) - Y*RILAM
             B(I2,I) = B(I2,I) + X*RILAM
             GO TO 770
            ENDIF
            IF (X.EQ.0.0D0) THEN
             B(M1,M1) = EPS3
             B(I1,M1) = 0.0D0
             X = EPS3**2
            ENDIF
            W = W/X
            X = B(M1,M1)*W
            Y = -B(I1,M1)*W
             DO 750  J = I, LUK
               J2 = J + 2
               B(I,J) = B(I,J) - X*B(M1,J) + Y*B(J2,M1)
               B(J2,I) = -X*B(J2,M1) - Y*B(M1,J)
  750        CONTINUE
            I2 = I + 2
            B(I2,I) = B(I2,I) - RILAM
  770    CONTINUE
         ENDIF
C
C======================================================================
C -8- REMPLACEMENT DES PIVOTS NULS PAR EPS3
C======================================================================
         IF ((B(LUK,LUK).EQ.0.0D0) .AND. (B(LUK+2,LUK).EQ.0.0D0)) THEN
             B(LUK,LUK) = EPS3
         ENDIF
C
C======================================================================
C -9- VECTEURS INITIAUX U ET V
C======================================================================
  910    CONTINUE
         IF (LUK.GE.1) THEN
         I = LUK + 1
          DO 950 II = 1, LUK
            I = I - 1
            X = U(I)
            Y = V(I)
            I1 = I + 1
             IF (I1.LE.LUK) THEN
              DO 930 J = I1, LUK
               X = X - B(I,J)*U(J) + B(J+2,I)*V(J)
               Y = Y - B(I,J)*V(J) - B(J+2,I)*U(J)
 930         CONTINUE
             ENDIF
            IF (ABS(B(I,I)) .GT. ABS(B(I+2,I))) THEN
             H = B(I+2,I)/B(I,I)
             A = 1.0D0/(H*B(I+2,I) + B(I,I))
             U(I) = (X + H*Y)*A
             V(I) = (Y - H*X)*A
            ELSE
             H = B(I,I)/B(I+2,I)
             A = 1.0D0/(H*B(I,I) + B(I+2,I))
             U(I) = (Y + H*X)*A
             V(I) = (H*Y - X)*A
            ENDIF
 950      CONTINUE
         ENDIF
C
C======================================================================
C -10- CALCUL DE LA NORME
C======================================================================
         ITS = ITS + 1
         RNORM = 0.0D0
         RNORMV = 0.0D0
         IF (LUK.GE.1) THEN
         DO 1010 I = 1, LUK
            ZR = ABS(U(I))
            ZI = ABS(V(I))
             IF (ZI .GT. ZR) THEN
              H = ZR
              ZR = ZI
              ZI = H
             ENDIF
             IF (ZI .EQ. 0.0D0) THEN
              X = ZR
             ELSE
              H = ZR * SQRT(1.0D0 + (ZI/ZR)**2.0D0)
              X = H
             ENDIF
            IF (RNORMV.LT.X) THEN
            RNORMV = X
            J = I
            ENDIF
            RNORM = RNORM + X
 1010    CONTINUE
         ENDIF
         M1 = IS + 1
C
C======================================================================
C -11- VECTEUR PROPRE COMPLEXE
C======================================================================
         IF (RNORM.GE.ACCRTL) THEN
C --- ON CONSERVE LE VECTEUR CALCULE SI LA NORME EST SUPERIEURE A ACCRTL
         X = U(J)
         Y = V(J)
          IF (LUK.GE.1) THEN
           DO 1110 I = 1, LUK
            IF (ABS(X) .GT. ABS(Y)) THEN
             H = Y/X
             A = 1.0D0/(H*Y + X)
             ZVPS(I,IS) = (U(I) + H*V(I))*A
             ZVPS(I,M) = (V(I) - H*U(I))*A
            ELSE
             H = X/Y
             A = 1.0D0/(H*X + Y)
             ZVPS(I,IS) = (V(I) + H*U(I))*A
             ZVPS(I,M) = (H*V(I) - U(I))*A
            ENDIF
 1110      CONTINUE
          ENDIF
         J = LUK + 1
         GO TO 1170
         ENDIF
         IF (ITS.LT.LUK) THEN
            XLUK = SQRT(DBLE(LUK))
            XEPS3 = EPS3/(XLUK + 1.0D0)
            U(1) = EPS3
            IF (N .GE. 2) THEN
             DO 1130 I=2,N
              U(I)=XEPS3
 1130        CONTINUE
            ENDIF
            U(LUK-ITS+1)=U(LUK-ITS+1) - EPS3*XLUK
             IF (LUK.GE.1) THEN
               DO 1150 I = 1, LUK
               V(I) = 0.0D0
 1150          CONTINUE
             ENDIF
         GO TO 910
         ENDIF
C --- MISE A ZERO DU VECTEUR PROPRE
         J = 1
 1170    CONTINUE
         IF (J.LE.N) THEN
         DO 1190 I = J, N
            ZVPS(I,IS) = 0.0D0
            ZVPS(I,M1) = 0.0D0
 1190    CONTINUE
         ENDIF
 1200    CONTINUE
         IS = IS + 2
         ENDIF
      ENDIF
C
 9000 CONTINUE
C
 9999 CONTINUE
      END

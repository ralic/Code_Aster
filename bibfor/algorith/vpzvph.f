      SUBROUTINE VPZVPH(NN,ACC,PREREL,HH,IH,VALPR,VALPI,ICNT,IER)
C
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/11/97   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C----------------------------------------------------------------------
C     PROCEDURE HQR
C     (CF. WILKINSON & REINSCH LINEAR ALGEBRA PP.363-365)
C     RECHERCHE DES VALEURS PROPRES D UNE MATRICE DE HESSENBERG REELLE
C     SUPERIEURE, HH, STOCKEE DANS LE TABLEAU HH(N,N) ET STOCKE LES
C     PARTIES REELLES DANS LE TABLEAU VALPR(N) ET LES PARTIES
C     IMAGINAIRES DANS LE TABLEAU VALPI(N).
C     ACC EST LA PRECISION MACHINE RELATIVE.
C     LA ROUTINE ECHOUE SI LA RECHERCHE DES VALEURS PROPRES NECESSITE
C     PLUS DE 30 ITERATIONS.
C
C
C --- DECLARATIONS
C
      IMPLICIT NONE
C
C ARGUMENTS
      INTEGER NN, IH, IER
      REAL*8 ACC,PREREL
      INTEGER ICNT(NN)
      REAL*8 HH(IH,NN), VALPR(NN), VALPI(NN)
C
C VARIABLES LOCALES
      REAL*8 NORM, P, Q, R, S, T, W, X, Y, Z
      INTEGER I, ITN, ITS, J, K, L, LL, M, M2, M3, MM
      INTEGER N, N2, NA, NHS
      LOGICAL NOTLST
C
C**********************************************************************
C                        DEBUT DU CODE EXECUTABLE
C**********************************************************************
C
C
C --- INITIALISATION
      IER = 0
      T = 0.0D0
      N = NN
      ITN = 30*N
C
C --- CALCUL DE LA NORME DE LA MATRICE
      NORM = 0.0D0
      K = 1
      DO 40 I = 1, N
         DO 20 J = K, N
            NORM = NORM + ABS(HH(I,J))
   20    CONTINUE
         K = I
   40 CONTINUE
      NHS = N*(N+1)/2 + N - 1
   60 CONTINUE
      IF (N.EQ.0)  GO TO 9999
      ITS = 0
      NA = N - 1
C
C --- RECHERCHE D ELEMENTS SIMPLES DE LA SOUS-DIAGONALE
   80 CONTINUE
      L = N + 1
      IF (N.GE.2) THEN
       DO 100 LL = 2, N
         L = L - 1
         S = ABS(HH(L-1,L-1)) + ABS(HH(L,L))
         IF (S.LT.PREREL) THEN
           S = NORM/DBLE(NHS)
         ENDIF
         IF (ABS(HH(L,L-1)).LE.ACC*S) THEN
           GO TO 120
         ENDIF
  100  CONTINUE
      ENDIF
      L = 1
  120 CONTINUE
      X = HH(N,N)
      IF (L.EQ.N) GO TO 360
      Y = HH(NA,NA)
      W = HH(N,NA)*HH(NA,N)
      IF (L.EQ.NA) GO TO 380
      IF (ITN.LE.0) THEN
       IER = 1
       GO TO 9999
      ENDIF
      IF ((ITS.EQ.10) .OR. (ITS.EQ.20)) THEN
C
C --- DECALAGE
       T = T + X
        IF (NA.GE.1) THEN
          DO 140 I = 1, NA
           HH(I,I) = HH(I,I) - X
  140     CONTINUE
        ENDIF
       HH(N,N) = 0.0D0
       S = ABS(HH(N,NA)) + ABS(HH(NA,N-2))
       X = 0.75D0*S
       Y = 0.75D0*S
       W = -0.4375D0*S**2
      ENDIF
      ITS = ITS + 1
      ITN = ITN - 1
C
C --- RECHERCHE DE DEUX ELEMENTS CONSECUTIFS DE LA SOUS-DIAGONALE
      IF (L.LE.(N-2)) THEN
       M = N - 1
       N2 = N - 2
       DO 160 MM = L, N2
         M = M - 1
         Z = HH(M,M)
         R = X - Z
         S = Y - Z
         P = (R*S-W)/HH(M+1,M) + HH(M,M+1)
         Q = HH(M+1,M+1) - Z - R - S
         R = HH(M+2,M+1)
         S = ABS(P) + ABS(Q) + ABS(R)
         P = P/S
         Q = Q/S
         R = R/S
         IF (M.EQ.L) THEN
           GO TO 180
         ENDIF
         IF ((ABS(HH(M,M-1))*(ABS(Q)+ABS(R))).LE.(ACC*ABS(P)
     &       *(ABS(HH(M-1,M-1))+ABS(Z)+ABS(HH(M+1,M+1))))) THEN
           GO TO 180
         ENDIF
 160   CONTINUE
      ENDIF
 180  CONTINUE
      M2 = M + 2
      IF (M2.LE.N) THEN
       DO 200 I = M2, N
         HH(I,I-2) = 0.0D0
 200   CONTINUE
      ENDIF
      M3 = M + 3
      IF (M3.LE.N) THEN
       DO 220 I = M3, N
         HH(I,I-3) = 0.0D0
  220  CONTINUE
      ENDIF
      IF (M.GT.NA) GO TO 80
C
C --- DOUBLE ETAPE QR CONCERNANT LES RANGEES L A N ET LES COLONNES M A N
      DO 340 K = M, NA
         NOTLST = .TRUE.
         IF (K.EQ.NA) NOTLST = .FALSE.
         IF (K.NE.M) THEN
          P = HH(K,K-1)
          Q = HH(K+1,K-1)
          R = 0.0D0
           IF (NOTLST) THEN
             R = HH(K+2,K-1)
           ENDIF
          X = ABS(P) + ABS(Q) + ABS(R)
           IF (X.EQ.0.0D0) GO TO 340
          P = P/X
          Q = Q/X
          R = R/X
         ENDIF
         S = SQRT(P**2+Q**2+R**2)
         IF (P.LT.0.0D0) THEN
           S = -S
         ENDIF
         IF (K.NE.M) THEN
           HH(K,K-1) = -S*X
         ELSE
           IF (L.NE.M) THEN
             HH(K,K-1) = -HH(K,K-1)
           ENDIF
         ENDIF
         P = P + S
         X = P/S
         Y = Q/S
         Z = R/S
         Q = Q/P
         R = R/P
         IF (K.LE.N) THEN
C
C --- MODIFICATION DES RANGEES
          IF (.NOT. NOTLST) THEN
            DO 240 J = K, N
               P = HH(K,J) + Q*HH(K+1,J)
               HH(K+1,J) = HH(K+1,J) - P*Y
               HH(K,J) = HH(K,J) - P*X
  240       CONTINUE
            GO TO 280
          ENDIF
          DO 260 J = K, N
            P = HH(K,J) + Q*HH(K+1,J) + R*HH(K+2,J)
            HH(K+2,J) = HH(K+2,J) - P*Z
            HH(K+1,J) = HH(K+1,J) - P*Y
            HH(K,J) = HH(K,J) - P*X
  260     CONTINUE
         ENDIF
  280    CONTINUE
         J = N
         IF ((K+3).LT.N) THEN
           J = K + 3
         ENDIF
         IF (L.LE.J) THEN
C
C --- MODIFICATION DES COLONNES
          IF (.NOT. NOTLST) THEN
           DO 300 I = L, J
            P = X*HH(I,K) + Y*HH(I,K+1)
            HH(I,K+1) = HH(I,K+1) - P*Q
            HH(I,K) = HH(I,K) - P
  300      CONTINUE
           GO TO 340
          ENDIF
          DO 320 I = L, J
            P = X*HH(I,K) + Y*HH(I,K+1) + Z*HH(I,K+2)
            HH(I,K+2) = HH(I,K+2) - P*R
            HH(I,K+1) = HH(I,K+1) - P*Q
            HH(I,K) = HH(I,K) - P
  320     CONTINUE
         ENDIF
  340 CONTINUE
      GO TO 80
C
C --- UNE RACINE ETE TROUVEE (REELLE)
  360 CONTINUE
      VALPR(N) = X + T
      VALPI(N) = 0.0D0
      ICNT(N) = ITS
      N = NA
      GO TO 60
C
C --- DEUX RACINES ONT ETE TROUVEES
  380 CONTINUE
      P = (Y-X)/2.0D0
      Q = P**2 + W
      Y = SQRT(ABS(Q))
      X = X + T
      ICNT(N) = -ITS
      ICNT(NA) = ITS
C
C --- PAIRE DE RACINES COMPLEXES CONJUGUEES
      IF (Q.LE.0.0D0) THEN
       VALPR(NA) = X + P
       VALPR(N) = X + P
       VALPI(NA) = Y
       VALPI(N) = -Y
      ELSE
C --- PAIRE DE RACINES REELLES
       IF (P.LT.0.0D0) THEN
         Y = -Y
       ENDIF
       Y = P + Y
       VALPR(NA) = X + Y
       VALPR(N) = X - W/Y
       VALPI(N) = 0.0D0
       VALPI(NA) = 0.0D0
      ENDIF
      N = N - 2
      GO TO 60
C
 9999 CONTINUE
      END

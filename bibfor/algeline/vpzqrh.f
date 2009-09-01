      SUBROUTINE VPZQRH (H,NEQ,IH,K,L,WR,WI,Z,IZ,MXITER,IER,NITQR)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER              NEQ,IH,K,L,IZ,IER,NITQR
      REAL*8             H(IH,NEQ), WR(NEQ),WI(NEQ),Z(IZ,NEQ)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 01/09/2009   AUTEUR DELMAS J.DELMAS 
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
C     RECHERCHE DES VALEURS PROPRES PAR LA METHODE QR SUR UNE MATRICE
C     MISE SOUS LA FORME DE HESSENBERG
C     ------------------------------------------------------------------
C     REFERENCE: F.L. BAUER - J.H. WILKINSON - C. REINSCH
C        HANDBOOK FOR AUTOMATIC COMPUTATION - LINEAR ALGEBRA - VOL.2
C        PAGE XXX
C     ------------------------------------------------------------------
      INTEGER            I,IEN,IENM2,NPL,LL,LB,NAML,MM,M,MP2,KA,NA
      INTEGER            ITER,J,JJ
      REAL*8             EPSMAC,T,X,Y,W,S,ZZ
      REAL*8             R,P,Q,RNORM,RA,SA,SCALE,VR,VI
      COMPLEX*16         Z3
      LOGICAL            NOTLAS
C     ------------------------------------------------------------------
      IER = 0
      EPSMAC = R8PREM()
      NITQR = 0
C
C     --- STOCKER LES RACINES TROUVEES PAR VPZBAL (DIT LA BALANCE) ---
      RNORM = 0.0D0
      KA = 1
      DO 10 I=1,NEQ
         DO 5 J=KA,NEQ
            RNORM = RNORM+ABS(H(I,J))
    5    CONTINUE
         KA = I
         IF (I.GE.K .AND. I.LE.L) GO TO 10
         WR(I) = H(I,I)
         WI(I) = 0.D0
   10 CONTINUE
      IEN = L
      T = 0.D0
C
C     --- RECHERCHE DES VALEURS PROPRES SUIVANTES ---
   15 CONTINUE
      IF (IEN.LT.K) GO TO 145
      ITER   = 0
      NA = IEN-1
      IENM2 = NA-1
C
C     --- RECHERCHE DU PLUS PETIT ELEMENT (SIMPLE) SUR LA SUR-DIAGONALE
   20 CONTINUE
      NPL = IEN+K
      DO 25 LL=K,IEN
         LB = NPL-LL
         IF (LB.EQ.K) GO TO 30
         S = ABS(H(LB-1,LB-1))+ABS(H(LB,LB))
         IF (S.EQ.0.0D0) S = RNORM
         IF (ABS(H(LB,LB-1)).LE.EPSMAC*S) GO TO 30
   25 CONTINUE
C
   30 CONTINUE
      X = H(IEN,IEN)
      IF (LB.EQ.IEN) GO TO 110
      Y = H(NA,NA)
      W = H(IEN,NA)*H(NA,IEN)
      IF (LB.EQ.NA) GO TO 115
      IF (ITER.EQ.MXITER) GO TO 250
C
C     --- FORMER UN DECALAGE TOUT LES DIX COUPS ---
      ITER = ITER+1
      IF (ITER .GT. NITQR) THEN
         NITQR = ITER
      ENDIF
      IF (MOD(ITER,10) .EQ. 0) THEN
         T = T+X
         DO 35 I=K,IEN
            H(I,I) = H(I,I)-X
   35    CONTINUE
         S = ABS(H(IEN,NA))+ABS(H(NA,IENM2))
         X = 0.75D0*S
         Y = X
         W = -0.4375D0*S*S
      ENDIF
C
C     --- RECHERCHE DES 2 PLUS PETITS ELEMENTS SUR LA SUR-DIAGONALE
      NAML = IENM2+LB
      DO 45 MM=LB,IENM2
         M = NAML-MM
         ZZ = H(M,M)
         R = X-ZZ
         S = Y-ZZ
         P = (R*S-W)/H(M+1,M)+H(M,M+1)
         Q = H(M+1,M+1)-ZZ-R-S
         R = H(M+2,M+1)
         S = ABS(P)+ABS(Q)+ABS(R)
         P = P/S
         Q = Q/S
         R = R/S
         IF (M.EQ.LB) GO TO 50
         IF (ABS(H(M,M-1))*(ABS(Q)+ABS(R)).LE.EPSMAC*ABS(P)*(ABS(H(M-1,
     *   M-1))+ABS(ZZ)+ABS(H(M+1,M+1)))) GO TO 50
   45 CONTINUE
   50 CONTINUE
      MP2 = M+2
      DO 55 I=MP2,IEN
         H(I,I-2) = 0.D0
         IF (I.EQ.MP2) GO TO 55
         H(I,I-3) = 0.D0
   55 CONTINUE
C
C     EN AVANT POUR LE "DOUBLE QR" SUR LA SOUS MATRICE
C              LIGNES DE "L" A "EN" ET COLONNES DE "M" A "EN"
      DO 105 KA=M,NA
         NOTLAS = KA.NE.NA
         IF (KA.EQ.M) GO TO 60
         P = H(KA,KA-1)
         Q = H(KA+1,KA-1)
         R = 0.D0
         IF (NOTLAS) R = H(KA+2,KA-1)
         X = ABS(P)+ABS(Q)+ABS(R)
         IF (X.EQ.0.D0) GO TO 105
         P = P/X
         Q = Q/X
         R = R/X
   60    CONTINUE
         S = SIGN(SQRT(P*P+Q*Q+R*R),P)
         IF (KA.EQ.M) THEN
            IF (LB.NE.M) H(KA,KA-1) = -H(KA,KA-1)
         ELSE
            H(KA,KA-1) = -S*X
         ENDIF
         P = P+S
         X = P/S
         Y = Q/S
         ZZ = R/S
         Q = Q/P
         R = R/P
C        --- ALTERATION DES LIGNES ---
         DO 80 J=KA,NEQ
            P = H(KA,J)+Q*H(KA+1,J)
            IF ( NOTLAS) THEN
               P = P+R*H(KA+2,J)
               H(KA+2,J) = H(KA+2,J)-P*ZZ
            ENDIF
            H(KA+1,J) = H(KA+1,J)-P*Y
            H(KA,J) = H(KA,J)-P*X
   80    CONTINUE
         J = MIN(IEN,KA+3)
C        --- ALTERATION DES COLONNES ---
         DO 90 I=1,J
            P = X*H(I,KA)+Y*H(I,KA+1)
            IF (.NOT.NOTLAS) GO TO 85
            P = P+ZZ*H(I,KA+2)
            H(I,KA+2) = H(I,KA+2)-P*R
   85       CONTINUE
            H(I,KA+1) = H(I,KA+1)-P*Q
            H(I,KA) = H(I,KA)-P
   90    CONTINUE
         IF (IZ.GE.NEQ) THEN
C           --- ON GARDE LES TRANSFORMATIONS POUR LES VECTEURS ----
            DO 100 I=K,L
               P = X*Z(I,KA)+Y*Z(I,KA+1)
               IF (NOTLAS) THEN
                  P = P+ZZ*Z(I,KA+2)
                  Z(I,KA+2) = Z(I,KA+2)-P*R
               ENDIF
               Z(I,KA+1) = Z(I,KA+1)-P*Q
               Z(I,KA) = Z(I,KA)-P
  100       CONTINUE
         ENDIF
  105 CONTINUE
      GO TO 20
C
C     ---------------------------- AU CAS PAR CAS  ---------------------
C
C     --- UNE RACINE TROUVEE ---
  110 CONTINUE
      H(IEN,IEN) = X+T
      WR(IEN) = H(IEN,IEN)
      WI(IEN) = 0.D0
      IEN = NA
      GO TO 15
C
C     --- DEUX RACINES TROUVEES ---
  115 CONTINUE
      P = (Y-X)*0.5D0
      Q = P*P+W
      ZZ = SQRT(ABS(Q))
      H(IEN,IEN) = X+T
      X = H(IEN,IEN)
      H(NA,NA) = Y+T
      IF (Q.LT.0.D0) GO TO 135
C
C     --- RACINES DOUBLES REELLES ---
      ZZ = P+SIGN(ZZ,P)
      WR(NA) = X+ZZ
      WR(IEN) = WR(NA)
      IF (ZZ.NE.0.D0) WR(IEN) = X-W/ZZ
      WI(NA) = 0.D0
      WI(IEN) = 0.D0
      X = H(IEN,NA)
C
C     --- SI X ET ZZ TROP PETIT ALORS ON FAIT UNE NORMALISATION   ---
C     --- MISE A L'ECHELLE OU RECADRAGE C'EST SELON VOTRE CULTURE ---
      SCALE = ABS(X) + ABS(ZZ)
      R = SCALE * SQRT( (X/SCALE)**2 + (ZZ/SCALE)**2 )
      P = X/R
      Q = ZZ/R
C
C     --- ALTERATION DES LIGNES ---
      DO 120 J=NA,NEQ
         ZZ = H(NA,J)
         H(NA,J) = Q*ZZ+P*H(IEN,J)
         H(IEN,J) = Q*H(IEN,J)-P*ZZ
  120 CONTINUE
C
C     --- ALTERATION DES COLONNES ---
      DO 125 I=1,IEN
         ZZ = H(I,NA)
         H(I,NA) = Q*ZZ+P*H(I,IEN)
         H(I,IEN) = Q*H(I,IEN)-P*ZZ
  125 CONTINUE
C
      IF (IZ.GE.NEQ) THEN
C        --- STOCKER LES TRANSFORMATIONS POUR LES VECTEURS ---
         DO 130 I=K,L
            ZZ = Z(I,NA)
            Z(I,NA) = Q*ZZ+P*Z(I,IEN)
            Z(I,IEN) = Q*Z(I,IEN)-P*ZZ
  130    CONTINUE
      ENDIF
      GO TO 140
C
C     --- VALEURS COMPLEXES CONJUGUEES ---
  135 CONTINUE
      WR(NA) = X+P
      WR(IEN) = X+P
      WI(NA) = ZZ
      WI(IEN) = -ZZ
  140 CONTINUE
      IEN = IENM2
      GO TO 15
C
C
C     --- TOUTES LES RACINES SONT TROUVEES, ON DEBUTE LA REMONTEE ---
  145 CONTINUE
      IF (IZ.LT.NEQ) GO TO 9999
C
C     ---- ON S'OCCUPE MAINTENANT DES VECTEURS ---
C
      IF (RNORM.EQ.0.D0) GO TO 9999
      DO 220 NN=1,NEQ
         IEN = NEQ+1-NN
         P = WR(IEN)
         Q = WI(IEN)
         NA = IEN-1
         IF (Q.GT.0.D0) GO TO 220
         IF (Q.LT.0.D0) GO TO 180
C
C        --- VECTEUR REEL ---
         M = IEN
         H(IEN,IEN) = 1.D0
         IF (NA.EQ.0) GO TO 220
         DO 175 II=1,NA
            I = IEN-II
            W = H(I,I)-P
            R = H(I,IEN)
            DO 150 J=M,NA
               R = R+H(I,J)*H(J,IEN)
  150       CONTINUE
            IF (WI(I).GE.0.D0) GO TO 160
            ZZ = W
            S = R
            GO TO 175
  160       CONTINUE
            M = I
            IF (WI(I).NE.0.D0) GO TO 165
            T = W
            IF (W.EQ.0.D0) T = EPSMAC*RNORM
            H(I,IEN) = -R/T
            GO TO 175
C
C           RESOLUTION DANS LE CAS REEL ---
  165       CONTINUE
            X = H(I,I+1)
            Y = H(I+1,I)
            Q = (WR(I)-P)*(WR(I)-P)+WI(I)*WI(I)
            T = (X*S-ZZ*R)/Q
            H(I,IEN) = T
            IF (ABS(X).LE.ABS(ZZ)) THEN
               H(I+1,IEN) = (-S-Y*T)/ZZ
            ELSE
               H(I+1,IEN) = (-R-W*T)/X
            ENDIF
  175    CONTINUE
         GO TO 220
C
C        --- CAS OU LE DERNIER VECTEUR EST IMAGINAIRE ---
  180    CONTINUE
         M = NA
C        --- VECTEUR COMPLEXE ---
         IF (ABS(H(IEN,NA)).LE.ABS(H(NA,IEN))) GO TO 185
         H(NA,NA) = Q/H(IEN,NA)
         H(NA,IEN) = -(H(IEN,IEN)-P)/H(IEN,NA)
         GO TO 190
  185    CONTINUE
         Z3 = DCMPLX(0.D0,-H(NA,IEN))/DCMPLX(H(NA,NA)-P,Q)
         H(NA,NA) =  DBLE (Z3)
         H(NA,IEN) = DIMAG(Z3)
  190    CONTINUE
         H(IEN,NA) = 0.D0
         H(IEN,IEN) = 1.D0
         IENM2 = NA-1
         IF (IENM2.EQ.0) GO TO 220
         DO 215 II=1,IENM2
            I = NA-II
            W = H(I,I)-P
            RA = 0.D0
            SA = H(I,IEN)
            DO 195 J=M,NA
               RA = RA+H(I,J)*H(J,NA)
               SA = SA+H(I,J)*H(J,IEN)
  195       CONTINUE
            IF (WI(I).LT.0.D0) THEN
               ZZ = W
               R  = RA
               S  = SA
            ELSEIF (WI(I).EQ.0.D0) THEN
               M  = I
               Z3 = DCMPLX(-RA,-SA)/DCMPLX(W,Q)
               H(I,NA)  = DBLE (Z3)
               H(I,IEN) = DIMAG(Z3)
            ELSE
C
C              --- RESOUDRE LES EQUATIONS (EN COMPLEXE)
               M = I
               X = H(I,I+1)
               Y = H(I+1,I)
               VR = (WR(I)-P)*(WR(I)-P)+WI(I)*WI(I)-Q*Q
               VI = (WR(I)-P)*Q
               VI = VI+VI
               IF (VR.EQ.0.D0.AND.VI.EQ.0.D0) VR=EPSMAC*RNORM*(ABS(W)
     +                                + ABS(Q)+ABS(X)+ABS(Y)+ABS(ZZ))
               Z3 = DCMPLX(X*R-ZZ*RA+Q*SA,X*S-ZZ*SA-Q*RA)/DCMPLX(VR,VI)
               H(I,NA)  = DBLE (Z3)
               H(I,IEN) = DIMAG(Z3)
               IF (ABS(X).LE.ABS(ZZ)+ABS(Q)) THEN
                  Z3 = DCMPLX(-R-Y*H(I,NA),-S-Y*H(I,IEN))/DCMPLX(ZZ,Q)
                  H(I+1,NA)  = DBLE (Z3)
                  H(I+1,IEN) = DIMAG(Z3)
               ELSE
                  H(I+1,NA) = (-RA-W*H(I,NA)+Q*H(I,IEN))/X
                  H(I+1,IEN) = (-SA-W*H(I,IEN)-Q*H(I,NA))/X
               ENDIF
            ENDIF
  215    CONTINUE
  220 CONTINUE
C     --- FIN DE LA REMONTEE ---
C
C     --- VECTEURS DES RACINES ISOLEES ---
      DO 230 I=1,NEQ
         IF (I.GE.K .AND. I.LE.L) GO TO 230
         DO 225 J=I,NEQ
            Z(I,J) = H(I,J)
  225    CONTINUE
  230 CONTINUE
      IF (L.EQ.0) GO TO 9999
C
C     APPLICATION DES TRANSFORMATIONS ---
      DO 245 JJ=K,NEQ
         J = NEQ+K-JJ
         M = MIN(J,L)
         DO 240 I=K,L
            ZZ = 0.D0
            DO 235 KA=K,M
               ZZ = ZZ+Z(I,KA)*H(KA,J)
  235       CONTINUE
            Z(I,J) = ZZ
  240    CONTINUE
  245 CONTINUE
      GO TO 9999
C
C     PAS DE CONVERGEBCE APRES "MXITER" INTERATION
C         ==>  IER = L'INDICE DE LA VALEUR PROPRE COURANTE
C
  250 CONTINUE
      IER = IEN
      DO 255 I=1,IEN
         WR(I) = 0.D0
         WI(I) = 0.D0
  255 CONTINUE
      IF (IZ.GE.NEQ) THEN
         DO 265 I=1,NEQ
            DO 260 J=1,NEQ
               Z(I,J) = 0.D0
  260       CONTINUE
  265    CONTINUE
      ENDIF
C     --- SORTIE ---
 9999 CONTINUE
      END

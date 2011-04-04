      SUBROUTINE VPINTE(OPTION,NFREQ,VALP,DET,IDET,IEME,NPAS,
     +                  TOLF, NITF, LRAIDE, LMASSE, LDYNAM,
     +                  RESUFI, RESUFR, NFREQB, SOLVEU)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16      OPTION
      INTEGER                                 IDET(*),IEME(*),NPAS(*)
      REAL*8                                                TOLF
      INTEGER           LRAIDE,LMASSE,LDYNAM,RESUFI(NFREQB,*)
      INTEGER                                          NFREQB
      REAL*8                     VALP(*),DET(*),RESUFR(NFREQB,*)
      CHARACTER*19      SOLVEU
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 04/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     METHODE D'INTERPOLATION DE VALEUR PROPRE COMPRISE ENTRE 2 BORNES
C     ------------------------------------------------------------------
C IN OPTION  : CH8 : OPTION D'INTERPOLATION
C          SEPARE ==> ON PREND LE MILIEU DE L'INTERVALLE
C          AJUSTE ==> ON EFFECTUE DES ITERATIONS DE TYPE SECANTE
C IN TOLF    : R8  : TOLERANCE POUR L'AJUSTEMENT DE FREQUENCE
C IN NITF    : R8  : NOMBRE MAXIMUM D'ITERATION DE LA METHODE
C IN  SOLVEU : K19 : SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
C     ------------------------------------------------------------------
C     METHODE D'INTERPOLATION DE DEGRE 1 (A DEUX POINTS)
C
C                   ZK * F(ZK-1) - ZK-1 * F(ZK)
C          ZK+1  =  ---------------------------
C                   WK-1 * F(ZK-1) - WK * F(ZK)
C     ------------------------------------------------------------------
C
      REAL*8  DET0, OM0, OM, PRECI, FTOL
      REAL*8  DD1, D1, F1, W1
      REAL*8  DD2, D2, F2, W2
C     ------------------------------------------------------------------
C
      IFREQ = 0
      DET0 = 0.D0
      IDET0 = 0
      IF (OPTION(1:6) .EQ. 'AJUSTE') THEN
C
         DO 1 JFREQ = 1,NFREQ - 1
            IEMEI = IEME(JFREQ+1) - IEME(JFREQ)
            IF (IEMEI.EQ.0) GO TO 1
            IF ( VALP(JFREQ) .EQ. 0.0D0 ) THEN
C              --- FREQUENCE NULLE  ---
               NBITER = 0
               VALEUR = VALP(JFREQ)
               PRECI  = 0.0D0
            ELSEIF (ABS(IEMEI).GT.1) THEN
C              --- FREQUENCES "MULTIPLES"  ---
               NBITER = 0
               VALEUR = (VALP(JFREQ)+VALP(JFREQ+1))*0.5D0
               PRECI  = (VALP(JFREQ+1)-VALP(JFREQ)) / VALEUR
               IF (JFREQ .EQ. 1 ) THEN
                  DO 110 II = 1, ABS(IEMEI)-1
                     IFREQ = IFREQ + 1
                     RESUFI(IFREQ,1)  = IEME(JFREQ+1)
                     RESUFI(IFREQ,2)  = MAX(NPAS(JFREQ),NPAS(JFREQ+1))
                     RESUFI(IFREQ,3)  = NBITER
                     RESUFI(IFREQ,4)  = NBITER
                     RESUFR(IFREQ,2)  = VALEUR
                     RESUFR(IFREQ,3)  = 0.0D0
                     RESUFR(IFREQ,14) = PRECI
  110             CONTINUE
               ENDIF
            ELSE
               OM0 = VALP(JFREQ)
               D1  = DET(JFREQ)*10.D0**(IDET(JFREQ)-IDET(JFREQ+1))
               D2  = DET(JFREQ+1)
               F1  = VALP(JFREQ)
               F2  = VALP(JFREQ+1)
               IP1 = 1
               IP2 = 1
               W1  = 1.D0
               W2  = 1.D0
               IER = 0
C
C              --- CALCUL DU ZERO ---
               DO 50 NITER = 1,NITF
                  ITRIG = 0
                  OM = (W1*F2*D1-W2*F1*D2)/ (W1*D1-W2*D2)
                  IF (NITER.LE.3) THEN
C                    --- CONTROLE DE NON SORTIE DES BORNES ---
                     FTOL = (F2-F1)/20.D0
                     IF (OM.LT.F1+FTOL) OM = F1 + FTOL
                     IF (OM.GT.F2-FTOL) OM = F2 - FTOL
                  ENDIF
 25               CONTINUE
                  IDET0 = 0
                  CALL VPSTUR(LRAIDE,OM,LMASSE,LDYNAM,
     +                        DET0,IDET0,IEME0,IER,SOLVEU)
                  PRECI = ABS(OM0-OM)/OM0
                  IF (PRECI.LE.TOLF.OR. IER.NE.0 ) THEN
                     NBITER = NITER
                     GO TO 60
                  ENDIF
                  OM0 = OM
                  IF ((DET0*D1).GE.0.D0) THEN
                     DD1 = DET0*10.D0** (IDET0-IDET(JFREQ+1))
                     IF (ABS(DD1).GT.ABS(D1) .AND. ITRIG.EQ.0) THEN
                        OM = (F1+F2)*0.5D0
                        ITRIG = 1
                        GO TO 25
                     ENDIF
                     D1 = DD1
                     F1 = OM
                     IP1 = IP1 + 1
                     IP2 = 1
                  ELSE
                     DD2 = DET0*10.D0** (IDET0-IDET(JFREQ+1))
                     IF (ABS(DD2).GT.ABS(D2) .AND. ITRIG.EQ.0) THEN
                        OM = (F1+F2)*0.5D0
                        ITRIG = 1
                        GO TO 25
                     ENDIF
                     D2 = DD2
                     F2 = OM
                     IP2 = IP2 + 1
                     IP1 = 1
                  ENDIF
                  W1 = 2** (((IP1-1)* (IP1-2))/2)
                  W2 = 2** (((IP2-1)* (IP2-2))/2)
 50            CONTINUE
               NBITER = -NITF
C
C              --- SORTIE DE LA BOUCLE SUR LES FREQUENCES ---
 60            CONTINUE
               IF (IER.NE.0 ) THEN
                  VALEUR = 1.01D0 * OM
               ELSE
                  VALEUR = (W1*F2*D1-W2*F1*D2)/ (W1*D1-W2*D2)
               ENDIF
            ENDIF
C
            IFREQ = IFREQ + 1
            RESUFI(IFREQ,1)  = IEME(JFREQ+1)
            RESUFI(IFREQ,2)  = MAX(NPAS(JFREQ),NPAS(JFREQ+1))
            RESUFI(IFREQ,3)  = NBITER
            RESUFI(IFREQ,4)  = NBITER
            RESUFR(IFREQ,2)  = VALEUR
            RESUFR(IFREQ,3)  = 0.0D0
            RESUFR(IFREQ,14) = PRECI
 1       CONTINUE
C
      ELSE
C
C        --- ON PREND LE MILIEU DE L'INTERVALLE ---
         NBITER = 0
          DO 200 JFREQ = 1,NFREQ - 1
            IF (ABS(IEME(JFREQ+1)-IEME(JFREQ)).GT.0) THEN
               IFREQ = IFREQ + 1
               VALEUR           = (VALP(JFREQ)+VALP(JFREQ+1))*0.5D0
               PRECI            = (VALP(JFREQ+1)-VALP(JFREQ))/VALEUR
               RESUFI(IFREQ,1)  = IEME(JFREQ+1)
               RESUFI(IFREQ,2)  = MAX(NPAS(JFREQ),NPAS(JFREQ+1))
               RESUFI(IFREQ,3)  = NBITER
               RESUFI(IFREQ,4)  = NBITER
               RESUFR(IFREQ,2)  = VALEUR
               RESUFR(IFREQ,3)  = 0.0D0
               RESUFR(IFREQ,14) = PRECI
            ENDIF
  200    CONTINUE

      ENDIF
C
C     --- MISE EN EXTENSION DES VALEURS PROPRES "MULTIPLES" ---
      IF ( ABS(RESUFI(IFREQ,1)-RESUFI(1,1)) .GT. IFREQ-1 ) THEN
         NFREQ = 0
         DO 300 JFREQ = 1, IFREQ-1
            NFREQ = NFREQ+1
            NTROU = ABS(RESUFI(NFREQ+1,1)-RESUFI(NFREQ,1))-1
            IF ( NTROU .GT.0) THEN
               DO 310 IDEC= NFREQ+IFREQ-JFREQ, NFREQ+1 ,-1
                  DO 312 JDEC =1,4
                     RESUFI(IDEC+NTROU,JDEC) = RESUFI(IDEC,JDEC)
  312             CONTINUE
                  DO 315 JDEC =1,14
                     RESUFR(IDEC+NTROU,JDEC) = RESUFR(IDEC,JDEC)
  315             CONTINUE
  310          CONTINUE
               DO 320 IDEC=  2, NTROU
                  DO 322 JDEC =1,4
                     RESUFI(NFREQ+IDEC,JDEC) = RESUFI(NFREQ+1,JDEC)
  322             CONTINUE
                  DO 325 JDEC =1,14
                     RESUFR(NFREQ+IDEC,JDEC) = RESUFR(NFREQ+1,JDEC)
  325             CONTINUE
  320          CONTINUE
               NFREQ = NFREQ + NTROU
            ENDIF
  300    CONTINUE
         NFREQ = NFREQ + 1
      ELSE
C        --- PAS DE VALEURS PROPRES "MULTIPLES" ---
         NFREQ = IFREQ
      ENDIF

C
      END

      SUBROUTINE HUJJID( MOD, MATER, INDI, DEPS, PROX, PROXC,
     &                   YD, YF, VIND, R, DRDY, IRET )
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/04/2013   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_20
C  --------------------------------------------------------------------
C  INTEGRATION PLASTIQUE (MECANISME DEVIATOIRE SEUL) DE LA LOI HUJEUX
C
C  RESOLUTION PAR METHODE DE NEWTON   DRDY(DY).DDY = - R(DY)
C
C  CALCUL DU SECOND MEMBRE : - R(DY)
C  CALCUL DU JACOBIEN      : DRDY(DY)
C  DY   =  ( SIG     , ESPVP     , R       , LAMBDA   )
C  R    = -( LE      , LEVP      , LR      , LF       )
C  DRDY =  ( DLEDS   , DLEDEVP   , DLEDR   , DLEDLA   )
C          ( DLEVPDS , DLEVPDEVP , DLEVPDR , DLEVPDLA )
C          ( DLRDS   , DLRDEVP   , DLRDR   , DLRDLA   )
C          ( DLFDS   , DLFDEVP   , DLFDR   , DLFDLA   )
C =====================================================================
C  IN   MOD   :  MODELISATION
C       MATER :  COEFFICIENTS MATERIAU
C       INDI  :  INDICE DES MECANISMES SUPPOSES ACTIFS
C       DEPS  :  INCREMENT DE DEFORMATION
C       YD    :  VARIABLES A T = (SIGD, VIND, DLAMBDAD)
C       YF    :  VARIABLES A T+DT = (SIGF, VINF, DLAMBDAF)
C       VIND  :  VARIABLES INTERNES A T
C  VAR  IND   :  TABLEAU DES NUMEROS DE MECANISMES ACTIFS
C  OUT  R     :  SECOND MEMBRE
C       DRDY  :  JACOBIEN
C       IRET  :  CODE RETOUR
C                = 0 OK
C                = 1 NOOK : SI LA SUBDIVISION DU PAS DE TEMPS EST ACTIV
C                           DANS STAT_NON_LINE, IL Y A SUBDIVISION
C =====================================================================
C =====================================================================
      INCLUDE 'jeveux.h'
      INTEGER     NDT, NDI, NMOD, I, J, K, KK, L
      INTEGER     INDI(7), NBMECA, IRET, IADZI, IAZK24
      INTEGER     IFM, NIV, NBMECT
      PARAMETER   (NMOD = 18)
      REAL*8      DEPSP(6), DEPSE(6)
      REAL*8      SIGD(3),SIGF(6),P(7),Q(7)
      REAL*8      YD(NMOD), YF(NMOD), DRDY(NMOD,NMOD)
      REAL*8      MATER(22,2), N, BETA, D, M, PCO, PREF, PC
      REAL*8      DEGR, PHI, ANGDIL, MDIL, B, DKSIDR(7)
      REAL*8      RC(7), DLAMBD(7), DEPSDS(6,6)
      REAL*8      HOOKNL(6,6), HOOK(6,6), DHOKDS(6,6)
      REAL*8      I1F, E, NU, AL, DEMU, COEF0, DCOEF0
      REAL*8      LE(6), LEVP, LR(4), LF(7), R(NMOD), DELTA(6)
      REAL*8      DLEDS(6,6), DLEDEV(6), DLEDR(6,4), DLEDLA(6,7)
      REAL*8      DLEVDS(6), DLEVDE, DLEVDR(4), DLEVDL(7)
      REAL*8      DLRDS(4,6), DLRDLE(4), DLRDR(4,4), DLRDLA(4,7)
      REAL*8      DLFDS(7,6), DLFDLE(7), DLFDR(7,4), DLFDLA(7,7)
      REAL*8      CDE(6), CTILD(6), CD2FDS(6,6)
      REAL*8      DLEDR1(6), PSI(42), AD(7), KSI(7)
      REAL*8      DPSIDS(6,6), DFDS(6), DLEK(6)
      REAL*8      TRACE, EPSVP, DEPS(6), TH(2), PROD
      REAL*8       ACYC, AMON, CMON, CCYC, XH(2)
      REAL*8      ZERO, UN, D12, D13, DEUX, LA, ALPHA
      REAL*8      TOLE1, COEF, MUL, CCOND, VIND(*), SI, SIGDC(9)
      REAL*8      PRODC, PRODM, PS, SCXH, SXH, FAC
      REAL*8      E1,E2,E3,NU12,NU13,NU23,G1,G2,G3,NU21,NU31,NU32
      REAL*8      PTRAC, PISO, PK, DPSI, DENOM, PCOH
      REAL*8      SC(6), TC(6), XC(6), SCXC, XCTC, RTRAC
      CHARACTER*8 MOD, NOMAIL
      LOGICAL     DEBUG, PROX(4), PROXC(4), DILA
C =====================================================================
      PARAMETER   ( D12    = 0.5D0  )
      PARAMETER   ( D13    = 0.333333333334D0  )
      PARAMETER   ( UN     = 1.D0   )
      PARAMETER   ( ZERO   = 0.D0   )
      PARAMETER   ( DEUX   = 2.D0   )
      PARAMETER   ( TOLE1   = 1.D-7 )
      PARAMETER   ( DEGR = 0.0174532925199D0 )

C =====================================================================
      COMMON /TDIM/   NDT, NDI
      COMMON /MESHUJ/ DEBUG
C =====================================================================
      CALL INFNIV(IFM,NIV)
C =====================================================================
C --- PROPRIETES HUJEUX MATERIAU --------------------------------------
C =====================================================================
      N      = MATER(1,2)
      BETA   = MATER(2,2)
      D      = MATER(3,2)
      B      = MATER(4,2)
      PHI    = MATER(5,2)
      ANGDIL = MATER(6,2)
      PCO    = MATER(7,2)
      PREF   = MATER(8,2)
      ACYC   = MATER(9,2)
      AMON   = MATER(10,2)
      CCYC   = DEUX*MATER(11,2)
      CMON   = MATER(12,2)
      M      = SIN(DEGR*PHI)
      MDIL   = SIN(DEGR*ANGDIL)
      COEF   = MATER(20,2)
      ALPHA  = COEF*D12
      PTRAC  = MATER(21,2)
      PISO   = ZERO

C --- PARAMETRE NECESSAIRE POUR GERER LA TRACTION
      RTRAC = 1.D-6 * ABS(PREF)
C =====================================================================
C --- PREMIER INVARIANT ET AUTRES GRANDEURS UTILES --------------------
C =====================================================================
      I1F  = D13 * TRACE(NDI,YF)
      IF ( (I1F/PREF) .LT. TOLE1 ) I1F = TOLE1*PREF

      DO 11 I = 1, 4
        PROX(I)  = .FALSE.
        PROXC(I) = .FALSE.
 11     CONTINUE

      DO 6 I = 1, NDT
        SIGF(I) = YF(I)
        PSI(I) = ZERO
        PSI(NDT+I)  = ZERO
        PSI(2*NDT+I) = ZERO
        PSI(3*NDT+I) = ZERO
        PSI(4*NDT+I) = ZERO
        PSI(5*NDT+I) = ZERO
        PSI(6*NDT+I) = ZERO
 6      CONTINUE

      DO 3 I = 1, 9
        SIGDC(I)=ZERO
  3     CONTINUE

      NBMECA = 0
      NBMECT = 0
      DO 4 K = 1, 7
        IF (INDI(K) .GT. 0) THEN
          NBMECT = NBMECT + 1
          IF (INDI(K).LE.8) NBMECA = NBMECA + 1
        ENDIF
        DLAMBD(K) = ZERO
        AD(K)     = ZERO
        KSI(K)    = ZERO
        Q(K)      = ZERO
        P(K)      = ZERO
 4      CONTINUE

      DO 5 K = 1, NBMECT
        KK = INDI(K)

        DLAMBD(K) = YF(NDT+1+NBMECA+K)

        IF (KK.LE.8) RC(K) = YF(NDT+1+K)

        CALL HUJDDD('PSI   ', INDI(K), MATER, INDI, YF, VIND,
     &              PSI((K-1)*NDT+1), DPSIDS, IRET)
        IF (IRET .EQ. 1) GOTO 1000

        IF (INDI(K) .LT. 4) THEN

          CALL HUJPRJ (INDI(K), SIGF, SIGD, P(K), Q(K))
          IF (P(K) .GE. PTRAC) GOTO 999
          CALL HUJKSI('DKSIDR', MATER, RC(K), DKSIDR(K), IRET)
          CALL HUJKSI('KSI   ', MATER, RC(K), KSI(K), IRET)
          IF (IRET.EQ.1) GOTO 1000
          AD(K)  = ACYC+KSI(K)*(AMON-ACYC)

        ELSEIF (INDI(K) .EQ. 4) THEN

          KSI(K) = UN
          P(K)   = (YF(1)+YF(2)+YF(3))*D13

        ELSEIF ((INDI(K) .LT. 8) .AND. (INDI(K) .GT. 4)) THEN

          CALL HUJPRC (K, INDI(K)-4, SIGF, VIND, MATER, YF,
     &                 P(K), Q(K), SIGDC(3*K-2))
          IF (P(K) .GE. PTRAC) GOTO 999
          CALL HUJKSI('DKSIDR', MATER, RC(K), DKSIDR(K), IRET)
          CALL HUJKSI('KSI   ', MATER, RC(K), KSI(K), IRET)
          IF(IRET.EQ.1) GOTO 1000
          AD(K) = DEUX*(ACYC+KSI(K)*(AMON-ACYC))

          TH(1) = VIND(4*INDI(K)-9)
          TH(2) = VIND(4*INDI(K)-8)
          PROD  = SIGDC(3*K-2)*TH(1) + SIGDC(3*K)*TH(2)/DEUX

          IF ((-Q(K)/PREF.LT.TOLE1).OR.((UN+PROD/Q(K)).LT.1.D-2)) THEN
             KK = KK - 4
             CALL HUJPXD(INDI(K),MATER,SIGF,VIND,PROX(KK),PROXC(KK))
          ELSE
            AD(K) = (ACYC+KSI(K)*(AMON-ACYC))*(UN+PROD/Q(K))
          ENDIF

        ELSEIF (INDI(K) .EQ. 8) THEN

          KSI(K) = UN
          CALL HUJPIC(K, INDI(K),SIGF, VIND, MATER, YF, P(K))

        ELSEIF ((INDI(K).GT.8).AND.(INDI(K).LT.12)) THEN
          GOTO 5

        ELSE
          CALL U2MESS('F', 'COMPOR1_8')
        ENDIF

 5      CONTINUE

      EPSVP = YF(NDT+1)
      PC    = PCO*EXP(-BETA*EPSVP)
      CMON = CMON * PC/PREF
      CCYC = CCYC * PC/PREF

C --- CONDITIONNEMENT DE LA MATRICE JACOBIENNE
      CCOND= MATER(1,1)
C =====================================================================
C --- OPERATEURS DE RIGIDITE ET DE SOUPLESSE (LINEAIRES OU NON LINEA.)
C =====================================================================
C --- OPERATEURS LINEAIRES --------------------------------------------
C =====================================================================
      CALL LCINMA (ZERO, HOOK)

      IF (MOD(1:2) .EQ. '3D'     .OR.
     &    MOD(1:6) .EQ. 'D_PLAN' .OR.
     &    MOD(1:4) .EQ. 'AXIS')  THEN

        IF (MATER(17,1).EQ.UN) THEN

          E    = MATER(1,1)
          NU   = MATER(2,1)
          AL   = E*(UN-NU) /(UN+NU) /(UN-DEUX*NU)
          DEMU = E     /(UN+NU)
          LA   = E*NU/(UN+NU)/(UN-DEUX*NU)

          DO 30 I = 1, NDI
            DO 30 J = 1, NDI
              IF (I.EQ.J) HOOK(I,J) = AL
              IF (I.NE.J) HOOK(I,J) = LA
 30           CONTINUE
          DO 35 I = NDI+1, NDT
            HOOK(I,I) = DEMU
 35         CONTINUE

        ELSEIF (MATER(17,1).EQ.DEUX) THEN

          E1   = MATER(1,1)
          E2   = MATER(2,1)
          E3   = MATER(3,1)
          NU12 = MATER(4,1)
          NU13 = MATER(5,1)
          NU23 = MATER(6,1)
          G1   = MATER(7,1)
          G2   = MATER(8,1)
          G3   = MATER(9,1)
          NU21 = MATER(13,1)
          NU31 = MATER(14,1)
          NU32 = MATER(15,1)
          DENOM= MATER(16,1)

          HOOK(1,1) = (UN - NU23*NU32)*E1/DENOM
          HOOK(1,2) = (NU21 + NU31*NU23)*E1/DENOM
          HOOK(1,3) = (NU31 + NU21*NU32)*E1/DENOM
          HOOK(2,2) = (UN - NU13*NU31)*E2/DENOM
          HOOK(2,3) = (NU32 + NU31*NU12)*E2/DENOM
          HOOK(3,3) = (UN - NU21*NU12)*E3/DENOM
          HOOK(2,1) = HOOK(1,2)
          HOOK(3,1) = HOOK(1,3)
          HOOK(3,2) = HOOK(2,3)
          HOOK(4,4) = G1
          HOOK(5,5) = G2
          HOOK(6,6) = G3

        ELSE
          CALL U2MESS('F', 'COMPOR1_38')
        ENDIF


C =====================================================================
C --- CP/1D -----------------------------------------------------------
C =====================================================================
      ELSEIF (MOD(1:6) .EQ. 'C_PLAN' .OR.
     &        MOD(1:2) .EQ. '1D')   THEN
        CALL U2MESS('F', 'COMPOR1_4')
      ENDIF


C =====================================================================
C --- OPERATEUR NON LINEAIRE ------------------------------------------
C =====================================================================
      COEF0 = ((I1F -PISO)/PREF) ** N
      DO 40 I = 1, NDT
        DO 40 J = 1, NDT
          HOOKNL(I,J) = COEF0*HOOK(I,J)
 40       CONTINUE


C =====================================================================
C --- DERIVEE PAR RAPPORT A DS DE L'OPERATEUR NON LINEAIRE: DHOOKDS ---
C =====================================================================
      DCOEF0 = D13*N/PREF * ((I1F -PISO)/PREF)**(N-1)
      DO 41 I = 1, NDT
        DO 41 J = 1, NDT
          DHOKDS(I,J) = DCOEF0*HOOK(I,J)
 41       CONTINUE


C =====================================================================
C --- I. CALCUL DE DLEDS (6X6) ----------------------------------------
C =====================================================================
C ---> I.1. CALCUL DE CTILD = DHOOKDS*(DEPS - DEPSP)
C ---> I.1.1. CALCUL DE DEPSP A T+DT
      DO 50 I = 1,NDT
        DEPSP(I) = ZERO
 50    CONTINUE

      DO 51 K = 1, NBMECT
        KK = (K-1)*NDT
        DO 53 I = 1, NDT
          DEPSP(I) = DEPSP(I) + DLAMBD(K)*PSI(KK+I)
 53       CONTINUE
 51     CONTINUE

C ------------ FIN I.1.1.
       DO 52 I = 1, NDT
         DEPSE(I) = DEPS(I) - DEPSP(I)
 52      CONTINUE
       CALL LCPRMV (DHOKDS, DEPSE, CTILD)
C ------------ FIN I.1.
C ---> I.2. CALCUL DE CD2FDS = HOOK * DEPSDS
C                     (6X6)    (6X6)  (6X6)
       CALL LCINMA (ZERO, DEPSDS)

       DO 60 K = 1, NBMECT
         KK = INDI(K)
         IF ((KK .EQ. 4).OR.(KK.EQ.8)) GOTO 610

         IF(KK.GT.8) GOTO 610

         CALL HUJDDD('DPSIDS', KK, MATER, INDI, YF, VIND,
     &               DFDS, DPSIDS, IRET)
         IF (IRET.EQ.1) GOTO 1000
         DO 60 I = 1, NDT
           DO 60 J = 1, NDT
             DEPSDS(I,J) = DEPSDS(I,J) + DLAMBD(K)*DPSIDS(I,J)
 60          CONTINUE
 610   CONTINUE

       CALL LCPRMM (HOOKNL, DEPSDS, CD2FDS)

C ------------ FIN I.2.
        CALL LCINMA (ZERO, DLEDS)
        DO 63 I = 1, NDT
          DLEDS(I,I) = UN
 63       CONTINUE

       DO 61 I = 1, NDT
         DO 62 J = 1, NDI
           DLEDS(I,J) = DLEDS(I,J) - (CTILD(I) - CD2FDS(I,J))
 62        CONTINUE
          DO 61 J = NDI+1, NDT
            DLEDS(I,J) = DLEDS(I,J) + CD2FDS(I,J)
  61        CONTINUE

C =====================================================================
C --- II. CALCUL DE DLEDR (6XNBMEC) -----------------------------------
C =====================================================================

      DO 70 I = 1, NDT
        DO 70 K = 1, 4
           DLEDR(I,K) = ZERO
 70        CONTINUE

      IF(NBMECA.EQ.0)GOTO 710

      DO 71 K = 1, NBMECA
         KK = INDI(K)
         PK = P(K) -PTRAC

         IF ((KK.EQ.4) .OR. (KK.EQ.8)) GOTO 710
         IF (KK .GT. 8) GOTO 710

         IF (KK .LT. 4) THEN

Ckh --- traction
         IF ((P(K)/PREF).GT.TOLE1) THEN
           DPSI =MDIL+Q(K)/P(K)
         ELSE
           IF (DEBUG) WRITE(6,'(A)')'HUJJID :: TRACTION MONOTONE'
           DPSI =MDIL+1.D+6*Q(K)/PREF
         ENDIF

         MUL = - DLAMBD(K)*ALPHA*DPSI*DKSIDR(K)

         DO 72 I = 1, NDI
           IF (I .NE. KK) THEN
             DELTA(I) = MUL
           ELSE
             DELTA(I) = ZERO
           ENDIF
 72        CONTINUE

         DO 73 I = NDI+1, NDT
           DELTA(I) = ZERO
 73        CONTINUE

         ELSEIF ((KK .LT. 8) .AND. (KK .GT. 4)) THEN
C ---> MECANISME CYCLIQUE DEVIATOIRE

           TH(1) = VIND(4*KK-9)
           TH(2) = VIND(4*KK-8)
           CALL HUJPRJ (INDI(K)-4, SIGF, SIGD, PS, PROD)
           PRODC  = 2.D0*SIGDC(3*K-2)*TH(1) + SIGDC(3*K)*TH(2)
           PRODM  = 2.D0*SIGD(1)*TH(1) + SIGD(3)*TH(2)
           PS     = 2.D0*SIGDC(3*K-2)*SIGD(1)+SIGDC(3*K)*SIGD(3)

Ckh --- traction
           IF ((P(K)/PREF).GT.TOLE1) THEN
             IF ((-Q(K)/PREF).GT.TOLE1) THEN
               DPSI =MDIL+PS/2.D0/P(K)/Q(K)
             ELSE
               DPSI =MDIL
             ENDIF
           ELSE
             IF (DEBUG) WRITE(6,'(A)')'HUJJID :: TRACTION CYCLIQUE'
             IF ((-Q(K)/PREF).GT.TOLE1) THEN
               DPSI = MDIL+PS/2.D-6/PREF/Q(K)
             ELSE
               DPSI = MDIL
             ENDIF
           ENDIF

           SI = UN
           DO 74 I = 1, NDI
             IF (I .NE. (KK-4)) THEN
               IF((-Q(K)/PREF).GT.TOLE1)THEN
                 DELTA(I) = DLAMBD(K)*(M*PK*(UN-B*LOG(PK/PC))/
     &                  (2.D0*Q(K))*(TH(1)*SI-SIGDC(3*K-2)*SI*PRODC/
     &                  (2.D0*Q(K)**2.D0))    -ALPHA*
     &                  (DKSIDR(K)*DPSI+
     &                  KSI(K)/2.D0*M*(UN-B*LOG(PK/PC))*(PRODM
     &                  -PS*PRODC/(2.D0*Q(K)**2.D0))/Q(K)))
               ELSE
                 DELTA(I) = DLAMBD(K)*(-ALPHA)*DKSIDR(K)*MDIL
               ENDIF
               SI = - SI
             ELSE
               DELTA(I) = ZERO
             ENDIF
 74          CONTINUE

           DO 75 I = NDI+1, NDT
             DELTA(I) = ZERO
 75          CONTINUE

           IF((-Q(K)/PREF).GT.TOLE1)THEN
             DELTA(NDT+5-KK)= DLAMBD(K)*(M*PK*(UN-B*LOG(PK/PC))/
     &                      (2.D0*Q(K))*(TH(2)-SIGDC(3*K)*PRODC/
     &                      (2.D0*Q(K)**2.D0)))
           ELSE
             DELTA(NDT+5-KK)= ZERO
           ENDIF

         ENDIF

         CALL LCPRMV (HOOKNL, DELTA, DLEDR1)
         DO 76 I = 1, NDT
           DLEDR(I,K) = DLEDR1(I) /ABS(PREF)
 76        CONTINUE

 71   CONTINUE

 710  CONTINUE

C =====================================================================
C --- III. CALCUL DE DLEDEVP (6X1) ------------------------------------
C =====================================================================
      DO 80 I = 1, NDT
        DLEDEV(I) = ZERO
 80     CONTINUE


C =====================================================================
C --- IV. CALCUL DE DLEDLA (6XNBMEC) ----------------------------------
C =====================================================================
      DO 90 K = 1, 6
        DO 90 L = 1, 7
          DLEDLA(K,L) = ZERO
 90       CONTINUE

      DO 91 K = 1, NBMECT
         KK = (K-1)*NDT+1
         CALL LCPRMV (HOOKNL, PSI(KK), DLEK)
         DO 91 I = 1, NDT
           DLEDLA(I,K) = DLEK(I) /CCOND
 91        CONTINUE

C =====================================================================
C --- V. CALCUL DE DLRDS (NBMECX6) ------------------------------------
C =====================================================================
      DO 100 K = 1, 4
        DO 100 I = 1, NDT
          DLRDS(K,I) = ZERO
 100      CONTINUE

C =====================================================================
C --- VI. CALCUL DE DLRDR (NBMECXNBMEC) -------------------------------
C =====================================================================
      DO 110 K = 1, 4
        DO 110 L = 1, 4
          DLRDR(K,L) = ZERO
 110      CONTINUE

      IF(NBMECA .EQ.0)GOTO 101

      DO 111 K = 1, NBMECA

        KK = INDI(K)

        IF (KK .LT. 4) THEN
          MUL        = (UN-RC(K))/AD(K)
          DLRDR(K,K) = UN + DEUX*DLAMBD(K)*MUL
     &                 + DLAMBD(K)*DKSIDR(K)*(AMON-ACYC)*MUL**DEUX
        ELSEIF (KK.EQ.4) THEN
          DLRDR(K,K) = UN + DEUX*DLAMBD(K)*(UN-RC(K))/CMON

        ELSEIF ((KK .GT. 4) .AND. (KK .LT. 8)) THEN
          MUL        = (UN-RC(K))/AD(K)
          DLRDR(K,K) = UN + DEUX*DLAMBD(K)*(MUL
     &                 + DKSIDR(K)*(AMON-ACYC)*MUL**DEUX)

        ELSEIF (KK .EQ. 8) THEN
          DLRDR(K,K) = UN + DEUX*DLAMBD(K)*(UN-RC(K))/CCYC

        ENDIF

C        DLRDR(K,K) = DLRDR(K,K)*CCOND/PREF
        DLRDR(K,K) = DLRDR(K,K)

 111    CONTINUE

 101    CONTINUE

C =====================================================================
C --- VII. CALCUL DE DLRDLA (NBMECXNBMEC) -----------------------------
C =====================================================================
      DO 112 K = 1, 4
        DO 112 L = 1, 7
          DLRDLA(K,L) = ZERO
 112      CONTINUE

      IF (NBMECA .EQ.0) GOTO 102

      DO 113 K = 1, NBMECA
        KK = INDI(K)
        IF (KK .LT. 4) THEN
          DLRDLA(K,K) = -( UN-RC(K) )**DEUX /AD(K)
        ELSEIF (KK.EQ.4) THEN
          DLRDLA(K,K) = -( UN-RC(K) )**DEUX /CMON

        ELSEIF ((KK .GT. 4) .AND. (KK .LT. 8)) THEN
          DLRDLA(K,K) = -( UN-RC(K) )**DEUX /AD(K)

        ELSEIF (KK .EQ. 8) THEN
          DLRDLA(K,K) = -( UN-RC(K) )**DEUX /CCYC

        ENDIF
        DLRDLA(K,K) = DLRDLA(K,K)/CCOND*ABS(PREF)
 113  CONTINUE

 102    CONTINUE

C =====================================================================
C --- VIII. CALCUL DE DLRDEVP (NBMECX1) -------------------------------
C =====================================================================
      DO 103 K = 1, 4
        DLRDLE(K) = ZERO
 103    CONTINUE

      IF (NBMECA .EQ.0) GOTO 104

      DO 120 K = 1, NBMECA
        KK = INDI(K)
        IF (KK .LT. 4) THEN
          DLRDLE(K) = ZERO
        ELSEIF (KK .EQ. 4) THEN
          DLRDLE(K) = -DLAMBD(K)*BETA*( UN-RC(K) )**DEUX /CMON

        ELSEIF ((KK .GT. 4) .AND. (KK .LT. 8)) THEN

C --- INITIALISATION DES VARIABLES D'HISTOIRE
          XH(1) = VIND(4*KK-11)
          XH(2) = VIND(4*KK-10)
          TH(1) = VIND(4*KK-9)
          TH(2) = VIND(4*KK-8)

C --- CALCUL DE F = M(1-BLOG((PK-PTRAC)/PC))
          PK =P(K) -PTRAC
          FAC = M*B*PK*BETA

C --- CALCUL DE D(SIG-CYC)/D(EVP)
          DO 121 I = 1, NDT
            XC(I) = ZERO
            SC(I) = ZERO
            TC(I) = ZERO
 121      CONTINUE

          SI = UN
          DO 122 I = 1, NDI
            IF(I.NE.(KK-4))THEN
              SC(I) = SIGDC(3*K-2)*SI
              TC(I) = TH(1)*SI
              XC(I) = XH(1)*SI
              SI    = -SI
            ENDIF
 122      CONTINUE
          SC(NDT+5-KK)    = SIGDC(3*K)
          TC(NDT+5-KK)    = TH(2)
          XC(NDT+5-KK)    = XH(2)

          SCXC = ZERO
          XCTC = ZERO
          DO 123 I = 1, NDT
            XC(I) = FAC*(XC(I)-TC(I)*RC(K))
            SCXC  = SCXC + SC(I)*XC(I)
            XCTC  = XCTC + XC(I)*TC(I)
 123      CONTINUE

C --- CALCUL DU PRODUIT SCALAIRE ENTRE TH ET SIG-CYC
          PROD  = DEUX*SIGDC(3*K-2)*TH(1) + SIGDC(3*K)*TH(2)

C --- CALCUL DE DLRDLE
          DLRDLE(K) = ZERO
         IF ((Q(K).GT.TOLE1).AND.((2.D0*Q(K)+PROD).GT.TOLE1)) THEN
           DLRDLE(K) = -DLAMBD(K)*(UN-RC(K))**DEUX/AD(K)
     &                  *(UN+PROD/(DEUX*Q(K)))
     &                 *(SCXC/Q(K)*PROD-2.D0*Q(K)*XCTC)
     &                 /(2.D0*Q(K)+PROD)**2.D0

         ENDIF

        ELSEIF (KK. EQ. 8) THEN
          DLRDLE(K) = -DLAMBD(K)*BETA*( UN-RC(K) )**DEUX /CCYC

        ENDIF
        DLRDLE(K) = DLRDLE(K)*ABS(PREF)/CCOND
 120  CONTINUE

 104    CONTINUE
C =====================================================================
C --- IX. CALCUL DE DLEVPDS (1X6) -------------------------------------
C =====================================================================
       DO 130 I = 1, NDT
         DLEVDS(I) = ZERO
 130     CONTINUE

      DO 131 K = 1, NBMECT
        KK =INDI(K)
        PK =P(K) -PTRAC
        IF ((KK.EQ.4) .OR. (KK.EQ.8)) GOTO 1310
        IF (KK.GT.8) GOTO 1310

Ckh --- traction
        IF ((P(K)/PREF).LT.TOLE1) THEN
          DILA =.TRUE.
          PCOH = 1.D-6*PREF
        ELSE
          DILA =.FALSE.
          PCOH = P(K)
        ENDIF

        IF (KK .LT. 4) THEN

        CALL HUJPRJ (KK, SIGF, SIGD, COEF0, MUL)
        IF ((-Q(K)/PREF) .LE. TOLE1) GOTO 131
        DLEVDS(NDT+1-KK) = DLEVDS(NDT+1-KK) +
     &  DLAMBD(K) * KSI(K)*COEF*SIGD(3) /PCOH/Q(K)/2.D0

        SI = UN
        DO 132 I = 1, NDI
          IF (I.NE.KK .AND. (.NOT.DILA)) THEN
            DLEVDS(I) = DLEVDS(I) +
     &      DLAMBD(K)*KSI(K)*COEF*(SIGD(1)*SI /P(K)/Q(K)/2.D0
     &      - D12*Q(K) /P(K)**DEUX)
            SI = -SI
           ELSEIF (I.NE.KK .AND. DILA) THEN
            DLEVDS(I) = DLEVDS(I) +
     &      DLAMBD(K)*KSI(K)*COEF*SIGD(1)*SI /PCOH/Q(K)/2.D0
            SI = -SI
          ENDIF
 132      CONTINUE

        ELSEIF ((KK. LT. 8) .AND. (KK. GT. 4)) THEN

          IF ((-Q(K)/PREF) .LE. TOLE1) GOTO 131

          CALL HUJPRJ(KK-4, SIGF, SIGD, COEF0, MUL)
          PS = 2.D0*SIGD(1)*SIGDC(3*K-2)+SIGD(3)*SIGDC(3*K)

          XH(1) = VIND(4*KK-11)
          XH(2) = VIND(4*KK-10)
          TH(1) = VIND(4*KK-9)
          TH(2) = VIND(4*KK-8)

          SXH = 2*SIGD(1)*(XH(1)-TH(1)*RC(K))+
     &           SIGD(3)*(XH(2)-TH(2)*RC(K))
          SCXH  = 2*SIGDC(3*K-2)*(XH(1)-TH(1)*RC(K))+
     &           SIGDC(3*K)*(XH(2)-TH(2)*RC(K))

          FAC = D12*M*(UN-B*(UN+LOG(PK/PC)))

          IF ((-Q(K)/PREF).GT.TOLE1) THEN
            DLEVDS(NDT+5-KK) = DLEVDS(NDT+5-KK) +
     &      DLAMBD(K) * KSI(K)*COEF/(2.D0*PCOH*Q(K))*
     &      (SIGD(3)+SIGDC(3*K)*(UN-PS/Q(K)**2.D0/2.D0))
          ENDIF

          SI = UN
          DO 133 I = 1, NDI
            IF (I .NE. (KK-4)) THEN
              IF ((-Q(K)/PREF).GT.TOLE1) THEN
                DLEVDS(I) = DLEVDS(I) +
     &          DLAMBD(K)*KSI(K)*COEF/(2.D0*PCOH*Q(K))*
     &          (SIGDC(3*K-2)*SI*(UN-PS/(2.D0*Q(K)**2.D0))+SIGD(1)*SI
     &          -FAC*(SXH-SCXH*PS*D12/Q(K)**2.D0)-D12*PS /PCOH)
                SI = -SI
              ENDIF
            ENDIF
 133      CONTINUE
        ENDIF

        DO 134 I = 1, NDT
          DLEVDS(I) = DLEVDS(I)/CCOND
 134    CONTINUE

 131    CONTINUE
 1310   CONTINUE
C =====================================================================
C --- X. CALCUL DE DLEVPDEVP (1X1) ------------------------------------
C =====================================================================
       DLEVDE = UN
       DO 140 K = 1, NBMECA
         KK = INDI(K)
         IF((KK.GT.4).AND.(KK.LT.8))THEN

           CALL HUJPRJ( KK-4, SIGF, SIGD, COEF0, MUL)
           IF (Q(K).GT.TOLE1) THEN
             XH(1) = VIND(4*KK-11)
             XH(2) = VIND(4*KK-10)
             TH(1) = VIND(4*KK-9)
             TH(2) = VIND(4*KK-8)
             PRODC  = 2.D0*SIGDC(3*K-2)*(XH(1)-RC(K)*TH(1)) +
     &               (SIGDC(3*K)*(XH(2)-RC(K)*TH(2)))
             PRODM  = 2.D0*SIGD(1)*(XH(1)-RC(K)*TH(1)) +
     &               (SIGD(3)*(XH(2)-RC(K)*TH(2)))
             PS     = 2.D0*SIGD(1)*SIGDC(3*K-2)+SIGD(3)+SIGDC(3*K)
             IF ((-Q(K)/PREF).GT.TOLE1) THEN
               DLEVDE = DLEVDE + DLAMBD(K)*COEF*KSI(K)/Q(K)/2.D0*
     &                         M*B*BETA*(PRODM - PS/2.D0/Q(K)**2.D0*
     &                         PRODC)
             ENDIF
           ENDIF
         ENDIF
 140   CONTINUE

C =====================================================================
C --- XI. CALCUL DE DLEVPDR (1XNBMEC) ---------------------------------
C =====================================================================
      DO 141 I = 1, 4
        DLEVDR(I) = ZERO
 141  CONTINUE

      IF (NBMECA.EQ.0) GOTO 152

      DO 151 K = 1, NBMECA

        KK = INDI(K)
        PK =P(K) -PTRAC

        IF (KK .LT. 4) THEN

Ckh --- traction
          IF ((P(K)/PREF).GT.TOLE1) THEN
            DPSI =MDIL+Q(K)/P(K)
          ELSE
            IF (DEBUG) WRITE(6,'(A)')'HUJJID :: TRACTION MONOTONE'
            DPSI =MDIL+1.D+6*Q(K)/PREF
          ENDIF

          DLEVDR(K) =
     &    DLAMBD(K)*COEF*DKSIDR(K)*DPSI

        ELSEIF (KK .EQ. 4) THEN

          DLEVDR(K) = ZERO

        ELSEIF ((KK .GT. 4) .AND. (KK .LT.8)) THEN

          CALL HUJPRJ(KK-4, SIGF, SIGD, COEF0, MUL)
          TH(1) = VIND(4*KK-9)
          TH(2) = VIND(4*KK-8)

          PRODC = 2.D0*SIGDC(3*K-2)*TH(1) + SIGDC(3*K)*TH(2)
          PRODM = 2.D0*SIGD(1)*TH(1) + SIGD(3)*TH(2)
          PS    = 2.D0*SIGD(1)*SIGDC(3*K-2)+SIGD(3)*SIGDC(3*K)

Ckh --- traction
          IF ((P(K)/PREF).GT.TOLE1) THEN
             IF ((-Q(K)/PREF).GT.TOLE1) THEN
               DPSI =MDIL+PS/2.D0/P(K)/Q(K)
             ELSE
               DPSI =MDIL
             ENDIF
           ELSE
             IF (DEBUG) WRITE(6,'(A)')'HUJJID :: TRACTION CYCLIQUE'
             IF ((-Q(K)/PREF).GT.TOLE1) THEN
               DPSI = MDIL+PS/2.D-6/PREF/Q(K)
             ELSE
               DPSI = MDIL
             ENDIF
           ENDIF

          IF ((-Q(K)/PREF).GT.TOLE1) THEN
            DLEVDR(K) = DLAMBD(K)*COEF*
     &                  (DKSIDR(K)*DPSI
     &                  +KSI(K)*M*(UN-B*LOG(PK/PC))/(2.D0*Q(K))*
     &                  (PRODM-PS*PRODC/(2.D0*Q(K)**2.D0)))
          ELSE
            DLEVDR(K) = DLAMBD(K)*COEF*DKSIDR(K)*MDIL
          ENDIF

        ELSEIF (KK .EQ. 8) THEN
          DLEVDR(K) = ZERO

        ENDIF
        DLEVDR(K) = DLEVDR(K)*CCOND/ABS(PREF)
 151    CONTINUE

 152    CONTINUE
C =====================================================================
C --- XII. CALCUL DE DLEVPDLA (1XNBMEC) -------------------------------
C =====================================================================
      DO 153 K = 1, 7
        DLEVDL(K) = ZERO
 153  CONTINUE

      DO 161 K = 1, NBMECT

        KK = INDI(K)
        PK =P(K) -PTRAC

        IF (KK .LT. 4) THEN

Ckh --- traction
          IF ((P(K)/PREF).GT.TOLE1) THEN
            DPSI =MDIL+Q(K)/P(K)
          ELSE
            IF (DEBUG) WRITE(6,'(A)')'HUJJID :: TRACTION MONOTONE'
            DPSI =MDIL+1.D+6*Q(K)/PREF
          ENDIF

          DLEVDL(K) = KSI(K)*COEF*DPSI
        ELSEIF (KK .EQ. 4) THEN
          DLEVDL(K) = UN

        ELSEIF ((KK .GT. 4) .AND. (KK .LT.8)) THEN

          CALL HUJPRJ(KK-4,SIGF,SIGD,COEF0,MUL)
          PS = 2.D0*SIGD(1)*SIGDC(3*K-2)+SIGD(3)*SIGDC(3*K)

Ckh --- traction
          IF ((P(K)/PREF).GT.TOLE1) THEN
            IF ((-Q(K)/PREF).GT.TOLE1) THEN
              DPSI =MDIL+PS/2.D0/P(K)/Q(K)
            ELSE
              DPSI =MDIL
            ENDIF
          ELSE
            IF (DEBUG) WRITE(6,'(A)')'HUJJID :: TRACTION CYCLIQUE'
            IF ((-Q(K)/PREF).GT.TOLE1) THEN
              DPSI = MDIL+PS/2.D-6/PREF/Q(K)
            ELSE
              DPSI = MDIL
            ENDIF
          ENDIF

          DLEVDL(K) = KSI(K)*COEF*DPSI

        ELSEIF (KK .EQ. 8) THEN

          IF (VIND(22).EQ.UN) THEN
            DLEVDL(K) = -UN
          ELSE
            DLEVDL(K) = UN
          ENDIF

        ENDIF
 161    CONTINUE


C =====================================================================
C --- XIII. CALCUL DE DLFDS (NBMECX6) ---------------------------------
C =====================================================================
      DO 162 K = 1, 7
        DO 162 I = 1, 6
          DLFDS(K,I) = ZERO
 162      CONTINUE

      DO 171 K = 1, NBMECT
        KK = INDI(K)
        CALL HUJDDD('DFDS  ', KK, MATER, INDI, YF, VIND,
     &              DFDS, DPSIDS, IRET)
        IF (IRET.EQ.1) GOTO 1000
        DO 171 I = 1, NDT
          DLFDS(K,I) = DFDS(I)
 171      CONTINUE


C =====================================================================
C --- XIV. CALCUL DE DLFDR (NBMECXNBMEC) ------------------------------
C =====================================================================
      DO 180 K = 1, 7
        DO 180 L = 1, 4
          DLFDR(K,L) = ZERO
 180      CONTINUE

      IF(NBMECA.EQ.0)GOTO 182

      DO 181 K = 1, NBMECA

        KK = INDI(K)
        PK =P(K) -PTRAC

        IF (KK .LT. 4) THEN

          DLFDR(K,K) = M*PK*( UN-B*LOG(PK/PC) )

        ELSEIF ((KK .EQ. 4).OR.(KK.EQ.8)) THEN

          DLFDR(K,K) = D*PC

        ELSEIF ((KK .GT. 4) .AND. (KK .LT.8)) THEN

          TH(1) = VIND(4*KK-9)
          TH(2) = VIND(4*KK-8)
          PROD  = SIGDC(3*K-2)*TH(1) + SIGDC(3*K)*TH(2)*D12
          IF((-Q(K)/PREF).GT.TOLE1)THEN
            DLFDR(K,K) = M*PK*( UN-B*LOG(PK/PC) )
     &                 *(UN+PROD/Q(K))
          ELSE
            DLFDR(K,K) = M*PK*( UN-B*LOG(PK/PC) )
          ENDIF
        ENDIF
        DLFDR(K,K) = DLFDR(K,K)/ABS(PREF)
 181    CONTINUE

 182    CONTINUE
C =====================================================================
C --- XV. CALCUL DE DLFDEVP (NBMECX1) ---------------------------------
C =====================================================================
       DO 183 K = 1, 7
         DLFDLE(K) = ZERO
 183   CONTINUE

       DO 190 K = 1, NBMECT

         KK = INDI(K)
         PK = P(K) -PTRAC

         IF (KK .LT. 4) THEN

           DLFDLE(K) = -M*B*PK*RC(K)*BETA /CCOND

         ELSEIF (KK .EQ. 4) THEN

           DLFDLE(K) = -RC(K)*D*PC*BETA /CCOND

         ELSEIF ((KK .GT. 4) .AND. (KK .LT.8)) THEN

           XH(1) = VIND(4*KK-11)
           XH(2) = VIND(4*KK-10)
           TH(1) = VIND(4*KK-9)
           TH(2) = VIND(4*KK-8)
           PROD  = SIGDC(3*K-2)*(XH(1)-RC(K)*TH(1)) +
     &             (SIGDC(3*K)*(XH(2)-RC(K)*TH(2)))*D12
           IF ((-Q(K)/PREF).GT.TOLE1) THEN
             DLFDLE(K) = M*B*PK*(PROD/Q(K)-RC(K))*BETA /CCOND
           ELSE
             DLFDLE(K) = M*B*PK*(-RC(K))*BETA /CCOND
           ENDIF
         ELSEIF (KK .EQ. 8) THEN

           IF (VIND(22).EQ.UN) THEN
             DLFDLE(K) = -D*PC*BETA*(RC(K)-VIND(21))/CCOND
           ELSE
             DLFDLE(K) = -D*PC*BETA*(VIND(21)+RC(K)) /CCOND
           ENDIF

         ENDIF
 190     CONTINUE


C =====================================================================
C --- XVI. CALCUL DE DLFDLA (NBMECXNBMEC) -----------------------------
C =====================================================================
       DO 200 K = 1, 7
         DO 200 L = 1, 7
           DLFDLA(K,L) = ZERO
 200       CONTINUE

C =====================================================================
C --- XVII. CALCUL DE LE (6) ---------------------------------------
C =====================================================================
C ---- XVII.1. CALCUL DE CDE = C*DEPSE
C                        6X1
C REMARQUE: ON A DEJA DEPSE CALCULE AU I.1.
       CALL LCPRMV (HOOKNL, DEPSE, CDE)
       DO 210 I = 1, NDT
         LE(I) = YF(I) - YD(I) - CDE(I)
 210     CONTINUE


C =====================================================================
C --- XVIII. CALCUL DE LEVP (1X1) -------------------------------------
C =====================================================================
        LEVP = YF(NDT+1) - YD(NDT+1)
        DO 220 K = 1, NBMECT

          KK = INDI(K)
          PK =P(K) -PTRAC

          IF (KK .LT. 4) THEN

Ckh --- traction
            IF ((P(K)/PREF).GT.TOLE1) THEN
              DPSI =MDIL+Q(K)/P(K)
            ELSE
              IF (DEBUG) WRITE(6,'(A)')'HUJJID :: TRACTION MONOTONE'
              DPSI =MDIL+1.D+6*Q(K)/PREF
            ENDIF
            LEVP = LEVP + COEF*DLAMBD(K)*KSI(K)*DPSI

          ELSEIF (KK .EQ. 4) THEN

            LEVP = LEVP + DLAMBD(K)

          ELSEIF ((KK .GT. 4) .AND. (KK .LT.8)) THEN

            CALL HUJPRJ(KK-4,SIGF,SIGD,COEF0,MUL)
            PS = 2.D0*SIGD(1)*SIGDC(3*K-2)+SIGD(3)*SIGDC(3)

Ckh --- traction
            IF ((P(K)/PREF).GT.TOLE1) THEN
              IF ((-Q(K)/PREF).GT.TOLE1) THEN
                DPSI =MDIL+PS/2.D0/P(K)/Q(K)
              ELSE
                DPSI =MDIL
              ENDIF
            ELSE
              IF (DEBUG) WRITE(6,'(A)')'HUJJID :: TRACTION CYCLIQUE'
              IF ((-Q(K)/PREF).GT.TOLE1) THEN
                DPSI = MDIL+PS/2.D-6/PREF/Q(K)
              ELSE
                DPSI = MDIL
              ENDIF
            ENDIF

            LEVP = LEVP + COEF*DLAMBD(K)*KSI(K)*DPSI

          ELSEIF (KK .EQ. 8) THEN

            IF(VIND(22).GT.ZERO)THEN
              LEVP = LEVP - DLAMBD(K)
            ELSE
              LEVP = LEVP + DLAMBD(K)
            ENDIF

          ENDIF

 220      CONTINUE


C =====================================================================
C --- XIX. CALCUL DE LR (NBMECX1) -------------------------------------
C =====================================================================
        DO 221 K = 1, 4
          LR(K) = ZERO
 221      CONTINUE

        IF (NBMECA.EQ.0) GOTO 231
        DO 230 K = 1, NBMECA
          KK = INDI(K)
          IF (KK .LT. 4) THEN
            LR(K) = YF(NDT+1+K) - YD(NDT+1+K) -
     &              DLAMBD(K)/AD(K)*(UN-RC(K))**DEUX
          ELSEIF (KK .EQ. 4) THEN
            LR(K) = YF(NDT+1+K) - YD(NDT+1+K) -
     &              DLAMBD(K)/CMON*(UN-RC(K))**DEUX

          ELSEIF ((KK .GT. 4) .AND. (KK .LT.8)) THEN
            TH(1) = VIND(4*INDI(K)-9)
            TH(2) = VIND(4*INDI(K)-8)
            PROD  = SIGDC(3*K-2)*TH(1) + SIGDC(3*K)*TH(2)/DEUX

            IF((-Q(K)/PREF.LT.TOLE1).OR.((UN+PROD/Q(K)).LT.TOLE1))THEN
              AD(K) = (ACYC+KSI(K)*(AMON-ACYC))
            ELSE
              AD(K) = (ACYC+KSI(K)*(AMON-ACYC))*(UN+PROD/Q(K))
            ENDIF
            LR(K) = YF(NDT+1+K) - YD(NDT+1+K) -
     &              DLAMBD(K)/AD(K)*(UN-RC(K))**DEUX
          ELSEIF (KK .EQ. 8) THEN
            LR(K) = YF(NDT+1+K) - YD(NDT+1+K) -
     &              DLAMBD(K)/CCYC*(UN-RC(K))**DEUX

          ENDIF
 230      CONTINUE

 231      CONTINUE
C =====================================================================
C --- XX. CALCUL DE LF (NBMECX1) --------------------------------------
C =====================================================================
        DO 232 K = 1, 7
          LF(K) = ZERO
 232    CONTINUE

        DO 240 K = 1, NBMECT
          KK = INDI(K)
          PK =P(K) -PTRAC
          IF (KK .LT. 4) THEN
            LF(K) = Q(K) + M*PK*RC(K)*( UN-B*LOG(PK/PC) )
          ELSEIF (KK .EQ. 4) THEN
            LF(K) = ABS(P(K)) + RC(K)*D*PC
          ELSEIF ((KK .GT. 4) .AND. (KK .LT.8)) THEN
            LF(K) = Q(K) + M*PK*RC(K)*( UN-B*LOG(PK/PC) )
          ELSEIF (KK .EQ. 8) THEN
            LF(K) = ABS(P(K)) + RC(K)*D*PC
          ELSEIF (KK .GT. 8) THEN
            CALL HUJPRJ(KK-8,YF,SIGD,PK,PS)
            LF(K) = PK + DEUX*RTRAC - PTRAC
          ENDIF
 240      CONTINUE


C =====================================================================
C --- ASSEMBLAGE DE R : -----------------------------------------------
C =====================================================================
C     R    = -( LE       , LEVP       , LR       , LF       )
C =====================================================================
C --- ASSEMBLAGE DE DRDY
C =====================================================================
C     DRDY =  ( DLEDS    , DLEDEVP    , DLEDR    , DLEDLA   )
C             ( DLEVPDS  , DLEVPDEVP  , DLEVPDR  , DLEVPDLA )
C             ( DLRDS    , DLRDEVP    , DLRDR    , DLRDLA   )
C             ( DLFDS    , DLFDEVP    , DFLFDR   , DFLFDLA  )
C =====================================================================
C --- ASSEMBLAGE DE R -------------------------------------------------
C =====================================================================
        DO 850 I = 1, NDT
           R(I)  = -LE(I) /CCOND
 850       CONTINUE
        R(NDT+1) = -LEVP

        IF (NBMECA.EQ.0) GOTO 951
        DO 950 K = 1, NBMECA
          R(NDT+1+K)        = -LR(K) /CCOND*ABS(PREF)
          R(NDT+1+NBMECA+K) = -LF(K) /CCOND
 950      CONTINUE
 951    CONTINUE

        IF(NBMECA.LT.NBMECT)THEN
          DO 952 K = 1, NBMECT
            IF(INDI(K).GT.8)THEN
              R(NDT+1+NBMECA+K) = -LF(K)/CCOND
            ENDIF
 952        CONTINUE
        ENDIF
C =====================================================================
C --- ASSEMBLAGE DE DRDY ----------------------------------------------
C =====================================================================
C DLEDDY
        CALL LCICMA (DLEDS,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,1)
        CALL LCICMA (DLEDEV,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,1,NDT+1)
        CALL LCICMA (DLEDR,6,4,NDT,NBMECA,1,1,DRDY,NMOD,NMOD,1,NDT+2)
        CALL LCICMA (DLEDLA,6,7,NDT,NBMECT,1,1,DRDY,NMOD,NMOD,1,
     &               NDT+2+NBMECA)
C DLEVPDDY
        CALL LCICMA (DLEVDS,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,1)
        DRDY(NDT+1,NDT+1) = DLEVDE
        CALL LCICMA (DLEVDR,1,4,1,NBMECA,1,1,DRDY,NMOD,NMOD,
     &               NDT+1,NDT+2)
        CALL LCICMA (DLEVDL,1,7,1,NBMECT,1,1,DRDY,NMOD,NMOD,NDT+1,
     &               NDT+2+NBMECA)
C DLRDDY
        CALL LCICMA (DLRDS,4,6,NBMECA,NDT,1,1,DRDY,NMOD,NMOD,NDT+2,1)
        CALL LCICMA (DLRDLE,4,1,NBMECA,1,1,1,DRDY,NMOD,NMOD,
     &               NDT+2,NDT+1)
        CALL LCICMA (DLRDR,4,4,NBMECA,NBMECA,1,1,DRDY,NMOD,NMOD,
     &               NDT+2,NDT+2)
        CALL LCICMA (DLRDLA,4,7,NBMECA,NBMECT,1,1,DRDY,NMOD,NMOD,NDT+2,
     &               NDT+2+NBMECA)
C DLFDDY
        CALL LCICMA (DLFDS,7,6,NBMECT,NDT,1,1,DRDY,NMOD,NMOD,
     &               NDT+2+NBMECA,1)
        CALL LCICMA (DLFDLE,7,1,NBMECT,1,1,1,DRDY,NMOD,NMOD,
     &               NDT+2+NBMECA,NDT+1)
        CALL LCICMA (DLFDR,7,4,NBMECT,NBMECA,1,1,DRDY,NMOD,NMOD,
     &               NDT+2+NBMECA,NDT+2)
        CALL LCICMA (DLFDLA,7,7,NBMECT,NBMECT,1,1,DRDY,NMOD,NMOD,
     &               NDT+2+NBMECA,NDT+2+NBMECA)

        GOTO 1000

 999    CONTINUE
        IF (DEBUG) THEN
          CALL TECAEL(IADZI,IAZK24)
          NOMAIL = ZK24(IAZK24-1+3) (1:8)
          WRITE(IFM,'(10(A))') 'HUJJID :: LOG(PK/PC) NON DEFINI DANS ',
     &                         'LA MAILLE ',NOMAIL
        ENDIF
        IRET=1
 1000   CONTINUE

C =====================================================================
C        CALL JEDEMA ()
C =====================================================================
        END

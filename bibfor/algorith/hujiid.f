      SUBROUTINE HUJIID (MOD, MATER, INDI, DEPS, I1E, YD, VIND, DY,
     &                   LOOP, DSIG, BNEWS, MTRAC, IRET)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_20
C     ----------------------------------------------------------------
C     LOI HUJEUX :  MECANISMES ISOTROPE ET DEVIATOIRE
C     CALCUL DE LA SOLUTION D ESSAI EXPLICITE
C            DY = (DSIG, DR, DEPSVP, DLAMB)
C     AVEC   Y  = ( SIG,  R,  EPSI_VOLU_P,  DLAMBDA )
C     A PARTIR DE LA PREDICTION ELASTIQUE
C     ----------------------------------------------------------------
C     IN   MOD      :  MODELISATION
C          MATER    :  COEFFICIENTS MATERIAU A T+DT
C          INDI     :  INDICE DES MECANISMES ACTIVES
C          DEPS     :  INCREMENT DE DEFORMATION
C          YD       :  VARIABLES A T = (SIGD, VIND, DLAMB)
C          VIND     :  VARIABLES INTERNES A T
C          I1E      :  TRACE(SIGE): CONTRAINTE DE PREDICTION
C          LOOP     :  UTILISE PREDICTION ELASTIQUE (= .FALSE.) OU
C                                         PLASTIQUE (= .TRUE. )
C          DSIG     :  INCREMENT DE CONTRAINTE PLASTIQUE NECESSAIRE
C                      POUR PREDICTION PLASTIQUE
C     OUT  DY       :  SOLUTION D ESSAI (DSIG, DVIN, DDLAMB)
C          INDI     :  MECANISMES ACTIVES + TRACTION (1 A 3 SUPPL)
C     ----------------------------------------------------------------
C     Y CONTIENT LES CONTRAINTES : SIG
C                LES VARIABLES D'ECROUISSAGE : R, EPSI_VOLU_P
C                LES MULTIPLICATEURS PLASTIQUES : DLAMBDA
C ====================================================================
      INCLUDE 'jeveux.h'
      INTEGER     NDT, NDI, I, J, K, KK, L, LL, NBMECT
      INTEGER     NBMECA, INDI(7), IRET, IADZI, IAZK24
      INTEGER     IFM, NIV
      REAL*8      DEPS(6), DEPSE(6), HOOKNL(6,6), I1E
      REAL*8      DSIG(6), SIGD(3), P(7), Q(7), PE(7), QE(7)
      REAL*8      YD(18), YE(18), DY(18), F2(7), DP(3)
      REAL*8      MATER(22,2), N, BETA, B, D, M, PCO, PC, PREF
      REAL*8      PHI, ANGDIL, MDIL, ACYC, AMON, CMON, CCYC
      REAL*8      RC(7), TRACE, EPSVP, AD(7), KSI(7)
      REAL*8      E, NU, AL, DEMU, I1DE, SIGE(6)
      REAL*8      DFDL(7,7), DR(7), DEPSVP, RTRAC
      REAL*8      DEGR, ZERO, UN, D13, DEUX
      REAL*8      DET, TOL, TOLE1, COEF, VIND(*)
      REAL*8      PSI(42), DFDS(6), DPSIDS(6,6)
      REAL*8      SIGDC(12), SIGDCE(12), PROD
      REAL*8      XK(2), TH(2), LA, PS, TP, TP1, S, DS
      REAL*8      PTRAC, PISO, PEK, DPSI, DSIGT(6), SIGT(6)
      REAL*8      E1,E2,E3,NU12,NU13,NU23,G1,G2,G3,NU21,NU31,NU32,DELTA
      REAL*8      FACTOR, MAXI, COHES, VEC(3), R8PREM, PT, QT
      CHARACTER*8 MOD, NOMAIL
      LOGICAL     DEBUG, LOOP, BNEWS(3), MTRAC
C ====================================================================
      PARAMETER   ( D13  = .3333333333334D0 )
      PARAMETER   ( UN   = 1.D0 )
      PARAMETER   ( ZERO = 0.D0 )
      PARAMETER   ( DEUX = 2.D0 )
      PARAMETER   ( TOLE1 = 1.D-7 )
      PARAMETER   ( DEGR = 0.0174532925199D0 )


C ====================================================================
      COMMON /TDIM/     NDT, NDI
      COMMON /MESHUJ/   DEBUG
C ====================================================================
      CALL INFNIV (IFM, NIV)

      DO 7 I = 1, 18
        YE(I) = ZERO
 7      CONTINUE
C ====================================================================
C --- PROPRIETES HUJEUX MATERIAU -------------------------------------
C ====================================================================
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
      PTRAC  = MATER(21,2)
      PISO   = ZERO
      RTRAC  = ABS(1.D-6*PREF)


C ====================================================================
C --- PREMIER INVARIANT ET AUTRES GRANDEURS UTILES -------------------
C ====================================================================
      IRET   = 0
      NBMECA = 0

       DO 4 K = 1, 4
         IF (INDI(K).GT.0) NBMECA=NBMECA+1
         Q(K)  = ZERO
         QE(K) = ZERO
 4       CONTINUE


C ====================================================================
C --- OPERATEUR DE RIGIDITE NON LINEAIRE -----------------------------
C ====================================================================
C --- OPERATEUR LINEAIRE NON LINEAIRE --------------------------------
C ====================================================================
      IF((I1E-PISO).GE.ZERO)THEN
        IRET = 1
        GOTO 9999
      ENDIF

      CALL LCINMA (ZERO, HOOKNL)

      IF (MOD(1:2) .EQ. '3D'     .OR.
     &    MOD(1:6) .EQ. 'D_PLAN' .OR.
     &    MOD(1:4) .EQ. 'AXIS')  THEN

        IF (MATER(17,1).EQ.UN) THEN

          E    = MATER(1,1)*((I1E -PISO)/PREF)**N
          NU   = MATER(2,1)
          AL   = E*(UN-NU) /(UN+NU) /(UN-DEUX*NU)
          DEMU = E     /(UN+NU)
          LA   = E*NU/(UN+NU)/(UN-DEUX*NU)

          DO 30 I = 1, NDI
            DO 30 J = 1, NDI
              IF (I.EQ.J) HOOKNL(I,J) = AL
              IF (I.NE.J) HOOKNL(I,J) = LA
 30           CONTINUE
          DO 35 I = NDI+1, NDT
            HOOKNL(I,I) = DEMU
 35         CONTINUE

        ELSEIF (MATER(17,1).EQ.DEUX) THEN

          E1   = MATER(1,1)*((I1E -PISO)/PREF)**N
          E2   = MATER(2,1)*((I1E -PISO)/PREF)**N
          E3   = MATER(3,1)*((I1E -PISO)/PREF)**N
          NU12 = MATER(4,1)
          NU13 = MATER(5,1)
          NU23 = MATER(6,1)
          G1   = MATER(7,1)*((I1E -PISO)/PREF)**N
          G2   = MATER(8,1)*((I1E -PISO)/PREF)**N
          G3   = MATER(9,1)*((I1E -PISO)/PREF)**N
          NU21 = MATER(13,1)
          NU31 = MATER(14,1)
          NU32 = MATER(15,1)
          DELTA= MATER(16,1)

          HOOKNL(1,1) = (UN - NU23*NU32)*E1/DELTA
          HOOKNL(1,2) = (NU21 + NU31*NU23)*E1/DELTA
          HOOKNL(1,3) = (NU31 + NU21*NU32)*E1/DELTA
          HOOKNL(2,2) = (UN - NU13*NU31)*E2/DELTA
          HOOKNL(2,3) = (NU32 + NU31*NU12)*E2/DELTA
          HOOKNL(3,3) = (UN - NU21*NU12)*E3/DELTA
          HOOKNL(2,1) = HOOKNL(1,2)
          HOOKNL(3,1) = HOOKNL(1,3)
          HOOKNL(3,2) = HOOKNL(2,3)
          HOOKNL(4,4) = G1
          HOOKNL(5,5) = G2
          HOOKNL(6,6) = G3

        ELSE
          CALL U2MESS('F', 'COMPOR1_39')
        ENDIF

      ELSEIF (MOD(1:6) .EQ. 'C_PLAN' .OR.
     &        MOD(1:2) .EQ. '1D')   THEN

        CALL U2MESS('F', 'COMPOR1_4')

      ENDIF


C ====================================================================
C --- ON CALCULE DE TOUTES FACONS UNE PREDICTION ---------------------
C --- ELASTIQUE EN TANT QUE DE BESOIN --------------------------------
C ====================================================================
      IF (.NOT. LOOP)  CALL LCPRMV (HOOKNL, DEPS, DSIG)
      CALL LCSOVN (NDT, YD, DSIG, YE)
C      LOOP = .FALSE.
C ====================================================================
C --- CALCUL DE L'INCRMEENT DE CONTRAINTES ELASTIQUE SI LOOP ACTIVE
C     CECI EST FAIT POUR DETECTER LES SOLLICITATIONS POSSIBLES
C     DES MECANISMES DE TRACTION A PARTIR DU TIR ELASTIQUE ET NON DE
C     L'ETAT DE CONTRAINTES CONVERGES PRECEDENT
C ====================================================================
      IF (LOOP) THEN
        CALL LCPRMV (HOOKNL, DEPS, DSIGT)
      ELSE
        CALL LCEQVN(NDT, DSIG, DSIGT)
      ENDIF
      CALL LCSOVN (NDT, YD, DSIGT, SIGT)

C --- FAUT-IL CONSIDERER LES MECANISMES DE TRACTION?
      NBMECT = NBMECA
      CALL LCEQVN(NDT, YE, SIGE)


      IF(DEBUG)WRITE(6,*)'BNEWS =',(BNEWS(I),I=1,3)
      DO 36 I = 1, 3
        CALL HUJPRJ(I,SIGT,SIGD,PT,QT)
        IF ((((PT+DEUX*RTRAC-PTRAC)/ABS(PREF)).GE.-R8PREM())
     &      .AND.(.NOT.BNEWS(I))) THEN
          NBMECT = NBMECT + 1
          INDI(NBMECT) = 8 + I
        ELSEIF ((.NOT.BNEWS(I)).AND.(MTRAC)) THEN
          NBMECT = NBMECT + 1
          INDI(NBMECT) = 8 + I
        ENDIF
  36    CONTINUE

      MAXI   = UN
      COHES  = -RTRAC+PTRAC
      FACTOR = UN

      IF ((NBMECT.NE.NBMECA).AND.(NBMECA.EQ.0)) GOTO 51

      DO 50 I = 1, NDI
        CALL HUJPRJ(I,SIGE,SIGD,PE(I),QE(I))
        CALL HUJPRJ(I,YD,SIGD,P(I),Q(I))
        CALL HUJPRJ(I,DSIG,SIGD,DP(I),Q(I))
        IF ((PE(I).GT.COHES) .AND. (DP(I).GT.TOLE1)) THEN
          FACTOR = (-P(I)+COHES)/DP(I)
          IF ((FACTOR.GT.ZERO) .AND. (FACTOR.LT.MAXI)) THEN
            MAXI = FACTOR
          ENDIF
        ENDIF
  50    CONTINUE

C ---> SI IL EXISTE SIG(I)>0, ALORS MODIFICATION DE LA PREDICTION
      IF (MAXI.LT.UN) THEN
        DO 60 I = 1, NDT
          DSIG(I) = MAXI * DSIG(I)
  60      CONTINUE
        IF (DEBUG) THEN
          WRITE (6,'(A,A,E12.5)')
     &   'HUJIID DEBUT : APPLICATION DE FACTOR POUR MODIFIER ',
     &    'LA PREDICTION -> FACTOR =',FACTOR
          WRITE(6,*)'YE =',(YD(I)+DSIG(I),I=1,NDT)
        ENDIF
        CALL LCSOVN (NDT, YD, DSIG, YE)
      ENDIF
 51   CONTINUE

      IF ((NBMECA .EQ. 1) .AND.
     &    ((INDI(1).EQ.4) .OR. (INDI(1).EQ.8))) THEN

        DO 31 I = NDT+1, 18
          DY(I)=ZERO
 31       CONTINUE
        DO 32 I = 1, NDT
          DY(I) = DSIG(I)
 32       CONTINUE

        GOTO 9999

      ENDIF

      DO 3 K = 1, 42
        PSI(K) = ZERO
  3     CONTINUE

      DO 6 K = 1, 7
        PE(K) = ZERO
        Q(K)  = ZERO
        QE(K) = ZERO
        P(K)  = ZERO
  6     CONTINUE

      DO 5 K = 1, NBMECT

        CALL HUJDDD('PSI   ', INDI(K), MATER, INDI, YD, VIND,
     &               PSI((K-1)*NDT+1), DPSIDS,IRET)
        IF (IRET.EQ.1) GOTO 999

        IF (INDI(K).LE.8) THEN

          RC(K) = YD(NDT+1+K)

          IF (INDI(K) .LT. 4) THEN

            CALL HUJPRJ (INDI(K), YD, SIGD, P(K), Q(K))
            CALL HUJPRJ (INDI(K), YE, SIGD, PE(K), QE(K))
            IF (((P(K) -PTRAC)/PREF) .LE. TOLE1 .OR.
     &          ((PE(K)-PTRAC)/PREF) .LE. TOLE1) GOTO 999
            CALL HUJKSI ('KSI   ', MATER, RC(K), KSI(K), IRET)
            IF (IRET.EQ.1) GOTO 999
            AD(K) = ACYC+KSI(K)*(AMON-ACYC)

          ELSEIF ((INDI(K) .GT. 4) .AND. (INDI(K) .LT. 8)) THEN

            CALL HUJPRC (K, INDI(K)-4, YD, VIND, MATER, YD, P(K),
     &                   Q(K), SIGDC(3*K-2))
            CALL HUJPRC (K, INDI(K)-4, YE, VIND, MATER, YD, PE(K),
     &                   QE(K), SIGDCE(3*K-2))
            IF (((P(K) -PTRAC)/PREF) .LE. TOLE1 .OR.
     &          ((PE(K)-PTRAC)/PREF) .LE. TOLE1) GOTO 999
            CALL HUJKSI ('KSI   ', MATER, RC(K), KSI(K), IRET)
            IF (IRET.EQ.1) GOTO 999

            TH(1) = VIND(4*INDI(K)-9)
            TH(2) = VIND(4*INDI(K)-8)
            PROD  = SIGDCE(3*K-2)*TH(1) + SIGDCE(3*K)*TH(2)/DEUX

            IF (QE(K).LT.TOLE1) THEN
              AD(K) = (ACYC+KSI(K)*(AMON-ACYC))
            ELSEIF ((UN+PROD/QE(K)).LT.TOLE1) THEN
              AD(K) = (ACYC+KSI(K)*(AMON-ACYC))
            ELSE
              AD(K) = (ACYC+KSI(K)*(AMON-ACYC))*(UN+PROD/QE(K))
            ENDIF

          ELSEIF (INDI(K) .EQ. 8) THEN

            CALL HUJPIC(K, INDI(K), YD, VIND, MATER, YD, P(K))
            CALL HUJPIC(K, INDI(K), YE, VIND, MATER, YD, PE(K))

            IF (((P(K) -PISO)/PREF) .LE. TOLE1 .OR.
     &          ((PE(K)-PISO)/PREF) .LE. TOLE1) GOTO 999

          ENDIF

          YE(NDT+1+K) = YD(NDT+1+K)

        ENDIF

 5    CONTINUE

      EPSVP     = YD(NDT+1)
      YE(NDT+1) = YD(NDT+1)
      PC        = PCO*EXP(-BETA*EPSVP)

      CMON = CMON * PC/PREF
      CCYC = CCYC * PC/PREF

      COEF = MATER(20,2)

C ====================================================================
C --- CALCUL DE DLAMBI, DLAMBD ---------------------------------------
C ====================================================================
C --- PAR RESOLUTION DU SYSTEME : ------------------------------------
C        _
C       (     D FD               D FD
C       (   --------  * DDLD + --------  * DDLI = - F2(1:3)
C       (   D DLAMBD           D DLAMBI
C       (
C       (     D FI               D FI
C       (   --------  * DDLD + --------  * DDLI = - F2(4)
C       (_  D DLAMBD           D DLAMBI
C
C ====================================================================
      DO 45 K = 1, NBMECT
        DO 45 L = 1, NBMECT
          DFDL(K,L) = ZERO
 45       CONTINUE


C ---> I. CALCUL DE DF. / DDLAMB. POUR DDLAMB. = 0
C ---> I.1. CALCUL DE DFDSE(K)*HOOKNL*PSI-(L)
      DO 40 K = 1, NBMECT
        KK = INDI(K)
        CALL HUJDDD ('DFDS  ', KK, MATER, INDI, YE, VIND,
     &               DFDS, DPSIDS,IRET)
        IF (IRET.EQ.1) GOTO 999
        DO 40 L = 1, NBMECT
          LL = (L-1)*NDT
          DO 40 I = 1, NDT
            DO 40 J = 1, NDT
              DFDL(K,L) = DFDL(K,L) - HOOKNL(I,J)*DFDS(I)*PSI(LL+J)
 40           CONTINUE

C ---- FIN I.1.
C ---> I.2. CALCUL DE DFDEVPE(K)*DEVPDDLAMB-(L)
C ----  I.2.1. MECANISME DEVIATOIRE MONOTONE
       DO 11 K = 1, NBMECT

         KK = INDI(K)
         PEK =PE(K) -PTRAC
         IF (KK .LT. 4) THEN

           F2(K) = -QE(K) - M*PEK*RC(K) * ( UN - B*LOG(PEK/PC) )
           IF (F2(K).GT.ZERO) F2(K) = ZERO

           DO 12 L = 1, NBMECA

             LL = INDI(L)

             IF (LL .LT. 4) THEN

Ckh -- traction
               IF ((P(L)/PREF).GT.TOLE1) THEN
                  DPSI = MDIL+Q(L)/P(L)
                ELSE
                  DPSI = MDIL+1.D+6*Q(L)/PREF
                ENDIF
               DFDL(K,L) = DFDL(K,L) + B*M*PEK*RC(K)*BETA
     &                     * KSI(L)*COEF*DPSI

             ELSEIF (LL .EQ. 4) THEN

               DFDL(K,L) = DFDL(K,L) + B*M*PEK*RC(K)*BETA

             ELSEIF ((LL .GT. 4) .AND. (LL .LT. 8)) THEN

               CALL HUJPRJ (LL-4, YE, SIGD, TP, TP1)
               PS = 2*SIGD(1)*SIGDCE(3*L-2)+SIGD(3)*SIGDCE(3*L)
Ckh -- traction
               IF (((P(L)/PREF).GT.TOLE1) .AND.
     &             ((-Q(L)/PREF).GT.TOLE1)) THEN
                 DPSI =MDIL+PS/(2.D0*P(L)*Q(L))
               ELSEIF (((P(L)/PREF).LE.TOLE1) .AND.
     &                ((-Q(L)/PREF).GT.TOLE1)) THEN
                 DPSI =MDIL+PS/(2.D-6*PREF*Q(L))
               ELSE
                 DPSI =MDIL
               ENDIF
               DFDL(K,L) = DFDL(K,L) + B*M*PEK*RC(K)*BETA
     &                      * KSI(L)*COEF * DPSI

             ELSEIF (LL .EQ. 8) THEN

               IF (VIND(22).EQ.UN) THEN
                 DFDL(K,L) = DFDL(K,L) - B*M*PEK*RC(K)*BETA
               ELSE
                 DFDL(K,L) = DFDL(K,L) + B*M*PEK*RC(K)*BETA
               ENDIF

             ENDIF

 12       CONTINUE

C ---- I.2.2. MECANISME ISOTROPE MONOTONE
          ELSEIF (KK .EQ. 4) THEN

            IF (K .NE. NBMECA) CALL U2MESS ('F', 'COMPOR1_5')
            I1DE  = D13*TRACE(NDI,YE)
            F2(K) = -ABS(I1DE) - RC(K)*D*PC
            IF (F2(K).GT.ZERO) F2(K) = ZERO

            DFDL(K,K) = DFDL(K,K) + RC(K)*D*PC*BETA

            DO 13 L = 1, NBMECA-1, 1
              LL = INDI(L)
              IF(LL.LT.4)THEN

Ckh --- traction
                IF ((P(L)/PREF).GT.TOLE1) THEN
                  DPSI =MDIL+Q(L)/P(L)
                ELSE
                  DPSI = MDIL+1.D+6*Q(L)/PREF
                ENDIF

                DFDL(K,L) = DFDL(K,L) + RC(K)*D*PC*BETA
     &                      * KSI(L)*COEF*DPSI

              ELSEIF ((LL.GT.4).AND.(LL.LT.8)) THEN

                CALL HUJPRJ (LL-4, YE, SIGD, TP, TP1)
                PS = 2*SIGD(1)*SIGDCE(3*L-2)+SIGD(3)*SIGDCE(3*L)
Ckh --- traction
                IF (((P(L)/PREF).GT.TOLE1) .AND.
     &              ((-Q(L)/PREF).GT.TOLE1)) THEN
                  DPSI =MDIL+PS/(2.D0*P(L)*Q(L))
                ELSEIF (((P(L)/PREF).LE.TOLE1) .AND.
     &                 ((-Q(L)/PREF).GT.TOLE1)) THEN
                  DPSI =MDIL+PS/(2.D-6*PREF*Q(L))
                ELSE
                  DPSI =MDIL
                ENDIF
                DFDL(K,L) = DFDL(K,L) + RC(K)*D*PC*BETA
     &                      * KSI(L)*COEF*MDIL
              ENDIF
 13           CONTINUE

C --- I.2.3. MECANISME DEVIATOIRE CYCLIQUE
          ELSEIF ((KK .LT. 8) .AND. (KK .GT. 4)) THEN

            F2(K) = -QE(K) - M*PEK*RC(K) * ( UN - B*LOG(PEK/PC) )
            IF (F2(K).GT.ZERO) F2(K) = ZERO

            XK(1) = VIND(4*KK-11)
            XK(2) = VIND(4*KK-10)
            TH(1) = VIND(4*KK-9)
            TH(2) = VIND(4*KK-8)
            PROD  = SIGDCE(3*K-2)*(XK(1)-RC(K)*TH(1)) +
     &              SIGDCE(3*K)*(XK(2)-RC(K)*TH(2))/DEUX

            DO 14 L = 1, NBMECA

              LL = INDI(L)
              IF (LL .LT. 4) THEN

Ckh --- traction
                IF ((P(L)/PREF).GT.TOLE1) THEN
                  DPSI =MDIL+Q(L)/P(L)
                ELSE
                  DPSI =MDIL+1.D+6*Q(L)/PREF
                ENDIF

                IF ((-QE(K)/PREF).GT.TOLE1) THEN
                  DFDL(K,L) = DFDL(K,L) + B*M*PEK*BETA*
     &                      (-PROD/QE(K)+ RC(K))
     &                      * KSI(L)*COEF*DPSI
                ELSE
                  DFDL(K,L) = DFDL(K,L) + B*M*PEK*BETA*RC(K)
     &                      * KSI(L)*COEF*DPSI
                ENDIF

              ELSEIF (LL .EQ. 4) THEN

                IF((-QE(K)/PREF).LT.TOLE1)THEN
                  DFDL(K,L) = DFDL(K,L) + B*M*PEK*BETA*RC(K)
                ELSE
                  DFDL(K,L) = DFDL(K,L) + B*M*PEK*BETA*
     &                      (-PROD/QE(K) + RC(K))
                ENDIF

              ELSEIF ((LL .GT. 4) .AND. (LL .LT. 8)) THEN

                CALL HUJPRJ (LL-4, YE, SIGD, TP, TP1)
                PS = 2*SIGD(1)*SIGDCE(3*L-2)+SIGD(3)*SIGDCE(3*L)
Ckh --- traction
                IF (((P(L)/PREF).GT.TOLE1) .AND.
     &              ((-Q(L)/PREF).GT.TOLE1)) THEN
                  DPSI =MDIL+PS/(2.D0*P(L)*Q(L))
                ELSEIF (((P(L)/PREF).LE.TOLE1) .AND.
     &                 ((-Q(L)/PREF).GT.TOLE1)) THEN
                  DPSI =MDIL+PS/(2.D-6*PREF*Q(L))
                ELSE
                  DPSI =MDIL
                ENDIF

                IF ((-QE(K)/PREF).LT.TOLE1) THEN
                  DFDL(K,L) = DFDL(K,L) + B*M*PEK*BETA*RC(K)
     &                      * KSI(L)*COEF*DPSI
                ELSE
                  DFDL(K,L) = DFDL(K,L) + B*M*PEK*BETA*
     &                      (-PROD/QE(K) + RC(K))
     &                      * KSI(L)*COEF*DPSI
                ENDIF

              ELSEIF (LL .EQ. 8) THEN

                IF ((-QE(K)/PREF).LT.TOLE1) THEN
                  IF (VIND(22).EQ.UN) THEN
                    DFDL(K,L) = DFDL(K,L) - B*M*PEK*BETA*RC(K)
                  ELSE
                    DFDL(K,L) = DFDL(K,L) + B*M*PEK*BETA*RC(K)
                  ENDIF
                ELSE
                  IF (VIND(22).EQ.UN) THEN
                    DFDL(K,L) = DFDL(K,L) - B*M*PEK*BETA*
     &                        (-PROD*QE(K) + RC(K))
                  ELSE
                    DFDL(K,L) = DFDL(K,L) + B*M*PEK*BETA*
     &                        (-PROD*QE(K) + RC(K))
                  ENDIF
                ENDIF
              ENDIF

 14        CONTINUE

C --- I.2.4. MECANISME ISOTROPE CYCLIQUE
          ELSEIF (KK .EQ. 8) THEN

            F2(K) = -ABS(PE(K)) - D*RC(K)*PC
            IF (F2(K).GT.ZERO) F2(K) = ZERO

            IF(VIND(22).EQ.UN)THEN
              DFDL(K,K) = DFDL(K,K) - D*PC*BETA*
     &                    (RC(K)-VIND(21))
            ELSE
              DFDL(K,K) = DFDL(K,K) + D*PC*BETA*
     &                    (RC(K)+VIND(21))
            ENDIF

            DO 15 L = 1, NBMECA-1, 1

              LL = INDI(L)
              IF(LL.LT.4)THEN

Ckh --- traction
                IF ((P(L)/PREF).GT.TOLE1) THEN
                  DPSI =MDIL+Q(L)/P(L)
                ELSE
                  DPSI =MDIL+1.D+6*Q(L)/PREF
                ENDIF

                IF (VIND(22).EQ.UN) THEN
                  DFDL(K,L) = DFDL(K,L) + D*PC*BETA*
     &                       (RC(K)-VIND(21))
     &                       *KSI(L)*COEF*DPSI
                ELSE
                  DFDL(K,L) = DFDL(K,L) + D*PC*BETA*
     &                       (RC(K)+VIND(21))
     &                       *KSI(L)*COEF*DPSI
                ENDIF

              ELSEIF ((LL.LT.8) .AND. (LL.GT.4)) THEN

                CALL HUJPRJ (LL-4, YE, SIGD, TP, TP1)
                PS = 2*SIGD(1)*SIGDCE(3*L-2)+SIGD(3)*SIGDCE(3*L)

Ckh --- traction
                IF (((P(L)/PREF).GT.TOLE1) .AND.
     &              ((-Q(L)/PREF).GT.TOLE1)) THEN
                  DPSI =MDIL+PS/(2.D0*P(L)*Q(L))
                ELSEIF (((P(L)/PREF).LE.TOLE1) .AND.
     &                 ((-Q(L)/PREF).GT.TOLE1))THEN
                  DPSI =MDIL+PS/(2.D-6*PREF*Q(L))
                ELSE
                  DPSI =MDIL
                ENDIF

                IF (VIND(22).EQ.UN) THEN
                  DFDL(K,L) = DFDL(K,L) + D*PC*BETA*
     &                       (RC(K)-VIND(21))
     &                       *KSI(L)*COEF*DPSI
                ELSE
                  DFDL(K,L) = DFDL(K,L) + D*PC*BETA*
     &                       (RC(K)+VIND(21))
     &                       *KSI(L)*COEF*DPSI
                ENDIF
              ENDIF
  15        CONTINUE

          ELSEIF (KK.GT.8) THEN
            CALL HUJPRJ(KK-8,YE,VEC,TP,TP1)
            F2(K) = -TP - DEUX*RTRAC + PTRAC
            IF (F2(K).GT.ZERO) F2(K) = ZERO
          ENDIF
 11       CONTINUE

C ---- FIN I.2.
C ---> I.3. CALCUL DE DFDRE(K)*DRDLAMB-(K)
          IF (NBMECA.EQ.0) GOTO 160
          DO 16 K = 1, NBMECA, 1

            KK = INDI(K)
            PEK =PE(K) -PTRAC

            IF (KK .LT. 4) THEN

              DFDL(K,K) = DFDL(K,K) + M*PEK*(UN-B*LOG(PEK/PC))
     &                    * (UN-RC(K))**DEUX /AD(K)

            ELSEIF (KK .EQ. 4) THEN

              DFDL(K,K) = DFDL(K,K) + D*PC * (UN-RC(K))**DEUX /CMON

            ELSEIF ((KK .GT. 4) .AND. (KK .LT. 8)) THEN

              TH(1) = VIND(4*KK-9)
              TH(2) = VIND(4*KK-8)

              PROD  = SIGDCE(3*K-2)*TH(1) + SIGDCE(3*K)*TH(2)/DEUX
              IF((-QE(K)/PREF).LT.TOLE1)THEN
                DFDL(K,K) = DFDL(K,K) + M*PEK*(UN-B*LOG(PEK/PC))
     &                      * (UN-RC(K))**DEUX /AD(K)

              ELSE
                DFDL(K,K) = DFDL(K,K) + M*PEK*(UN-B*LOG(PEK/PC))
     &                      * (UN+PROD/QE(K))
     &                      * (UN-RC(K))**DEUX /AD(K)

                IF(DFDL(K,K).EQ.ZERO) DFDL(K,K) = DFDL(K,K) +
     &                        2.D0*M*PEK*(UN-B*LOG(PEK/PC))
     &                      * (UN-RC(K))**DEUX /AD(K)
              ENDIF

            ELSEIF (KK .EQ. 8) THEN

              DFDL(K,K) = DFDL(K,K) + D*PC*(UN-RC(K))**DEUX /CCYC

            ENDIF
 16       CONTINUE

 160   CONTINUE
C ---- RESOLUTION PAR PIVOT DE GAUSS

       CALL MGAUSS ('NCVP', DFDL, F2, 7, NBMECT, 1, DET, IRET)
       IF (IRET.EQ.1) GOTO 9999

C --- MULTIPLICATEUR PLASTIQUE NEGATIF NON AUTORISE
          DO 17 K=1,NBMECT
            IF (F2(K) .LT. ZERO) F2(K)=ZERO
 17         CONTINUE


C ====================================================================
C --- CALCUL DES INCREMENTS DE DEFORMATIONS ELASTIQUE ----------------
C ====================================================================
       DO 240 I = 1, NDT
         DEPSE(I) = DEPS(I)
 240     CONTINUE

       DO 242 K = 1, NBMECT
         KK = (K-1)*NDT
         DO 242 I = 1, NDT
           DEPSE(I) = DEPSE(I) - F2(K)*PSI(KK+I)
 242       CONTINUE

C ====================================================================
C --- CALCUL INCREMENT DE CONTRAINTES  DSIG = HOOKNL-.DEPSE ----------
C ====================================================================
       IF(.NOT. LOOP)CALL LCPRMV (HOOKNL, DEPSE, DSIG)
       CALL LCSOVN (NDT, YD, DSIG, YE)

       MAXI   = UN
       COHES  = -RTRAC+PTRAC
       FACTOR = UN

       DO 280 I = 1, NDI
         CALL HUJPRJ(I,YE,SIGD,PE(I),QE(I))
         CALL HUJPRJ(I,YD,SIGD,P(I),Q(I))
         CALL HUJPRJ(I,DSIG,SIGD,DP(I),Q(I))
         IF (PE(I).GT.COHES .AND. DP(I).GT.TOLE1) THEN
           FACTOR = (-P(I)+COHES)/DP(I)
           IF ((FACTOR.GT.ZERO).AND.(FACTOR.LT.MAXI)) THEN
             MAXI = FACTOR
           ENDIF
         ENDIF
  280    CONTINUE

CKH ON IMPOSE UNE VARIATION DE DSIGMA < 50% DE SIGMA_INIT
CAF A CONDITION QU'IL N'Y AIT PAS DE MECANISMES DE TRACTION ACTIVES
       IF(NBMECT.EQ.NBMECA)THEN
         S=0.D0
         DS=0.D0
         TOL = .5D0
         DO 282 I=1,NDT
           S = S+YD(I)**2.D0
           DS= DS+DSIG(I)**2.D0
 282     CONTINUE
         S=SQRT(S)
         DS=SQRT(DS)

         FACTOR = UN
         IF ((-S/PREF) .GT. TOLE1) THEN
           IF (DS/S .GT. TOL) FACTOR = TOL*S/DS
         ELSEIF ((-DS/PREF) .GT. TOL) THEN
           FACTOR = -TOL*PREF/DS
         ENDIF

         MAXI=MIN(FACTOR,MAXI)
       ENDIF
C ---> SI IL EXISTE SIG(I)>0, ALORS MODIFICATION DE LA PREDICTION
       IF (MAXI.LT.UN) THEN
         DO 290 I = 1, NDT
           DSIG(I) = MAXI * DSIG(I)
  290      CONTINUE
         IF (DEBUG) THEN
           WRITE (6,'(A,A,E12.5)')
     &     'HUJIID FIN:: APPLICATION DE FACTOR POUR MODIFIER ',
     &     'LA PREDICTION -> FACTOR =',FACTOR
           WRITE(6,*)'YE =',(YD(I)+DSIG(I),I=1,NDT)
         ENDIF
       ENDIF

C ====================================================================
C --- CALCUL INCREMENT DE LA VARIABLE INTERNE RC ---------------------
C ====================================================================
       IF (NBMECA.EQ.0) GOTO 281

       DO 250 K = 1, NBMECA
          KK = INDI(K)
          IF (KK.LT.4) THEN
            DR(K) =  F2(K) *(UN-RC(K))**DEUX /AD(K)
            IF ((YD(NDT+1+K)+DR(K)).GT.UN) THEN
              F2(K) = (UN-RC(K))/((UN-RC(K))**DEUX /AD(K))
              DR(K) = F2(K)*(UN-RC(K))**DEUX/AD(K)
            ENDIF
          ELSEIF(KK.EQ.4) THEN
            DR(K) =  F2(K) *(UN-RC(K))**DEUX /CMON

          ELSEIF ((KK .LT. 8) .AND. (KK .GT. 4)) THEN
            DR(K) =  F2(K) *(UN-RC(K))**DEUX /AD(K)
            IF ((YD(NDT+1+K)+DR(K)).GT.VIND(KK-4)) THEN
              F2(K) = (VIND(KK-4)-RC(K))/
     &                ((UN-RC(K))**DEUX /AD(K))
              DR(K) = F2(K)*(UN-RC(K))**DEUX/AD(K)
            ENDIF

          ELSEIF (KK .EQ. 8) THEN
            DR(K) =  F2(K) *(UN-RC(K))**DEUX /CCYC

          ENDIF
 250      CONTINUE

 281      CONTINUE
C ====================================================================
C --- CALCUL INCREMENT DE LA VARIABLE INTERNE DEPSVP -----------------
C ====================================================================
        DEPSVP = ZERO
        DO 251 K = 1, NBMECT
          KK=INDI(K)
          IF (KK .LT. 4) THEN

Ckh --- traction
            IF ((P(K)/PREF).GT.TOLE1) THEN
              DPSI =MDIL+Q(K)/P(K)
            ELSE
              DPSI =MDIL+1.D+6*Q(K)/PREF
            ENDIF
            DEPSVP = DEPSVP - F2(K)*COEF*KSI(K)*DPSI

          ELSEIF (KK .EQ. 4) THEN

            DEPSVP = DEPSVP - F2(K)

          ELSEIF ((KK .LT. 8) .AND. (KK .GT. 4)) THEN

            CALL HUJPRJ (KK-4, YD, SIGD, TP, TP1)
            PS = 2*SIGD(1)*SIGDC(3*K-2)+SIGD(3)*SIGDC(3*K)
Ckh --- traction
            IF (((P(K)/PREF).GT.TOLE1) .AND.
     &              ((-Q(K)/PREF).GT.TOLE1)) THEN
              DPSI =MDIL+PS/(2.D0*P(K)*Q(K))
            ELSEIF (((P(K)/PREF).LE.TOLE1) .AND.
     &             ((-Q(K)/PREF).GT.TOLE1)) THEN
              DPSI =MDIL+PS/(2.D-6*PREF*Q(K))
            ELSE
              DPSI =MDIL
            ENDIF

            DEPSVP = DEPSVP - F2(K)*COEF*KSI(K)*DPSI

          ELSEIF (KK .EQ. 8) THEN
            IF(VIND(22).EQ.UN)THEN
              DEPSVP = DEPSVP + F2(K)
            ELSE
              DEPSVP = DEPSVP - F2(K)
            ENDIF

          ENDIF

 251      CONTINUE

C ====================================================================
C --- SOLUTION D ESSAI -----------------------------------------------
C ====================================================================
        DO 259 I = 1, 18
          DY(I) = ZERO
 259      CONTINUE

        DO 260 I = 1, NDT
          DY(I) = DSIG(I)
 260      CONTINUE

        IF(ABS(DEPSVP).LT.1.D-1)THEN
           DY(NDT+1) = DEPSVP
        ELSE
           DY(NDT+1) = ZERO
        ENDIF

        IF (NBMECA.EQ.0) GOTO 271
        DO 270 K = 1, NBMECA
          DY(NDT+1+K)        = DR(K)
          DY(NDT+1+NBMECA+K) = F2(K)
 270      CONTINUE

 271    CONTINUE

        IF (NBMECA.LT.NBMECT) THEN
          DO 272 I = 1, NBMECT
            IF (INDI(I).GT.8) DY(NDT+1+NBMECA+I) = F2(I)
 272        CONTINUE
        ENDIF

        GOTO 9999

 999    CONTINUE

        IF (DEBUG) THEN
          CALL TECAEL(IADZI,IAZK24)
          NOMAIL = ZK24(IAZK24-1+3) (1:8)
          WRITE (IFM,'(10(A))')
     &    'HUJIID :: LOG(PK/PC) NON DEFINI DANS LA MAILLE',NOMAIL
          WRITE (IFM,'(A)') '          ON NE FAIT PAS LA PREDICTION'
        ENDIF

        DO 300 I = 1, 18
          DY(I) = ZERO
 300      CONTINUE

 9999   CONTINUE

        END

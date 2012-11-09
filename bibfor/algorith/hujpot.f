      SUBROUTINE HUJPOT (MOD, MATER, VIND, DEPSH, SIGD, SIGE,
     &                   ETATF, RDCTPS, IRET, AREDEC)
      IMPLICIT NONE
C          CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C   ------------------------------------------------------------------
C   DEFINITION DU DOMAINE POTENTIEL DES MECANISMES ACTIFS
C   IN  MOD    :  MODELISATION
C       MATER  :  COEFFICIENTS MATERIAU A T+DT
C       VIND   :  VARIABLES INTERNES  A T
C       DEPS   :  INCREMENT DE DEFORMATION
C       SIGD   :  CONTRAINTE A T
C       SIGE   :  CONTRAINTE A T+DT  (ELAS)
C
C   OUT VIND   :  VARIABLES INTERNES MODIFIEES PAR LES NOUVEAUX
C                 MECANISMES
C       ETATF  :  ETAT PLASTIQUE OU ELASTIQUE DU POINT CONSIDERE
C       RDCTPS :  REDECOUPAGE DU PAS DE TEMPS SI NECESSAIRE
C       AREDEC :  DECOUPAGE LOCAL ACTIF = .TRUE.
C       IRET   :  CODE RETOUR DE  L'INTEGRATION DE LA LOI DE HUJEUX
C                    IRET=0 => PAS DE PROBLEME
C                    IRET=1 => ECHEC
C   ------------------------------------------------------------------
      INTEGER     NDT, NDI, ELAS, JJ, IRET
      INTEGER      I, INDI(7), J, MONO(7),HIST(4,2)
      REAL*8      TOLE1, SIGD(6),SIGE(6)
      REAL*8      VIND(*), CHARGE
      REAL*8      MATER(22,2), UN, ZERO, PREF
      REAL*8      AL, LA, DEMU, E, NU, I1E, YE(18), YD(18)
      REAL*8      HOOKNL(6,6), DFDS(6), DEPSH(6), DSIG(6)
      REAL*8      ACTIF, DPSIDS(6,6), N, DEUX, SEUIL
      REAL*8         DEPS(6)
      REAL*8      E1,E2,E3,NU12,NU13,NU23,G1,G2,G3,NU21,NU31,NU32,DELTA
      REAL*8      R8PREM, PISO
      LOGICAL     DEBUG, PROX, RDCTPS, AREDEC, BID
      CHARACTER*7 ETATF
      CHARACTER*8 MOD
      REAL*8      VINM(50), SEUILM

C ----------------------------------------------------------------------
      COMMON /TDIM/   NDT, NDI
      COMMON /MESHUJ/ DEBUG
C ----------------------------------------------------------------------
      PARAMETER   (TOLE1 = 1.D-7)
      PARAMETER   (UN   = 1.D0)
      PARAMETER   (ZERO = 0.D0)
      PARAMETER   (DEUX = 2.D0)

C ======================================================================
C -------------------- DETERMINATION DES CRITERES ACTIFS A T ----------
C ======================================================================
      IF (DEBUG) WRITE(6,'(A)')' ==> HUJPOT'
      IF (DEBUG) WRITE(6,*)'     INIT - VIND=',(VIND(23+I),I=1,8)
C --- MISE A ZERO POUR CORRIGER ZERO NUMERIQUE
      DO 10 I = 1, NDT
        DEPS(I) = DEPSH(I)
        IF (ABS(DEPS(I)).LT.R8PREM()) DEPS(I)=ZERO
  10    CONTINUE
      ELAS = 0
      RDCTPS = .FALSE.

C ====================================================================
C --- CONSTRUCTION DES SURFACES CYCLIQUES PRECEDENTES -----------
C ====================================================================
      CALL LCEQVN(50,VIND,VINM)
      DO 50 I = 1, 3
        IF ((VIND(5*I+31).NE.ZERO).OR.
     &     (VIND(5*I+32).NE.ZERO)) THEN
          VINM(4*I+5) = VIND(5*I+31)
          VINM(4*I+6) = VIND(5*I+32)
          VINM(4*I+7) = VIND(5*I+33)
          VINM(4*I+8) = VIND(5*I+34)
          VINM(I+4)   = VIND(5*I+35)
        ENDIF
  50    CONTINUE

C ====================================================================
C --- PROPRIETES MATERIAU HUJEUX -------------------------------------
C ====================================================================
      N     = MATER(1,2)
      PREF  = MATER(8,2)
      PISO  = 1.5D0*MATER(21,2)
      PISO  = ZERO

C ====================================================================
C ------------------ INITIALISATION VARIABLES ------------------------
C ====================================================================

      DO 14 I = 1, 7
        MONO(I) = 0
        INDI(I) = 0
  14    CONTINUE
      DO 15 I = 1, 4
        HIST(I,1) = 0
        HIST(I,2) = 0
        MONO(I)   = I
  15    CONTINUE
      DO 25 I = 1, NDT
        YE(I) = SIGE(I)
        YD(I) = SIGD(I)
  25    CONTINUE
      YE(NDT+1) = VIND(23)
      YD(NDT+1) = VIND(23)

C ====================================================================
C --------------------- I) CONSTRUCTION DE C -------------------------
C ====================================================================
      CALL LCINMA (ZERO, HOOKNL)
      I1E  = (SIGE(1)+SIGE(2)+SIGE(3))/3.D0

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
          CALL U2MESS('F', 'COMPOR1_37')
        ENDIF

      ELSEIF (MOD(1:6) .EQ. 'C_PLAN' .OR.
     &        MOD(1:2) .EQ. '1D')   THEN

        CALL U2MESS('F', 'COMPOR1_4')

      ENDIF


C ====================================================================
C -------------- CALCUL DE DSIGMA = C*DEPSILON -----------------------
C ====================================================================
      CALL LCPRMV (HOOKNL, DEPS, DSIG)


C ====================================================================
C ----------- DETERMINATION DES CRITERES ACTIFS PRECEDEMMENT ---------
C ====================================================================
      J = 0
      DO 400 I = 1, 4

        IF ((VIND(23+I).EQ.UN).OR.(VIND(23+I).EQ.ZERO)) THEN
          YE(NDT+1+I) = VIND(I)
          YD(NDT+1+I) = VIND(I)
          HIST(I,1)   = I
          INDI(I)     = I
          IF (VIND(23+I).EQ.UN) THEN
            HIST(I,2)   = 1
          ELSE
            HIST(I,2)   = 0
          ENDIF
        ELSE
          YE(NDT+1+I) = VIND(I+4)
          YD(NDT+1+I) = VIND(I+4)
          HIST(I,1)   = I + 4
          INDI(I)     = I + 4
          IF (VIND(27+I).EQ.UN) THEN
            HIST(I,2)   = 1
          ELSE
            HIST(I,2)   = 0
          ENDIF
        ENDIF


C ====================================================================
C --------------------- CALCUL DE DFDS*C(SIGE)*DEPS ------------------
C ====================================================================
        CALL HUJDDD ('DFDS  ', INDI(I), MATER, INDI, YE, VIND,
     &               DFDS, DPSIDS,IRET)
        IF (IRET.EQ.1) THEN
          IRET = 0
          IF (.NOT.AREDEC) THEN
            RDCTPS = .TRUE.
            GOTO 999
          ELSE
            CALL HUJDDD ('DFDS  ', INDI(I), MATER, INDI, YD, VIND,
     &               DFDS, DPSIDS,IRET)
            IF (IRET.EQ.1) GOTO 999
          ENDIF
        ENDIF
        ACTIF = ZERO
        DO 45 JJ = 1, NDT
          ACTIF = ACTIF + DSIG(JJ)*DFDS(JJ)
 45       CONTINUE

        ACTIF = ACTIF/MATER(1,1)
        CHARGE = -UN
        IF (INDI(I).GT.4) THEN
          YE(NDT+1+I) = VIND(I)
          YD(NDT+1+I) = VIND(I)
          CALL HUJDDD ('DFDS  ', MONO(I), MATER, MONO, YE, VIND,
     &                  DFDS, DPSIDS,IRET)

          IF (IRET.EQ.1) THEN
            IRET = 0
            IF (.NOT.AREDEC) THEN
              RDCTPS = .TRUE.
              GOTO 999
            ELSE
              CALL HUJDDD ('DFDS  ', MONO(I), MATER, MONO, YD, VIND,
     &                  DFDS, DPSIDS,IRET)
              IF (IRET.EQ.1) GOTO 999
            ENDIF
          ENDIF

          CHARGE = ZERO
          DO 46 JJ = 1, NDT
            CHARGE = CHARGE + DSIG(JJ)*DFDS(JJ)
 46         CONTINUE
          CHARGE = CHARGE/MATER(1,1)
        ENDIF

        IF (DEBUG) THEN
          WRITE(6,*)'INDI   = ',INDI(I)
          WRITE(6,*)'ACTIF  = ',ACTIF
          WRITE(6,*)'CHARGE = ',CHARGE
          WRITE(6,*)
        ENDIF

C ====================================================================
C --------------------- CRITERE MONOTONE ACTIF ? ---------------------
C ====================================================================
        IF (CHARGE.GE.(-R8PREM())) THEN
          IF (INDI(I).NE.8) THEN
              CALL HUJPXD(INDI(I), MATER, SIGD ,VIND, PROX, BID)
              IF (PROX) THEN
                VIND(19+INDI(I))   = UN
                VIND(23+INDI(I))   = ZERO
                VIND(INDI(I)*4-11) = ZERO
                VIND(INDI(I)*4-10) = ZERO
                VIND(INDI(I)*4-9)  = ZERO
                VIND(INDI(I)*4-8)  = ZERO
                VIND(INDI(I))      = MATER(18,2)
                GOTO 400
C              ELSEIF(.NOT.AREDEC)THEN
C --> SINON ==> REDECOUPAGE DU PAS DE TEMPS
C                RDCTPS = .TRUE.
C                GOTO 999
              ENDIF
          ELSE
            CALL HUJPXS(MATER, SIGD, VIND, PROX)
              IF (PROX) THEN
                VIND(21) = ZERO
                VIND(22) = ZERO
                VIND(27) = UN
                VIND(31) = ZERO
                GOTO 400
C              ELSEIF(.NOT.AREDEC)THEN
C --> SINON ==> REDECOUPAGE DU PAS DE TEMPS
C                RDCTPS = .TRUE.
C                GOTO 999
              ENDIF
          ENDIF
        ENDIF


C =====================================================================
C -------------------------- CRITERE ACTIF ----------------------------
C =====================================================================
        IF (INDI(I).LT.5) THEN

C **************************
C --- CRITERES MONOTONES ---
C **************************
          IF (HIST(I,2).EQ.1) THEN
            IF (ACTIF.GE.(-R8PREM())) THEN
              VIND(23+INDI(I)) = UN
            ELSE
              VIND(23+INDI(I)) = -UN
              IF (INDI(I).LT.4) THEN
                CALL HUJMED(INDI(I), MATER, VIND, SIGD)
                VIND(I+4) = MATER(18,2)
                CALL HUJCDC(INDI(I), MATER, SIGE, VIND, SEUIL)
                IF (VIND(I+4).EQ.UN) SEUIL = - UN
              ELSEIF (INDI(I).EQ.4) THEN
                CALL HUJMEI(VIND)
                VIND(8) = MATER(19,2)
                CALL HUJCIC(MATER, SIGE, VIND, SEUIL)
              ENDIF
              IF (SEUIL.GT.TOLE1) THEN
                VIND(27+INDI(I)) = UN
              ELSE
                VIND(27+INDI(I)) = ZERO
                ELAS = ELAS + 1
              ENDIF
            ENDIF
          ELSE
            IF (ACTIF.GE.(-R8PREM())) THEN
              IF (INDI(I).LT.4) THEN
                CALL HUJCRD(I, MATER, SIGE, VIND, SEUIL)
              ELSE
                CALL HUJCRI(MATER, SIGE, VIND, SEUIL)
              ENDIF
              IF (SEUIL.GT.TOLE1) THEN
                VIND(23+INDI(I)) = UN
              ELSE
                VIND(23+INDI(I)) = ZERO
                ELAS = ELAS + 1
              ENDIF
            ELSE
              VIND(23+INDI(I)) = ZERO
              ELAS = ELAS + 1
            ENDIF
          ENDIF
        ELSE


C **************************
C --- CRITERES CYCLIQUES ---
C **************************
          IF (HIST(I,2).EQ.1) THEN
            IF (ACTIF.GE.(-R8PREM())) THEN
              IF (VIND(I+4).LT.UN) THEN
                VIND(23+INDI(I)) = UN
              ELSE
                VIND(23+INDI(I)) = ZERO
              ENDIF
            ELSE
              IF (INDI(I).LT.8) THEN
                CALL HUJMED(INDI(I), MATER, VIND, SIGD)
                VIND(I+4) = MATER(18,2)
                CALL HUJCDC(INDI(I)-4, MATER, SIGE, VIND, SEUIL)
                IF (VIND(I+4).EQ.UN) SEUIL = -UN
              ELSE
                CALL HUJMEI(VIND)
                VIND(8) = MATER(19,2)
                CALL HUJCIC(MATER, SIGE, VIND, SEUIL)
              ENDIF
              IF (SEUIL.GT.TOLE1) THEN
                VIND(23+INDI(I)) = UN
              ELSE
                VIND(23+INDI(I)) = ZERO
                ELAS = ELAS + 1
              ENDIF
            ENDIF
          ELSE
            IF (ACTIF.GE.(-R8PREM())) THEN
              IF ((INDI(I).GT.4).AND.(INDI(I).LT.8)) THEN
                CALL HUJCDC(INDI(I)-4, MATER, SIGE, VIND, SEUIL)
                IF (VIND(I+4).EQ.UN) SEUIL = -UN
                IF (SEUIL.GT.TOLE1) THEN
                  VIND(23+INDI(I)) = UN
                  IF ((VIND(5*I+31).NE.ZERO).OR.
     &               (VIND(5*I+32).NE.ZERO)) THEN
                  CALL HUJCDC(INDI(I)-4, MATER, SIGE, VINM, SEUILM)
                    IF (SEUILM.GT.TOLE1) THEN
                      VIND(4*I+5) = VIND(5*I+31)
                      VIND(4*I+6) = VIND(5*I+32)
                      VIND(4*I+7) = VIND(5*I+33)
                      VIND(4*I+8) = VIND(5*I+34)
                      VIND(I+4)   = VIND(5*I+35)
                      VIND(5*I+31) = ZERO
                      VIND(5*I+32) = ZERO
                      VIND(5*I+33) = ZERO
                      VIND(5*I+34) = ZERO
                      VIND(5*I+35) = MATER(18,2)
                    ENDIF
                  ENDIF
                ELSE
                  VIND(23+INDI(I)) = ZERO
                  ELAS = ELAS + 1
                ENDIF
              ELSE
                CALL HUJCIC(MATER, SIGE, VIND, SEUIL)
                IF (SEUIL.GT.TOLE1) THEN
                  VIND(23+INDI(I)) = UN
                ELSE
                  VIND(23+INDI(I)) = ZERO
                  ELAS = ELAS + 1
                ENDIF
              ENDIF
            ELSE
              SEUIL = ZERO
              IF (INDI(I).LT.8) THEN
                IF (VIND(I+4).NE.MATER(18,2)) THEN
                  CALL HUJMED(INDI(I), MATER, VIND, SIGD)
                  VIND(I+4) = MATER(18,2)
                  CALL HUJCDC(INDI(I)-4, MATER, SIGE, VIND, SEUIL)
                ENDIF
              ELSE
                CALL HUJMEI(VIND)
                VIND(8) = MATER(19,2)
                CALL HUJCIC(MATER, SIGE, VIND, SEUIL)
              ENDIF
              IF (SEUIL.GT.TOLE1) THEN
                VIND(23+INDI(I)) = UN
              ELSE
                VIND(23+INDI(I)) = ZERO
                ELAS = ELAS + 1
              ENDIF
            ENDIF
          ENDIF
        ENDIF

 400    CONTINUE


C ======================================================================
C ---------------- DETERMINATION ETAT ELASTIQUE OU PLASTIQUE -----------
C ======================================================================
      IF (ELAS .EQ. 4) THEN
        ETATF = 'ELASTIC'
      ELSE
        ETATF = 'PLASTIC'
      ENDIF

 999  CONTINUE
      IF (DEBUG) WRITE(6,*)'FIN - VIND=',(VIND(23+I),I=1,8)
      END

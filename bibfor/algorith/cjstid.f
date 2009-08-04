       SUBROUTINE CJSTID ( MOD, MATER, NVI, EPS, SIG, VIN, DSDE )
        IMPLICIT NONE
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/08/2009   AUTEUR MEUNIER S.MEUNIER 
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
C       ----------------------------------------------------------------
C     CALCUL DE LA MATRICE TANGENTE DU PROBLEME CONTINU DE LA LOI CJS
C     POUR LE MECANISME PLASTIQUE DEVIATOIRE
C     IN   MOD     :  MODELISATION
C          MATER   :  COEFFICIENTS MATERIAU
C          NVI     :  NB DE VARIABLES INTERNES
C          EPS     :  DEFORMATIONS
C          SIG     :  CONTRAINTES
C          VIN     :  VARIABLES INTERNES
C     OUT  DSDESY  :  MATRICE TANGENTE SYMETRISEE
C ======================================================================
        INTEGER     NDT, NDI , NVI, I, J
        REAL*8      MATER(14,2), VIN(*), SIG(6), DSDE(6,6)
        REAL*8      HOOK(6,6), I1, S(6), SII, HTS, SIIC, NORM(6)
        REAL*8      EPS(6), EPSV, TRACE, COS3T, HLODE, BETA, BETAPR
        REAL*8      QISO, R, GR, X(6), GX(6), XII, GAMMA, MUCJS
        REAL*8      KOE, KE, KOP, KP, N, RM, RC, A, B, C, PCO, PA
        REAL*8      E, NU, AL, LA, MU, Q(6), QII, QQ(6), QQII
        REAL*8      PC, COS3TQ, HTQ, COSA, COSDIF, RR, PHI, PHIO
        REAL*8      COS3TS, TANGS, TANGQ, TETAS, TETAQ, TROIS
        REAL*8      DFDDS(6), GD(6), TRGD, ZERO, D12, UN, DEUX
        REAL*8      HDEV, QGX, DFHGD, DFH(6), HGD(6), HK(6), DFHK, MUN5
        REAL*8      T1(6), T2(6), KRON(6), EPSSIG, SIIREL, PREF,QINIT
        REAL*8      TRUC, SIGNE, COEF3, COEF4, COEF5
        CHARACTER*8 MOD
C ======================================================================
        COMMON /TDIM/   NDT, NDI
C ======================================================================
        PARAMETER   ( EPSSIG = 1.D-8 )
        DATA          KRON  /1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
        DATA          MUN5  /-1.5D0/
        DATA          ZERO  / 0.D0 /
        DATA          D12   / .5D0 /
        DATA          UN    / 1.D0 /
        DATA          DEUX  / 2.D0 /
        DATA          TROIS / 3.D0 /
C ======================================================================
        CALL JEMARQ ()
C ======================================================================
C - RECUPERATION DES GRANDEURS UTILES : I1, VARIABLES INTERNES R ET X, -
C --- PARAMETRES CJS, RIGIDITE HOOK ------------------------------------
C ======================================================================
        PA    = MATER(12,2)
        QINIT = MATER(13,2)
C ======================================================================
C --- CALCUL DE LA TRACE DE SIG ----------------------------------------
C ======================================================================
        I1 = TRACE(NDI,SIG)
        IF( (I1+QINIT).EQ.0.0D0 ) THEN
           I1  = -QINIT+1.D-12 * PA
           PREF = ABS(PA)
        ELSE
           PREF = ABS(I1+QINIT)
        ENDIF
C ======================================================================
        QISO = VIN(1)
        R = VIN(2)
        DO 15 I = 1,NDT
        X(I) = VIN(I+2)
  15    CONTINUE
C ======================================================================
C --- RECUPERATION DES DONNEES MATERIAUX -------------------------------
C ======================================================================
        KOE   = MATER(1,1)/TROIS/( UN-DEUX*MATER(2,1) )
        BETA  = MATER(1,2)
        RM    = MATER(2,2)
        N     = MATER(3,2)
        KOP   = MATER(4,2)
        RC    = MATER(5,2)
        A     = MATER(6,2)
        B     = MATER(7,2)
        C     = MATER(8,2)
        GAMMA = MATER(9,2)
        MUCJS = MATER(10,2)
        PCO   = MATER(11,2)
        PA    = MATER(12,2)
        KE    = KOE * ((I1+QINIT)/TROIS/PA)**N
        KP    = KOP * (QISO/PA)**N
C ======================================================================
C --- OPERATEUR DE RIGIDITE CALCULE A ITERARTION -----------------------
C ======================================================================
        E  = MATER(1,1) * ((I1+QINIT)/TROIS/PA)**N
        NU = MATER(2,1)
        AL = E  * (UN-NU) / (UN+NU) / (UN-DEUX*NU)
        LA = NU * E       / (UN+NU) / (UN-DEUX*NU)
        MU = E  * D12     / (UN+NU)
        CALL LCINMA ( ZERO , HOOK )
C ======================================================================
C --- 3D/DP/AX ---------------------------------------------------------
C ======================================================================
        IF ( MOD(1:2) .EQ. '3D'     .OR.
     &       MOD(1:6) .EQ. 'D_PLAN' .OR.
     &       MOD(1:4) .EQ. 'AXIS'        )THEN
             DO 20 I = 1,NDI
             DO 20 J = 1,NDI
                 IF(I.EQ.J) HOOK(I,J) = AL
                 IF(I.NE.J) HOOK(I,J) = LA
 20          CONTINUE
             DO 30 I = NDI+1 , NDT
             DO 30 J = NDI+1 , NDT
                 IF(I.EQ.J) HOOK(I,J) = DEUX* MU
 30          CONTINUE
C ======================================================================
C --- CP/1D ------------------------------------------------------------
C ======================================================================
        ELSE IF ( MOD(1:6) .EQ. 'C_PLAN' .OR.
     &            MOD(1:2) .EQ. '1D' )THEN
             CALL U2MESS('F','ALGORITH2_15')
        ENDIF
C ======================================================================
C --- LOIS D'ECOUISSAGE DES VARIABLES INTERNES R ET X ------------------
C ======================================================================
C --- ECROUISSAGE ISOTROPE ---------------------------------------------
C ======================================================================
        GR = - A * (UN-R/RM)**DEUX * (I1+QINIT) *
     &                                       ((I1+QINIT)/TROIS/PA)**MUN5
C ======================================================================
C --- ECROUISSAGE CINEMATIQUE ------------------------------------------
C ======================================================================
        CALL     LCDEVI(SIG,S)
        CALL     LCPRSC(S,S,TRUC)
        SII    = SQRT(TRUC)
        SIIREL = SII/PREF
        COS3TS = COS3T(S, PREF, EPSSIG)
        HTS    = HLODE(GAMMA,COS3TS)
C ======================================================================
        CALL     CJSQIJ(S, I1, X, Q)
        CALL     LCPRSC(Q,Q,TRUC)
        QII    = SQRT(TRUC)
        COS3TQ = COS3T(Q, PREF, EPSSIG)
        HTQ    = HLODE(GAMMA,COS3TQ)
C ======================================================================
C --- CALCUL DE Q ------------------------------------------------------
C ======================================================================
        CALL CALCQ(Q, GAMMA, PREF, EPSSIG, QQ)
C ======================================================================
C --- CALCUL DE PC (CONTRAINTE MOYENNE CRITIQUE) -----------------------
C ======================================================================
        CALL   LCPRSC(QQ,QQ,TRUC)
        QQII = SQRT(TRUC)
        CALL   LCPRSC(X,X,TRUC)
        XII  = SQRT(TRUC)
        EPSV = TRACE(NDI,EPS)
        PC = PCO*EXP(-C*EPSV)
C ======================================================================
C --- CALCUL DE LA FONCTION PHI ----------------------------------------
C ======================================================================
        IF(XII .LE. EPSSIG) THEN
        PHI = UN
        ELSE IF(SIIREL .LE. EPSSIG) THEN
              COSA = UN
              COSDIF = UN
              RR = RC + MUCJS*MAX(ZERO,LOG(TROIS*PC/(I1+QINIT)))
              PHIO = COSA/( RR - HTS/HTQ*RM*COSDIF)
              PHI = PHIO * HTS * QQII
        ELSE
              COSA =  ( QII*QII - SII*SII - I1*XII*I1*XII ) /
     &                 (DEUX*SII*I1*XII)

              TANGS = SQRT(UN-COS3TS*COS3TS) / COS3TS
              TANGQ = SQRT(UN-COS3TQ*COS3TQ) / COS3TQ
              TETAS = ATAN2(TANGS,1.D0) / TROIS
              TETAQ = ATAN2(TANGQ,1.D0) / TROIS
              COSDIF = COS(TETAS-TETAQ)

              RR = RC + MUCJS*MAX(ZERO,LOG(TROIS*PC/(I1+QINIT)))
              PHIO = COSA/( RR - HTS/HTQ*RM*COSDIF)
              PHI = PHIO * HTS * QQII
        ENDIF

        DO 60 I=1, NDT
        GX(I) = (I1+QINIT)/B*( QQ(I) + PHI*X(I) ) *
     &           ((I1+QINIT)/TROIS/PA)**MUN5
 60     CONTINUE
C ======================================================================
C --- LOI D'ECOULEMENT DU MECANISME DEVIATOIRE -------------------------
C ======================================================================
        CALL LCPRSC(QQ,X,TRUC)
        TRUC = TRUC - R

        DO 70 I = 1,NDI
           DFDDS(I) = QQ(I) - TRUC
  70    CONTINUE

        DO 80 I = NDI+1, NDT
           DFDDS(I) = QQ(I)
  80    CONTINUE

        SIIC = -RC * (I1+QINIT) / HTS
        SIGNE = VIN(NVI-1)
        BETAPR = BETA * (SII/SIIC - UN) * SIGNE
        COEF4 = UN / SQRT( BETAPR*BETAPR + TROIS )
        COEF3 = COEF4 * BETAPR / SII

        DO 90 I = 1,NDT
           NORM(I) = COEF3 * S(I) + COEF4 * KRON(I)
  90    CONTINUE

        CALL LCPRSC(DFDDS,NORM,TRUC)
        DO 100 I = 1,NDT
           GD(I) = DFDDS(I) - TRUC * NORM(I)
  100   CONTINUE

        TRGD = ZERO
        DO 115 I = 1,NDI
           TRGD = TRGD + GD(I)
  115   CONTINUE
C ======================================================================
C --- MODULE PLASTIQUE DEVIATOIRE : HDEV -------------------------------
C ======================================================================
        QGX = ZERO
        DO 110 I = 1,NDT
           QGX = QGX + QQ(I) * GX(I)
  110   CONTINUE
        HDEV = (I1+QINIT) * (- GR + QGX)
C ======================================================================
C --- CALCUL DU TERME DFDDS.HOOK.GD : DFHGD ----------------------------
C ======================================================================
        DFHGD = ZERO
        DO 120 I = 1, NDT
        DO 120 J = 1, NDT
           DFHGD = DFHGD + DFDDS(I)*HOOK(I,J)*GD(J)
  120   CONTINUE
C ======================================================================
C --- CALCUL DU TERME HOOK.GD : HGD ------------------------------------
C ======================================================================
        CALL LCINVE( ZERO,HGD)
        DO 130 I = 1, NDT
        DO 130 J = 1, NDT
           HGD(I) = HGD(I) + HOOK(I,J)*GD(J)
  130   CONTINUE
C ======================================================================
C --- CALCUL DU TERME HOOK.KRON : HK -----------------------------------
C ======================================================================
        CALL LCINVE( ZERO,HK)
        DO 140 I = 1, NDT
        DO 140 J = 1, NDT
           HK(I) = HK(I) + HOOK(I,J)*KRON(J)
  140   CONTINUE
C ======================================================================
C --- CALCUL DU TERME DFDDS.HOOK : DFH ---------------------------------
C ======================================================================
        CALL LCINVE( ZERO,DFH)
        DO 150 I = 1, NDT
        DO 150 J = 1, NDT
           DFH(I) = DFH(I) + DFDDS(J)*HOOK(J,I)
  150   CONTINUE
C ======================================================================
C --- CALCUL DU TERME DFDDS.HOOK.K : DFHK ------------------------------
C ======================================================================
        DFHK = ZERO
        DO 160 I = 1, NDT
        DO 160 J = 1, NDT
           DFHK = DFHK + DFDDS(I)*HOOK(I,J)*KRON(J)
  160   CONTINUE
C ======================================================================
C --- CALCUL DU TERME (KE+KP)*(HDEV+DFHGD)-1/3*KE*TRGD*DFHK : COEF5 ----
C ======================================================================
        COEF5 = (KE+KP)*(HDEV+DFHGD)-KE/TROIS*TRGD*DFHK
C ======================================================================
C --- CALCUL DU TERME (KE*(HDEV+DFHGD)*KRON-KE*TRGD*DFH)/COEF5 : T1 ----
C ======================================================================
        CALL LCINVE( ZERO,T1)
        DO 170 I = 1, NDT
           T1(I) =  KE/COEF5*((HDEV+DFHGD)*KRON(I) - TRGD*DFH(I))
  170   CONTINUE
C ======================================================================
C --- CALCUL DU TERME ((KE+KP)*DFH-1/3*KE*DFHK*KRON)/COEF5 : T2 --------
C ======================================================================
        CALL LCINVE( ZERO,T2)
        DO 180 I = 1, NDT
           T2(I) = ((KE+KP)*DFH(I) - KE/TROIS*DFHK*KRON(I))/COEF5
  180   CONTINUE
C ======================================================================
C --- CALCUL DE DSDE(I,J,K,L) = ----------------------------------------
C ======================================================================
C                      1
C    HOOK(I,J,K,L) -  --- * HOOK(I,J,P,Q)*KRON(P,Q)*T1(K,L)
C                      3
C ======================================================================
C                  - HOOK(I,J,M,N)*GD(M,N)*T2(K,L)
C ======================================================================
C --- C'EST A DIRE :  DSDE = HOOK - HK*T1/TROIS - HGD*T2 ---------------
C ======================================================================
        CALL MATINI(6,6,0.D0,DSDE)
        DO 200 I = 1, NDT
        DO 200 J = 1, NDT
           DSDE(I,J) = HOOK(I,J) - HK(I)*T1(J)/TROIS - HGD(I)*T2(J)
  200   CONTINUE
C ======================================================================
        CALL JEDEMA ()
C ======================================================================
        END

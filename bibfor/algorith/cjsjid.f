        SUBROUTINE CJSJID( MOD, MATER, EPSD, DEPS,
     &                     YD, YF, GD, R, SIGNE, DRDY)
        IMPLICIT NONE
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/10/2009   AUTEUR FERNANDES R.FERNANDES 
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
C TOLE CRP_20
C     ------------------------------------------------------------------
C     INTEGRATION PLASTIQUE (MECANISME DEVIATOIRE SEUL) DE LA LOI CJS
C
C     RESOLUTION PAR METHODE DE NEWTON       DRDY(DYI) DDYI = - R(DYI)
C
C     CALCUL DU SECOND MEMBRE : - R(DYI)
C     CALCUL DU JACOBIEN      : DRDY(DYI)
C        Y =  ( SIG    , QISO   , R      , X      , LAMBI  , LAMBD  )
C        R = -( LE     , LQ     , LR     , LX     , FI     , FD     )
C     DRDY =  ( DLEDS  , DLEDQ  , DLEDR  , DLEDX  , DLEDLI , DLEDLD )
C             ( DLQDS  , DLQDQ  , DLQDR  , DLQDX  , DLQDLI , DLQDLD )
C             ( DLRDS  , DLRDQ  , DLRDR  , DLRDX  , DLRDLI , DLRDLD )
C             ( DLXDS  , DLXDQ  , DLXDR  , DLXDX  , DLXDLI , DLXDLD )
C             ( DFIDS  , DFIDQ  , DFIDR  , DFIDX  , DFIDLI , DFIDLD )
C             ( DFDDS  , DFDDQ  , DFDDR  , DFDDX  , DFDDLI , DFDDLD )
C ======================================================================
C     IN   MOD      :  MODELISATION
C          MATER    :  COEFFICIENTS MATERIAU A T+DT
C          EPSD     :  DEFORMATION A T
C          DEPS     :  INCREMENT DE DEFORMATION
C          YD       :  VARIABLES A T = (SIGD, VIND, LAMBD)
C          YF       :  VARIABLES A T+DT = (SIGF, VINF, LAMBF)
C     VAR  GD       :  TENSEUR DE LA LOI D ECOULEMENT PLASTIQUE DEV.
C     OUT  R        :  SECOND MEMBRE
C          SIGNE    :  SIGNE DE S:DEPSDP
C          DRDY     :  JACOBIEN
C ======================================================================
        INTEGER       NDT, NDI, NMOD, I, J, K, CODRET
        PARAMETER     (NMOD = 16)
        REAL*8        EPSD(6), DEPS(6), DEPSE(6), DEPSDP(6)
        REAL*8        DSIGNL(6), DSIGL(6), SIGF(6)
        REAL*8        YD(NMOD), YF(NMOD), R(NMOD), DRDY(NMOD,NMOD)
        REAL*8        MATER(14,2), N, RM, RC, A, B, C, PCO, PA, PC
        REAL*8        KOP, KP, BETA, BETAPR, GAMMA, MUCJS, DHDS(6)
        REAL*8        LE(6), LQ, LR, LX(6), FI, FD, TRACE, TS(6)
        REAL*8        HOOKNL(6,6), HOOK(6,6), KOOH(6,6), DHDQ(6)
C        REAL*8       KOOHNL(6,6)
        REAL*8        I1F, E, NU, AL, LA, MU, UNPNUE, UNSURE, MNUSE
        REAL*8        QISO, GQISO
        REAL*8        RF, GR, DGRDR, DGRDS(6)
        REAL*8        XF(6), XII, GX(6), DGXDX(6,6), DGXDS(6,6)
        REAL*8        EPSV, PHI, PHIO, RR, COSA, COSDIF
        REAL*8        TRUC, SIGNE, DLAMBI, DLAMBD
        REAL*8        S(6), SII, SIIC, HTS, DETS, COS3TS
        REAL*8        SIIREL, QIIREL, QUATRE
        REAL*8        Q(6), QII,  HTQ, DETQ, COS3TQ
        REAL*8        TANGS, TANGQ, TETAS, TETAQ
        REAL*8        QQ(6), QQII, NORM(6), VECTAN(6), GD(6), GDD(6)
        REAL*8        COEF0, COEF1, COEF3, COEF4, COEF5, COEF6
        REAL*8        COEF7, COEF8, COEF9, COEF10, COEF11, COEF12
        REAL*8        COEF13, COEF14, COEF15, COEF16, COEF17, COEF18
        REAL*8        COEF19, COEF20, COEF21, COEF22
        REAL*8        DGDDS(6,6), DGDDR(6), DGDDX(6,6),DGQDQ
        REAL*8        DLEDS(6,6), DLEDQ(6), DLEDR(6), DLEDX(6,6)
        REAL*8        DLEDLI(6),DLEDLD(6)
        REAL*8        DLQDS(6), DLQDQ, DLQDR, DLQDX(6), DLQDLI, DLQDLD
        REAL*8        DLRDS(6), DLRDQ, DLRDR, DLRDX(6), DLRDLI, DLRDLD
        REAL*8        DLXDS(6,6), DLXDQ(6), DLXDR(6), DLXDX(6,6)
        REAL*8        DLXDLI(6), DLXDLD(6)
        REAL*8        DFIDS(6), DFIDQ, DFIDR, DFIDX(6), DFIDLI, DFIDLD
        REAL*8        DFDDS(6), DFDDQ, DFDDR, DFDDX(6), DFDDLI, DFDDLD
        REAL*8        DVDS(6,6)
        REAL*8        DSDS(6,6), DSSDS(6), DS2DS(6), DS2CDS(6)
        REAL*8        DQQDS(6,6), DQIIDS(6), DQQ2DS(6), DQQDX(6,6)
        REAL*8        DCADS(6), DCFDS(6), DRRDS(6), DPHODS(6), DPHIDS(6)
        REAL*8        DHTSDS(6), DHTQDQ(6), DHTQDS(6)
        REAL*8        T(6), TD(6), DTDDQ(6,6)
        REAL*8        DQQDQ(6,6), DQDS(6,6)
        REAL*8        D2FDSQ(6,6), D2FDS2(6,6), D2FDSX(6,6)
        REAL*8        TERME1, TERME2, TERME3, TERME4, TERME5
        REAL*8        PROD0, PROD1, PROD2, PROD3, PROD4, PROD5, PROD6
        REAL*8        MUN5, ZERO, UN, D12, DEUX, TROIS, CINQ, SIX
        REAL*8        EPSSIG,PREF,QINIT
        REAL*8        KRON(6), IDEN6(6,6)
        CHARACTER*8   MOD
C ======================================================================
        PARAMETER     ( MUN5   =-1.5D0  )
        PARAMETER     ( D12    = 0.5D0  )
        PARAMETER     ( UN     = 1.D0   )
        PARAMETER     ( ZERO   = 0.D0   )
        PARAMETER     ( DEUX   = 2.D0   )
        PARAMETER     ( TROIS  = 3.D0   )
        PARAMETER     ( QUATRE = 4.D0   )
        PARAMETER     ( CINQ   = 5.D0   )
        PARAMETER     ( SIX    = 6.D0   )
        PARAMETER     ( EPSSIG = 1.D-8   )
C ======================================================================
        COMMON /TDIM/   NDT, NDI
C ======================================================================
        DATA    IDEN6   /UN     , ZERO  , ZERO  , ZERO  ,ZERO  ,ZERO,
     &                   ZERO   , UN    , ZERO  , ZERO  ,ZERO  ,ZERO,
     &                   ZERO   , ZERO  , UN    , ZERO  ,ZERO  ,ZERO,
     &                   ZERO   , ZERO  , ZERO  , UN    ,ZERO  ,ZERO,
     &                   ZERO   , ZERO  , ZERO  , ZERO  ,UN    ,ZERO,
     &                   ZERO   , ZERO  , ZERO  , ZERO  ,ZERO  ,UN/
        DATA          KRON /UN , UN , UN , ZERO ,ZERO ,ZERO/
C ======================================================================
C --- ATTENTION : NE PAS CONFONDRE LA VARIABLE INTERNE R ---------------
C --- --------- : AVEC LE SYSTEME NON LINEAIRE GLOBAL NOTE AUSSI R -----
C ======================================================================
        CALL JEMARQ ()
C ======================================================================
C --- PROPRIETES CJS MATERIAU ------------------------------------------
C ======================================================================
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
        QINIT = MATER(13,2)
C ======================================================================
C --- PREMIER INVARIANT ET AUTRES GRANDEURS UTILES ---------------------
C ======================================================================
        I1F = TRACE(NDI,YF)
        IF((I1F+QINIT)  .EQ. 0.D0 ) THEN
          I1F  = -QINIT+1.D-12 * PA
          PREF = ABS(PA)
        ELSE
          PREF = ABS(I1F+QINIT)
        ENDIF
        QISO = YF(NDT+1)
        RF = YF(NDT+2)
        DO 20 I=1,NDT
        XF(I)= YF(NDT+2+I)
        GDD(I) = GD(I)
 20     CONTINUE
        DLAMBI = YF(2*NDT+3) - YD(2*NDT+3)
        DLAMBD = YF(2*NDT+4) - YD(2*NDT+4)
C ======================================================================
        DO 25 I=1, NDT
        SIGF(I) = YF(I)
 25     CONTINUE
        KP = KOP * ( QISO/PA )**N
C ======================================================================
C --- OPERATEURS DE RIGIDITE ET DE SOUPLESSE (LINEAIRES OU NON LINEA.) -
C ======================================================================
C --- OPERATEURS LINEAIRES ---------------------------------------------
C ======================================================================
        CALL LCINMA(ZERO,HOOK)
        CALL LCINMA(ZERO,KOOH)
        E  = MATER(1,1)
        NU = MATER(2,1)
        AL = E  * (UN-NU) / (UN+NU) / (UN-DEUX*NU)
        LA = NU * E       / (UN+NU) / (UN-DEUX*NU)
        MU = E  * D12     / (UN+NU)
        UNPNUE = ( UN + NU ) / E
        UNSURE =   UN / E
        MNUSE  = - NU / E
C ======================================================================
C --- 3D/DP/AX ---------------------------------------------------------
C ======================================================================
        IF ( MOD(1:2) .EQ. '3D'     .OR.
     &       MOD(1:6) .EQ. 'D_PLAN' .OR.
     &       MOD(1:4) .EQ. 'AXIS'        )THEN
             DO 30 I = 1,NDI
             DO 30 J = 1,NDI
                 IF(I.EQ.J) THEN
                    HOOK(I,J) = AL
                    KOOH(I,J) = UNSURE
                 ENDIF
                 IF(I.NE.J) THEN
                    HOOK(I,J) = LA
                    KOOH(I,J) = MNUSE
                 ENDIF
 30          CONTINUE
             DO 35 I = NDI+1 , NDT
             DO 35 J = NDI+1 , NDT
                 IF(I.EQ.J) THEN
                    HOOK(I,J) = DEUX* MU
                    KOOH(I,J) = UNPNUE
                 ENDIF
 35          CONTINUE
C ======================================================================
C --- CP/1D ------------------------------------------------------------
C ======================================================================
        ELSE IF ( MOD(1:6) .EQ. 'C_PLAN' .OR.
     &            MOD(1:2) .EQ. '1D' )THEN
             CALL U2MESS('F','ALGORITH2_15')
        ENDIF
C ======================================================================
C --- OPERATEURS NON LINEAIRE ------------------------------------------
C ======================================================================
        COEF0 = ((I1F+QINIT)/TROIS/PA)**N
        DO 40 I=1, NDT
        DO 40 J=1, NDT
           HOOKNL(I,J) = COEF0*HOOK(I,J)
C           KOOHNL(I,J) = COEF0*KOOH(I,J)
 40     CONTINUE
C ======================================================================
C --- LOIS D ECROUISSAGE : GQISO, GR ET GX -----------------------------
C ======================================================================
C --- ECROUISSAGE ISOTROPE DU MECANISME ISOTROPE -----------------------
C ======================================================================
        GQISO = - KP
C ======================================================================
C --- ECROUISSAGE ISOTROPE DU MECANISME DEVIATOIRE ---------------------
C ======================================================================
        COEF1 = ((I1F+QINIT)/TROIS/PA)**MUN5
        GR    = - A * (UN-RF/RM)**DEUX * (I1F+QINIT) * COEF1
C ======================================================================
C --- ECROUISSAGE CINEMATIQUE DU MECANISME DEVIATOIRE ------------------
C ======================================================================
        CALL CJSQCO( GAMMA, SIGF, XF, PREF, EPSSIG, I1F,
     &               S, SII, SIIREL, COS3TS, HTS, DETS,
     &               Q, QII, QIIREL, COS3TQ, HTQ, DETQ )

        CALL CALCQ(Q,GAMMA,PREF,EPSSIG,QQ,CODRET)
        CALL LCPRSC(QQ,QQ,TRUC)
        QQII = SQRT(TRUC)
        CALL LCPRSC(XF,XF,TRUC)
        XII = SQRT(TRUC)
        EPSV = ZERO
        DO 70 I = 1,NDI
           EPSV = EPSV + EPSD(I)+ DEPS(I)
  70    CONTINUE

        PC = PCO*EXP(-C*EPSV)

        IF(XII .LE. EPSSIG ) THEN
           PHI = UN
        ELSE IF(SIIREL .LE. EPSSIG ) THEN
              COSA = UN
              COSDIF = UN
              RR = RC + MUCJS*MAX(ZERO,LOG(TROIS*PC/(I1F+QINIT)))
              PHIO = COSA/( RR - HTS/HTQ*RM*COSDIF)
              PHI = PHIO * HTS * QQII
        ELSE
              COSA =  ( QII*QII - SII*SII - I1F*I1F*XII*XII ) /
     &                 (DEUX*SII*I1F*XII)

              TANGS = SQRT(UN-COS3TS*COS3TS) / COS3TS
              TANGQ = SQRT(UN-COS3TQ*COS3TQ) / COS3TQ
              TETAS = ATAN2(TANGS,1.D0) / TROIS
              TETAQ = ATAN2(TANGQ,1.D0) / TROIS
              COSDIF = COS(TETAS-TETAQ)

              RR = RC + MUCJS*MAX(ZERO,LOG(TROIS*PC/(I1F+QINIT)))
              PHIO = COSA/( RR - HTS/HTQ*RM*COSDIF)
              PHI = PHIO * HTS * QQII
        ENDIF
C ======================================================================
        DO 80 I=1, NDT
           GX(I) = (I1F+QINIT)/B*( QQ(I) + PHI*XF(I) ) * COEF1
 80     CONTINUE
C ======================================================================
C --- LOIS D ECOULEMENTS : ---------------------------------------------
C ======================================================================
C --- LOI D'ECOULEMENT DU MECANISME ISOTROPE ---------------------------
C --- ON NE LE CALCUL PAS CAR CA VAUT SIMPLEMENT -1/3 KRON(I) ----------
C ======================================================================
C --- LOI D'ECOULEMENT DU MECANISME DEVIATOIRE : GD --------------------
C ======================================================================
        CALL LCPRSC(QQ,XF,TRUC)
        TRUC = TRUC - RF
        DO 90 I=1, NDT
           DFDDS(I) = QQ(I) - TRUC*KRON(I)
 90     CONTINUE
C ======================================================================
C --- CALCUL DE L INCREMENT DE DEFORMATION PLASTIQUE DEV. EN -----------
C --- UTILISANT LE TENSEUR GD DE L ITERATION DE NEWTON PRECEDENTE ------
C --- ET LA VALEUR DE DLAMBD -------------------------------------------
C ======================================================================
        DO 100 I=1,NDT
           DEPSDP(I) = DLAMBD*GDD(I)
 100    CONTINUE
C ======================================================================
        CALL LCPRSC(S,DEPSDP,TRUC)
        IF(TRUC .GE. ZERO) THEN
          SIGNE = UN
        ELSE
          SIGNE = - UN
        ENDIF
        SIIC = -RC * (I1F+QINIT) / HTS
        BETAPR = BETA * (SII/SIIC - UN) * SIGNE
        COEF4 = BETAPR / SII
        COEF5 = UN / SQRT( BETAPR*BETAPR + TROIS )
        DO 120 I = 1,NDT
           VECTAN(I) = COEF4 *  S(I) +  KRON(I)
           NORM(I)   = COEF5 * VECTAN(I)
 120    CONTINUE
C ======================================================================
        CALL LCPRSC(DFDDS,NORM,PROD0)
        DO 130 I = 1,NDT
           GD(I) = DFDDS(I) - PROD0 * NORM(I)
 130    CONTINUE
C ======================================================================
C --- CALCULS PRELIMIAIRES DE DERIVEES ---------------------------------
C ======================================================================
C --- DERIVEE DE S PAR RAPPORT A SIG : DSDS ----------------------------
C ======================================================================
        DO 150 I=1, NDT
        DO 150 J=1, NDT
           DSDS(I,J) = IDEN6(I,J) - KRON(I) * KRON(J) / TROIS
 150    CONTINUE
C ======================================================================
C --- DERIVEE DE Q PAR RAPPORT A SIG : DQDS ----------------------------
C ======================================================================
        DO 160 I=1, NDT
        DO 165 J=1, NDT
           DQDS(I,J) = IDEN6(I,J) - KRON(J) * ( KRON(I)/TROIS  + XF(I) )
 165    CONTINUE
 160    CONTINUE
C ======================================================================
C --- EXPRESSION DE TS, T, TD ET DE SA DERIVEE PAR RAPPORT A Q, DTDDQ --
C ======================================================================
        CALL CJST   ( S,   TS       )
        CALL CJST   ( Q,   T        )
        CALL LCDEVI ( T,   TD       )
        CALL CJSDTD ( MOD, Q, DTDDQ )
C ======================================================================
C --- DERIVEE DE HTQ PAR RAPPORT A Q : DHDQ ----------------------------
C --- DERIVEE DE HTS PAR RAPPORT A S : DHDS ----------------------------
C ======================================================================
        COEF6  = SQRT(TROIS/DEUX)*GAMMA/HTQ**CINQ/QII**TROIS
        COEF7  = - GAMMA*COS3TQ/(DEUX*HTQ**CINQ*QII**DEUX)
        COEF8  = SQRT(TROIS/DEUX)*GAMMA/HTS**CINQ/SII**TROIS
        COEF9  = - GAMMA*COS3TS/(DEUX*HTS**CINQ*SII**DEUX)
        DO 168 I = 1, NDT
          DHDQ(I)   = COEF6*T(I)  + COEF7*Q(I)
          DHDS(I)   = COEF8*TS(I) + COEF9*S(I)
 168    CONTINUE
C ======================================================================
C --- DERIVEE DE QQ PAR RAPPORT A Q : DQQDQ ----------------------------
C ======================================================================
        COEF6 = UN + GAMMA/DEUX*COS3TQ
        COEF7 = SQRT(54.0D0)*GAMMA/(SIX*QII*QII)
        COEF8 = SQRT(54.0D0)*GAMMA/DEUX/QII**QUATRE/HTQ**CINQ
        COEF9 = SQRT(54.0D0)*GAMMA/SIX/QII**DEUX/HTQ**CINQ
        DO 170 I=1, NDT
        DO 170 J=1, NDT
        DQQDQ(I,J)= -CINQ/HTQ**SIX*(COEF6*Q(I)/QII+COEF7*TD(I))*DHDQ(J)
     &          + COEF6/HTQ**CINQ*(IDEN6(I,J)/QII-Q(I)*Q(J)/QII**TROIS)
     &             + COEF8*Q(I)*(T(J)-3*Q(J)*DETQ/QII**DEUX)
     &             + COEF9*(DTDDQ(I,J)-DEUX*TD(I)*Q(J)/QII**DEUX)
 170    CONTINUE
C ======================================================================
C --- DERIVEE DE QQ PAR RAPPORT A SIG : DQQDS --------------------------
C ======================================================================
        DO 180 I=1, NDT
           DO 190 J=1, NDT
              DQQDS(I,J) = ZERO
              DO 200 K=1, NDT
                 DQQDS(I,J) = DQQDS(I,J) + DQQDQ(I,K)*DQDS(K,J)
 200       CONTINUE
 190       CONTINUE
 180    CONTINUE
C ======================================================================
C --- DERIVEE DE DFDDS PAR RAPPORT A Q : D2FDSQ ------------------------
C ======================================================================
        DO 210 I=1, NDT
           DO 220 J=1, NDT
              PROD1 = ZERO
              DO 230 K=1, NDT
                 PROD1 = PROD1 + DQQDQ(K,J) * XF(K)
 230          CONTINUE
              D2FDSQ(I,J) = DQQDQ(I,J) - PROD1 * KRON(I)
 220       CONTINUE
 210    CONTINUE
C ======================================================================
C --- DERIVEE DE DFDDS PAR RAPPORT A SIG : D2FDS2 ----------------------
C ======================================================================
        DO 240 I=1, NDT
           DO 250 J=1, NDT
              D2FDS2(I,J) = ZERO
              DO 260 K=1, NDT
                 D2FDS2(I,J) = D2FDS2(I,J) + D2FDSQ(I,K) * DQDS(K,J)
 260          CONTINUE
 250       CONTINUE
 240    CONTINUE
C ======================================================================
C --- DERIVEE DE SII PAR RAPPORT A SIG : DS2DS -------------------------
C ======================================================================
        DO 270 I=1, NDT
           DS2DS(I) = ZERO
           DO 280 J=1, NDT
              DS2DS(I) = DS2DS(I) + S(J) * DSDS(I,J)
 280       CONTINUE
           DS2DS(I) = DS2DS(I) / SII
 270    CONTINUE
C ======================================================================
C --- DERIVEE DE SIIC PAR RAPPORT A SIG : DS2CDS -----------------------
C ======================================================================
        COEF9  = - RC / HTS
        COEF10 = RC * (I1F+QINIT) / HTS**DEUX
        DO 290 I=1, NDT
           PROD2 = ZERO
           DO 300 J=1, NDT
              PROD2 = PROD2 + DHDS(J) * DSDS(J,I)
 300       CONTINUE
           DS2CDS(I) = COEF9 * KRON(I) + COEF10 * PROD2
 290    CONTINUE
C ======================================================================
C --- DERIVEE DU RAPPORT (SII/SIIC) PAR RAPPORT A SIG : DSSDS ----------
C ======================================================================
        DO 310 I=1, NDT
           DSSDS(I) = DS2DS(I) / SIIC - DS2CDS(I) * SII / SIIC / SIIC
 310    CONTINUE
C ======================================================================
C --- DERIVEE DU VECTEUR TANGENT A LA SURFACE POTENTIELLE, VECTAN, -----
C --- PAR RAPPORT A SIG : DVDS -----------------------------------------
C ======================================================================
        DO 320 I=1, NDT
        DO 320 J=1, NDT
           DVDS(I,J) = BETAPR / SII * DSDS(I,J)
     &        + SIGNE*BETA*S(I)*(DS2DS(J)/SII/SII - DS2CDS(J)/SIIC/SIIC)
 320    CONTINUE
C ======================================================================
C --- DERIVEE DE QII PAR RAPPORT A SIG : DQIIDS ------------------------
C ======================================================================
        DO 330 I=1, NDT
           DQIIDS(I) = ZERO
           DO 340 J=1, NDT
              DQIIDS(I) = DQIIDS(I) + Q(J)/QII*DQDS(J,I)
 340       CONTINUE
 330    CONTINUE
C ======================================================================
C --- DERIVEE DE QQII PAR RAPPORT A SIG : DQQ2DS -----------------------
C ======================================================================
        DO 350 I=1, NDT
           DQQ2DS(I) = ZERO
           DO 360 J=1, NDT
              DQQ2DS(I) = DQQ2DS(I) + QQ(J)/QQII*DQQDS(J,I)
 360       CONTINUE
 350    CONTINUE
C ======================================================================
C --- DERIVEE DE HTQ PAR RAPPORT A SIG : DHTQDS ------------------------
C --- DERIVEE DE HTS PAR RAPPORT A SIG : DHTSDS ------------------------
C ======================================================================
        DO 390 I=1, NDT
           DHTQDS(I) = ZERO
           DHTSDS(I) = ZERO
           DO 400 J=1, NDT
              DHTQDS(I) = DHTQDS(I) + DHDQ(J) * DQDS(J,I)
              DHTSDS(I) = DHTSDS(I) + DHDS(J) * DSDS(J,I)
 400       CONTINUE
 390    CONTINUE
C ======================================================================
C --- DERIVEE DE PHI PAR RAPPORT A SIG : DPHIDS ------------------------
C --- EN FONCTION DES DIFFERENTES VALEURS DE SII ET XII ----------------
C ======================================================================
C --- INITIALISATION : -------------------------------------------------
C ======================================================================
        DO 410 I=1, NDT
           DPHIDS(I) = ZERO
 410    CONTINUE
C ======================================================================
C --- 1ER CAS : XII = 0  ON NE FAIT RIEN -------------------------------
C ======================================================================
        IF (XII .GT. EPSSIG) THEN
C ======================================================================
C --- 2EME CAS : SII = 0 -----------------------------------------------
C ======================================================================
        IF(SIIREL .LT. EPSSIG) THEN
C ======================================================================
C --- DERIVEE DE RR PAR RAPPORT A SIG : DRRDS --------------------------
C ======================================================================
        DO 420 I=1, NDT
           DRRDS(I) = - MUCJS/(I1F+QINIT) * KRON(I)
 420    CONTINUE
C ======================================================================
C --- DERIVEE DE PHIO PAR RAPPORT A SIG : DPHODS -----------------------
C ======================================================================
        DO 430 I=1, NDT
           DPHODS(I) = - COSA/( RR - HTS/HTQ*RM*COSDIF )**DEUX *
     &                   ( DRRDS(I) - RM*COSDIF/HTQ*DHTSDS(I)
     &                               + HTS/HTQ/HTQ*RM*COSDIF*DHTQDS(I) )
 430    CONTINUE
C ======================================================================
C --- DERIVEE DE PHI PAR RAPPORT A SIG : DPHIDS ------------------------
C ======================================================================
        DO 440 I=1, NDT
           DPHIDS(I) = HTS*QQII*DPHODS(I) + PHIO*QQII*DHTSDS(I)
     &                                              + PHIO*HTS*DQQ2DS(I)
 440    CONTINUE
C ======================================================================
C --- 3EME CAS : SII ET XII NON NULS -----------------------------------
C ======================================================================
        ELSE
C ======================================================================
C --- DERIVEE DE COSA PAR RAPPORT A SIG : DCADS ------------------------
C ======================================================================
        DO 450 I=1, NDT
           DCADS(I) = ( QII*DQIIDS(I)-I1F*XII*XII*KRON(I)-SII*DS2DS(I) )
     &                                                      /SII/I1F/XII
     &           - D12*( QII*QII - SII*SII - I1F*I1F*XII*XII )
     &           /(SII*I1F*XII)**DEUX*(I1F*XII*DS2DS(I)+SII*XII*KRON(I))
 450    CONTINUE
C ======================================================================
C --- DERIVEE DE COSDIF PAR RAPPORT A SIG : DCFDS ----------------------
C ======================================================================
          COEF17 = SQRT(54.0D0)/SII**TROIS
          COEF18 = SQRT(54.0D0)/QII**TROIS
          COEF19 = TROIS*DETS/SII**DEUX
          COEF20 = TROIS*DETQ/QII**DEUX
          COEF21 = SQRT(UN - COS3TS*COS3TS)
          COEF22 = SQRT(UN - COS3TQ*COS3TQ)
          DO 460 I=1, NDT
             DCFDS(I) = SIN(TETAS-TETAQ)/TROIS *
     &                ( COEF21 * ( COEF17 * ( TS(I) - COEF19 * S(I) ) )-
     &                  COEF22 * ( COEF18 * ( T(I)  - COEF20 * Q(I) ) ))
 460      CONTINUE
C ======================================================================
C --- DERIVEE DE RR PAR RAPPORT A SIG : DRRDS --------------------------
C ======================================================================
        DO 470 I=1, NDT
           DRRDS(I) = - MUCJS/(I1F+QINIT) * KRON(I)
 470    CONTINUE
C ======================================================================
C --- DERIVEE DE PHIO PAR RAPPORT A SIG : DPHODS -----------------------
C ======================================================================
        DO 480 I=1, NDT
           DPHODS(I) = DCADS(I) / ( RR - HTS/HTQ*RM*COSDIF )
     &             - COSA/( RR - HTS/HTQ*RM*COSDIF )**DEUX *
     &              ( DRRDS(I) - RM*COSDIF/HTQ*DHTSDS(I)
     &              + HTS/HTQ/HTQ*RM*COSDIF*DHTQDS(I)
     &              - HTS/HTQ*RM*DCFDS(I) )
 480    CONTINUE
C ======================================================================
C --- DERIVEE DE PHI PAR RAPPORT A SIG : DPHIDS ------------------------
C ======================================================================
        DO 490 I=1, NDT
           DPHIDS(I) = HTS*QQII*DPHODS(I) + PHIO*QQII*DHTSDS(I)
     &                                              + PHIO*HTS*DQQ2DS(I)
 490    CONTINUE
        ENDIF
        ENDIF
C ======================================================================
C --- DERIVEE DE QQ PAR RAPPORT A X : DQQDX ----------------------------
C ======================================================================
        DO 500 I=1, NDT
           DO 510 J=1, NDT
              DQQDX(I,J) = ZERO
              DO 520 K=1, NDT
                 DQQDX(I,J) = DQQDX(I,J) - I1F * DQQDQ(I,K) * IDEN6(K,J)
 520          CONTINUE
 510       CONTINUE
 500    CONTINUE
C ======================================================================
C --- DERIVEE DE DFDDS PAR RAPPORT A X : D2FDSX ------------------------
C ======================================================================
        DO 530 I=1, NDT
           DO 540 J=1, NDT
              PROD3 = ZERO
              DO 550 K=1, NDT
                 PROD3 = PROD3 + DQQDX(K,J) * XF(K) + QQ(K) * IDEN6(K,J)
 550          CONTINUE
              D2FDSX(I,J) = DQQDX(I,J) - PROD3 * KRON(I)
 540       CONTINUE
 530    CONTINUE
C ======================================================================
C --- DERIVEES DES LOIS D ECROUISSAGE GQISO, GR ET GX ------------------
C --- PAR RAPPORT A QISO, R, X ET SIG ----------------------------------
C --- ET DERIVEES DE LA LOI D ECOULEMENT GD ----------------------------
C ======================================================================
C --- DERIVEES DE LA LOI D ECROUISSAGE ISOTROPE DU MECANISME ISOTROPE --
C ======================================================================
        DGQDQ = - N * KOP / PA * ( PA/QISO )**(UN-N)
C ======================================================================
C --- DERIVEES DE LA LOI D ECROUISSAGE ISOTROPE DU MECANISME DEVIATOIRE-
C ======================================================================
        DGRDR = DEUX * A / RM * (UN-RF/RM) * (I1F+QINIT) * COEF1
        DO 600 I=1,NDT
           DGRDS(I) = A / DEUX * (UN-RF/RM)**DEUX * COEF1 * KRON(I)
 600    CONTINUE
C ======================================================================
C --- DERIVEES DE LA LOI D ECROUISSAGE CINEMATIQUE ---------------------
C --- DU MECANISME DEVIATOIRE ------------------------------------------
C ======================================================================
        DO 620 I=1, NDT
        DO 630 J=1, NDT
        DGXDS(I,J) =  - COEF1/DEUX/B * ( QQ(I) + PHI*XF(I) ) * KRON(J)
     &     + (I1F+QINIT)*COEF1/B * ( DQQDS(I,J) + XF(I)* DPHIDS(J) )
        DGXDX(I,J) =
     &    (I1F+QINIT)*COEF1/B * ( DQQDX(I,J) + PHI*IDEN6(I,J) )
 630    CONTINUE
 620    CONTINUE
C ======================================================================
C --- DERIVEES DE LA LOI D ECOULEMENT DU MECANISME DEVIATOIRE ----------
C ======================================================================
        COEF13 = COEF5 * COEF5 * DEUX * BETA * BETA * (SII/SIIC - UN)
        DO 650 I=1, NDT
           DO 660 J=1, NDT
              PROD4 = ZERO
              PROD5 = ZERO
              PROD6 = ZERO
              DO 670 K=1, NDT
                 PROD4 = PROD4 + D2FDS2(K,J) * NORM(K)
                 PROD5 = PROD5 + DFDDS(K) * DVDS(K,J)
                 PROD6 = PROD6 + D2FDSX(K,J) * NORM(K)
 670          CONTINUE
              DGDDS(I,J) = D2FDS2(I,J) - PROD4 * NORM(I)
     &                                       - COEF5 * PROD5 * NORM(I)
     &                                     - COEF5 * PROD0 * DVDS(I,J)
     &                           + COEF13 * PROD0 * NORM(I) * DSSDS(J)
              DGDDX(I,J) = D2FDSX(I,J) - PROD6 * NORM(I)
 660       CONTINUE
 650    CONTINUE
C ======================================================================
        COEF14 = BETAPR * BETAPR / ( BETAPR * BETAPR + TROIS )
        COEF15 = - TROIS * BETAPR / SII / ( BETAPR * BETAPR + TROIS )
        DO 690 I=1, NDT
           DGDDR(I) = COEF14*KRON(I) + COEF15 * S(I)
 690    CONTINUE
C ======================================================================
C --- LOI D ETAT : LE --------------------------------------------------
C --- ET SES DERIVEES : DLEDS, DLEDQ, DLEDR, DLEDX, DLEDLI, DLEDLD -----
C ======================================================================
C --- LOI D ETAT -------------------------------------------------------
C ======================================================================
        DO 700 I=1,NDT
           DEPSE(I) = DEPS(I) + DLAMBI/TROIS*KRON(I) - DLAMBD*GD(I)
 700    CONTINUE
        CALL LCPRMV ( HOOKNL, DEPSE, DSIGNL )
        DO 710 I=1,NDT
           LE(I) = YF(I) - YD(I) - DSIGNL(I)
 710    CONTINUE
C ======================================================================
C --- DERIVEE DE LA LOI D ETAT -----------------------------------------
C ======================================================================
        COEF16 = N/TROIS/PA * (TROIS*PA/(I1F+QINIT))**(UN-N)
        CALL LCPRMV ( HOOK, DEPSE, DSIGL )
        CALL LCINMA(ZERO,DLEDS)
        DO 720 I=1,NDT
           DO 730 J=1, NDT
              TERME1=ZERO
              TERME2=ZERO
              DO 740 K=1, NDT
                 TERME1 = TERME1 + HOOKNL(I,K)*DGDDS(K,J)
                 TERME2 = TERME2 + HOOKNL(I,K)*DGDDX(K,J)
 740          CONTINUE
              DLEDS(I,J) = IDEN6(I,J) - COEF16 * DSIGL(I) * KRON(J)
     &                     + DLAMBD * TERME1

              DLEDX(I,J) = DLAMBD * TERME2
 730       CONTINUE
C ======================================================================
           DLEDQ(I) = ZERO
           TERME3   = ZERO
           TERME4   = ZERO
           TERME5   = ZERO
           DO 750 K=1, NDT
              TERME3 = TERME3 + HOOKNL(I,K)*DGDDR(K)
              TERME4 = TERME4 - HOOKNL(I,K)*KRON(K)/TROIS
              TERME5 = TERME5 + HOOKNL(I,K)*GD(K)
 750       CONTINUE
           DLEDR(I)  = DLAMBD * TERME3
           DLEDLI(I) = TERME4
           DLEDLD(I) = TERME5
 720    CONTINUE
C ======================================================================
C --- LOI D ECROUISSAGE DE QISO : LQ -----------------------------------
C --- ET SES DERIVEES : DLQDS, DLQDQ, DLQDR, DLQDX, DLQDLI, DLQDLD -----
C ======================================================================
C --- LOI D ECROUISSAGE DE QISO ----------------------------------------
C ======================================================================
        LQ = QISO - YD(NDT+1) - DLAMBI*GQISO
C ======================================================================
C --- DERIVEE DE LA LOI D ECROUISSAGE DE QISO --------------------------
C ======================================================================
        DO 755 I=1, NDT
           DLQDS(I) = ZERO
           DLQDX(I) = ZERO
 755    CONTINUE
        DLQDQ  = UN - DLAMBI*DGQDQ
        DLQDR  = ZERO
        DLQDLI = - GQISO
        DLQDLD = ZERO
C ======================================================================
C --- LOI D ECROUISSAGE DE R : LR --------------------------------------
C --- ET SES DERIVEES : DLRDS, DLRDQ, DLRDR, DLRDX, DLRDLI, DLRDLD -----
C ======================================================================
C --- LOI D ECROUISSAGE DE R -------------------------------------------
C ======================================================================
        LR = RF - YD(NDT+2) - DLAMBD*GR
C ======================================================================
C --- DERIVEE DE LA LOI D ECROUISSAGE DE R -----------------------------
C ======================================================================
        DO 760 I=1, NDT
           DLRDS(I)= - DLAMBD * DGRDS(I)
           DLRDX(I)=   ZERO
 760    CONTINUE
        DLRDQ  = ZERO
        DLRDR  = UN - DLAMBD * DGRDR
        DLRDLI = ZERO
        DLRDLD = - GR
C ======================================================================
C --- LOI D ECROUISSAGE DE X : LX --------------------------------------
C --- ET SES DERIVEES : DLXDS, DLXDQ, DLXDR, DLXDX, DLXDLI, DLXDLD -----
C ======================================================================
C --- LOI D ECROUISSAGE DE X -------------------------------------------
C ======================================================================
        DO 770 I=1, NDT
           LX(I) = XF(I) - YD(NDT+2+I) - DLAMBD*GX(I)
 770    CONTINUE
C ======================================================================
C --- DERIVEE DE LA LOI D ECROUISSAGE DE X -----------------------------
C ======================================================================
        DO 780 I=1, NDT
           DO 790 J=1, NDT
              DLXDS(I,J)= - DLAMBD * DGXDS(I,J)
              DLXDX(I,J)= IDEN6(I,J) - DLAMBD * DGXDX(I,J)
 790       CONTINUE
           DLXDQ(I)  =   ZERO
           DLXDR(I)  =   ZERO
           DLXDLI(I) =   ZERO
           DLXDLD(I) = - GX(I)
 780    CONTINUE
C ======================================================================
C --- FONCTION SEUIL ISOTROPE : FI -------------------------------------
C --- ET SES DERIVEES : DFIDS, DFIDQ, DFIDR, DFIDX, DFIDLI, DFIDLD -----
C ======================================================================
C --- SEUIL ISOTROPE ---------------------------------------------------
C ======================================================================
        FI = -(I1F+QINIT)/TROIS + QISO
C ======================================================================
C --- DERIVEES DU SEUIL ISOTROPE ---------------------------------------
C ======================================================================
        DO 800 I=1,NDT
           DFIDS(I) = - KRON(I)/TROIS
           DFIDX(I) =   ZERO
 800    CONTINUE
        DFIDQ  = UN
        DFIDR  = ZERO
        DFIDLI = ZERO
        DFIDLD = ZERO
C ======================================================================
C --- FONCTION SEUIL DEVIATOIRE : FD -----------------------------------
C --- ET SES DERIVEES : DFDDS, DFDDQ, DFDDR, DFDDX, DFDDLI, DFDDLD -----
C ======================================================================
C --- SEUIL DEVIATOIRE -------------------------------------------------
C ======================================================================
        FD = QII*HTQ + RF*(I1F+QINIT)
C ======================================================================
C --- DERIVEES DU SEUIL DEVIATOIRE : -----------------------------------
C ======================================================================
C --- DFDDS : DEJA CALCULE CI-DESSUS -----------------------------------
C ======================================================================
         DFDDQ  = ZERO
         DFDDR  = (I1F+QINIT)
         DFDDLI = ZERO
         DFDDLD = ZERO
         DO 810 I=1, NDT
            DFDDX(I) = ZERO
            DO 820 J=1, NDT
               DFDDX(I) = DFDDX(I)
     &               -(I1F+QINIT)*(QII*DHDQ(J)+HTQ*Q(J)/QII)*IDEN6(J,I)
 820        CONTINUE
 810     CONTINUE
C ======================================================================
C --- ASSEMBLAGE DE R : ------------------------------------------------
C ======================================================================
C        R = -( LE     , LQ    , LR    , LX    , FI     , FD     )
C ======================================================================
C --- ASSEMBLAGE DE DRDY
C ======================================================================
C     DRDY =  ( DLEDS  , DLEDQ  , DLEDR  , DLEDX  , DLEDLI , DLEDLD )
C             ( DLQDS  , DLQDQ  , DLQDR  , DLQDX  , DLQDLI , DLQDLD )
C             ( DLRDS  , DLRDQ  , DLRDR  , DLRDX  , DLRDLI , DLRDLD )
C             ( DLXDS  , DLXDQ  , DLXDR  , DLXDX  , DLXDLI , DLXDLD )
C             ( DFIDS  , DFIDQ  , DFIDR  , DFIDX  , DFIDLI , DFIDLD )
C             ( DFDDS  , DFDDQ  , DFDDR  , DFDDX  , DFDDLI , DFDDLD )
C ======================================================================
C --- ASSEMBLAGE DE R --------------------------------------------------
C ======================================================================
        DO 850 I=1,NDT
           R(I) = -LE(I)
           R(NDT+2+I) = - LX(I)
 850    CONTINUE
        R(NDT+1)   = - LQ
        R(NDT+2)   = - LR
        R(2*NDT+3) = - FI
        R(2*NDT+4) = - FD
C ======================================================================
C --- ASSEMBLAGE DE DRDY -----------------------------------------------
C ======================================================================
        CALL LCICMA(DLEDS,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,1)
        CALL LCICMA(DLEDQ,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,1,NDT+1)
        CALL LCICMA(DLEDR,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,1,NDT+2)
        CALL LCICMA(DLEDX,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,NDT+3)
        CALL LCICMA(DLEDLI,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,1,2*NDT+3)
        CALL LCICMA(DLEDLD,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,1,2*NDT+4)

        CALL LCICMA(DLQDS,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,1)
        DRDY(NDT+1, NDT+1) = DLQDQ
        DRDY(NDT+1, NDT+2) = DLQDR
        CALL LCICMA(DLQDX,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,NDT+3)
        DRDY(NDT+1, 2*NDT+3) = DLQDLI
        DRDY(NDT+1, 2*NDT+4) = DLQDLD

        CALL LCICMA(DLRDS,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,NDT+2,1)
        DRDY(NDT+2, NDT+1) = DLRDQ
        DRDY(NDT+2, NDT+2) = DLRDR
        CALL LCICMA(DLRDX,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,NDT+2,NDT+3)
        DRDY(NDT+2, 2*NDT+3) = DLRDLI
        DRDY(NDT+2, 2*NDT+4) = DLRDLD

        CALL LCICMA(DLXDS,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,NDT+3,1)
        CALL LCICMA(DLXDQ,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,NDT+3,NDT+1)
        CALL LCICMA(DLXDR,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,NDT+3,NDT+2)
        CALL LCICMA(DLXDX,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,NDT+3,NDT+3)
        CALL LCICMA(DLXDLI,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,NDT+3,2*NDT+3)
        CALL LCICMA(DLXDLD,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,NDT+3,2*NDT+4)

        CALL LCICMA(DFIDS,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,2*NDT+3,1)
        DRDY(2*NDT+3, NDT+1) = DFIDQ
        DRDY(2*NDT+3, NDT+2) = DFIDR
        CALL LCICMA(DFIDX,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,2*NDT+3,NDT+3)
        DRDY(2*NDT+3, 2*NDT+3) = DFIDLI
        DRDY(2*NDT+3, 2*NDT+4) = DFIDLD

        CALL LCICMA(DFDDS,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,2*NDT+4,1)
        DRDY(2*NDT+4, NDT+1) = DFDDQ
        DRDY(2*NDT+4, NDT+2) = DFDDR
        CALL LCICMA(DFDDX,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,2*NDT+4,NDT+3)
        DRDY(2*NDT+4, 2*NDT+3) = DFDDLI
        DRDY(2*NDT+4, 2*NDT+4) = DFDDLD
C ======================================================================
        CALL JEDEMA ()
C ======================================================================
        END

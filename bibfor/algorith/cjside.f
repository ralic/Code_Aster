        SUBROUTINE CJSIDE( MOD, MATER, EPSD, DEPS, YD, GD, DY)
        IMPLICIT NONE
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/10/2002   AUTEUR CIBHHBC R.FERNANDES 
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
C     ------------------------------------------------------------------
C     LOI CJS :  MECANISME DEVIATOIRE
C     CALCUL DE LA SOLUTION D ESSAI EXPLICITE DY = (DSIG, DVIN, DLAMB )
C                           AVEC               Y = ( SIG,  VIN,  LAMB )
C     PARTIR DE LA PREDICTION ELASTIQUE
C     ------------------------------------------------------------------
C     IN   MOD      :  MODELISATION
C          MATER    :  COEFFICIENTS MATERIAU A T+DT
C          EPSD     :  DEFORMATION A T+DT
C          DEPS     :  INCREMENT DE DEFORMATION
C          YD       :  VARIABLES A T = (SIGD, VIND, LAMB)
C     VAR  GD       :  TENSEUR DE LA LOI D ECOULEMENT PLASTIQUE DEV.
C     OUT  DY       :  SOLUTION D ESSAI (DSIG, DVIN, DLAMB)
C     ------------------------------------------------------------------
C     Y CONTIENT LES CONTRAINTES : SIG
C                LES VARIABLES INTERNES :  R, X
C                LE MULTIPLICATEUR PLASTIQUE :  LAMBD
C ======================================================================
        INTEGER       NDT, NDI, I, J
        REAL*8        EPSD(6), DEPS(6), DEPSE(6), TRDEPS, KRON(6)
        REAL*8        DSIG(6), SIGD(6), SIGE(6), YD(*), DY(*), DQE(6)
        REAL*8        MATER(14,2), N, RM, RC, A, B, C, PCO, PC, PA
        REAL*8        KE, KOE, BETA, BETAPR, GAMMA, MUCJS, DQ(6)
        REAL*8        HOOKNL(6,6), EPSSIG, PREF, QINIT
        REAL*8        E, NU, AL, LA, MU, I1D, I1E
        REAL*8        TRUC,   SIGNE, DLAMBD, COS3T, RCOS3T
        REAL*8        S(6), SII, SIIC, HTS, DETS, COS3TS, SIIREL
        REAL*8        SE(6), SIIE, HTSE, DETSE, CO3TSE, SIIERE
        REAL*8        Q(6), QII, HTQ, DETQ, COS3TQ, QIIREL
        REAL*8        QE(6), QIIE, HTQE, DETQE,CO3TQE,QIIERE
        REAL*8        TANGS, TANGQ, TETAS, TETAQ
        REAL*8        QQ(6), QQII, NORM(6), DFDDS(6), GD(6), TRGD
        REAL*8        COEF1, COEF3, COEF4, COEF5, COEF6, PROD1
        REAL*8        RD, XD(6), GR, GX(6), XII, DR, DX(6)
        REAL*8        EPSV, PHI, PHIO, RR, COSA, COSDIF
        REAL*8        FD, DFDDL, DRDL, DI1DL, DQIIDL, DHDL,TRACE
        REAL*8        DQDL(6), MUN5, ZERO, UN, D12, DEUX, TROIS, CINQ
        CHARACTER*8   MOD
C ======================================================================
        PARAMETER     ( MUN5 =-1.5D0  )
        PARAMETER     ( D12  = .5D0   )
        PARAMETER     ( UN   = 1.D0   )
        PARAMETER     ( ZERO = 0.D0   )
        PARAMETER     ( DEUX = 2.D0   )
        PARAMETER     ( TROIS= 3.D0   )
        PARAMETER     ( CINQ = 5.D0   )
        PARAMETER     ( EPSSIG = 1.D-8 )
C ======================================================================
        COMMON /TDIM/   NDT, NDI
        DATA            KRON /UN , UN , UN , ZERO ,ZERO ,ZERO/
C ======================================================================
        CALL JEMARQ ()
C ======================================================================
C --- PROPRIETES CJS MATERIAU ------------------------------------------
C ======================================================================
        BETA  = MATER(1,2)
        RM    = MATER(2,2)
        N     = MATER(3,2)
        RC    = MATER(5,2)
        A     = MATER(6,2)
        B     = MATER(7,2)
        C     = MATER(8,2)
        GAMMA = MATER(9,2)
        MUCJS = MATER(10,2)
        PCO   = MATER(11,2)
        PA    = MATER(12,2)
        KOE   = MATER(1,1)/TROIS/( UN-DEUX*MATER(2,1) )
        QINIT = MATER(13,2)
C ======================================================================
C --- PREMIER INVARIANT ET AUTRES GRANDEURS UTILES ---------------------
C ======================================================================
        I1D = TRACE(NDI, YD)
        IF( (I1D+QINIT) .EQ.0.0D0 ) THEN
          I1D  = -QINIT+1.D-12 * PA
          PREF = ABS(PA)
        ELSE
          PREF = ABS(I1D+QINIT)
        ENDIF

        RD = YD(NDT+1)
        DO 15 I=1,NDT
        XD(I)= YD(NDT+1+I)
 15     CONTINUE
        KE = KOE * ( (I1D+QINIT)/TROIS/PA )**N
C ======================================================================
C --- OPERATEUR DE RIGIDITE NON LINEAIRE -------------------------------
C ======================================================================
C --- OPERATEUR LINEAIRE NON LINEAIRE ----------------------------------
C ======================================================================
        CALL LCINMA(ZERO,HOOKNL)
        E = MATER(1,1) * ((I1D+QINIT)/TROIS/PA)**N
        NU = MATER(2,1)
        AL = E  * (UN-NU) / (UN+NU) / (UN-DEUX*NU)
        LA = NU * E       / (UN+NU) / (UN-DEUX*NU)
        MU = E  * D12     / (UN+NU)
C ======================================================================
C --- 3D/DP/AX ---------------------------------------------------------
C ======================================================================
        IF ( MOD(1:2) .EQ. '3D'     .OR.
     &       MOD(1:6) .EQ. 'D_PLAN' .OR.
     &       MOD(1:4) .EQ. 'AXIS'        )THEN
             DO 20 I = 1,NDI
             DO 20 J = 1,NDI
                 IF(I.EQ.J) HOOKNL(I,J) = AL
                 IF(I.NE.J) HOOKNL(I,J) = LA
 20          CONTINUE
             DO 25 I = NDI+1 , NDT
             DO 25 J = NDI+1 , NDT
                 IF(I.EQ.J) HOOKNL(I,J) = DEUX* MU
 25          CONTINUE
C ======================================================================
C --- CP/1D ------------------------------------------------------------
C ======================================================================
        ELSE IF ( MOD(1:6) .EQ. 'C_PLAN' .OR.
     +            MOD(1:2) .EQ. '1D' )THEN
             CALL UTMESS('F','CJS','LES MODELISATIONS AUTORISEES'//
     +                       ' SONT 3D ET D_PLAN ET AXIS')
        ENDIF
C ======================================================================
C --- LOIS D ECROUISSAGE : GR ET GX ------------------------------------
C ======================================================================
C --- ECROUISSAGE ISOTROPE ---------------------------------------------
C ======================================================================
        COEF1 = ( (I1D+QINIT)/TROIS/PA )**MUN5
        GR = - A * (UN-RD/RM)**DEUX * (I1D+QINIT) * COEF1
C ======================================================================
C --- ECROUISSAGE CINEMATIQUE ------------------------------------------
C ======================================================================
        DO 30 I=1, NDT
        SIGD(I) = YD(I)
 30     CONTINUE
C ======================================================================
C --- CALCUL DE S, SII, COS3TS, .... -----------------------------------
C ======================================================================
        CALL CJSQCO( GAMMA, SIGD, XD, PREF, EPSSIG, I1D,
     +               S, SII, SIIREL, COS3TS, HTS, DETS,
     +               Q, QII, QIIREL, COS3TQ, HTQ, DETQ )
C ======================================================================
C --- ON CALCULE DE TOUTES FACONS UNE PREDICTION ELASTIQUE -------------
C ======================================================================
        CALL LCPRMV ( HOOKNL, DEPS, DSIG )
        CALL LCSOVE ( SIGD, DSIG, SIGE )
        I1E = TRACE(NDI, SIGE)
        IF((I1E +QINIT).EQ. ZERO) THEN
           I1E = -QINIT+1.D-12*PA
        ENDIF
C
        CALL CJSQCO ( GAMMA, SIGE, XD, PREF, EPSSIG, I1E,
     +                SE, SIIE, SIIERE, CO3TSE, HTSE, DETSE,
     +                QE, QIIE, QIIERE, CO3TQE, HTQE, DETQE )

C ======================================================================
C --- SI QII EST QUASI-NULL, IL N'Y A PAS DE DEVIATEUR. ----------------
C --- DONC LE TENSEUR QQ(SIGD) N'EXISTE PAS. ON PRENDRA ALORS ----------
C --- A LA PLACE QQ(SIG_PREDICTION ELAS) -------------------------------
C ======================================================================
        IF( QIIREL .LE. EPSSIG ) THEN
           CALL CALCQ ( NDT, QE, GAMMA, PREF, EPSSIG, QQ)
        ELSE
           CALL CALCQ ( NDT, Q, GAMMA, PREF, EPSSIG, QQ)
        ENDIF
        CALL LCPRSC(QQ,QQ,QQII)
        QQII = SQRT(QQII)

        CALL LCPRSC(XD,XD,XII)
        XII = SQRT(XII)

        EPSV = ZERO
        DO 50 I = 1,NDI
        EPSV = EPSV + EPSD(I)+ DEPS(I)
  50    CONTINUE

        PC = PCO*EXP(-C*EPSV)

        IF(XII .LE. EPSSIG ) THEN
         PHI = UN
        ELSE IF(SIIREL .LE. EPSSIG ) THEN
              COSA = UN
              COSDIF = UN
              RR = RC + MUCJS*MAX(ZERO,LOG(TROIS*PC/(I1D+QINIT)))
              PHIO = COSA/( RR - HTS/HTQ*RM*COSDIF)
              PHI = PHIO * HTS * QQII
        ELSE
              COSA =  ( QII*QII - SII*SII - I1D*I1D*XII*XII ) /
     &                 (DEUX*SII*I1D*XII)

              TANGS = SQRT(UN-COS3TS*COS3TS) / COS3TS
              TANGQ = SQRT(UN-COS3TQ*COS3TQ) / COS3TQ
              TETAS = ATAN2(TANGS,1.D0) / TROIS
              TETAQ = ATAN2(TANGQ,1.D0) / TROIS
              COSDIF = COS(TETAS-TETAQ)

              RR = RC + MUCJS*MAX(ZERO,LOG(TROIS*PC/(I1D+QINIT)))
              PHIO = COSA/( RR - HTS/HTQ*RM*COSDIF)
              PHI = PHIO * HTS * QQII
        ENDIF

        DO 60 I=1, NDT
           GX(I) = (I1D+QINIT)/B*( QQ(I) + PHI*XD(I) ) * COEF1
 60     CONTINUE
C ======================================================================
C --- LOI D ECOULEMENT : GD --------------------------------------------
C ======================================================================
        CALL LCPRSC(QQ,XD,TRUC)
        TRUC = TRUC - RD

        DO 70 I=1, NDT
        DFDDS(I) = QQ(I) - TRUC*KRON(I)
 70     CONTINUE
C ======================================================================
C --- HYPOTHESE : SIGNE(S,DEPS) = SIGNE(S,DEPSDP) ----------------------
C ======================================================================
        CALL LCPRSC(S,DEPS,TRUC)
        IF(TRUC .GE. ZERO) THEN
          SIGNE =   UN
        ELSE
          SIGNE = - UN
        ENDIF

        SIIC = -RC * (I1D+QINIT) / HTS
        BETAPR = BETA * (SII/SIIC - UN) * SIGNE

        IF(SIIREL .GT. EPSSIG ) THEN
           COEF3 = BETAPR / SII
           COEF4 = UN / SQRT( BETAPR*BETAPR + TROIS )

           DO 80 I = 1,NDT
             NORM(I)   = COEF4 * ( COEF3 *  S(I) +  KRON(I) )
 80        CONTINUE
        ELSE
           COEF3 = BETAPR / SIIE
           COEF4 = UN / SQRT( BETAPR*BETAPR + TROIS )
           DO 81 I = 1,NDT
              NORM(I)   = COEF4 * ( COEF3 *  SE(I) +  KRON(I) )
 81        CONTINUE
        ENDIF

        CALL LCPRSC(DFDDS,NORM,PROD1)
        DO 90 I = 1,NDT
           GD(I) = DFDDS(I) - PROD1 * NORM(I)
 90     CONTINUE
C ======================================================================
C --- CALCUL DE DLAMBD -------------------------------------------------
C ======================================================================
        FD =  QIIE * HTQE + RD * (I1E+QINIT)
        TRGD   = TRACE(NDI,GD)
        TRDEPS = TRACE(NDI,DEPS)
        DI1DL = - TROIS*KE*TRGD
        DRDL  = GR
        CALL LCPRMV ( HOOKNL, GD, DQDL)
        DO 110 I = 1, NDT
           DQDL(I) = - DQDL(I) + KE*TRGD*( KRON(I) + TROIS*XD(I) )
     &                               - GX(I) * ( I1D + TROIS*KE*TRDEPS )
 110    CONTINUE
C ======================================================================
C --- POUR LE CALCUL DE DQIIDL, DHDL: ----------------------------------
C --- SI QII EST QUASI-NULL, IL N'Y A PAS DE DEVIATEUR. ----------------
C ======================================================================
        IF( QIIREL .LE. EPSSIG ) THEN
           DQIIDL = ZERO
           DO 125 I = 1, NDT
             DQIIDL = DQIIDL + QE(I)/QIIE * DQDL(I)
 125       CONTINUE

           RCOS3T = COS3T(NDT, QE, PREF, EPSSIG)
           CALL     CJST (QE,DQE)
           COEF5  = SQRT(TROIS/DEUX)*GAMMA/HTQE**CINQ/QIIE**TROIS
           COEF6  = - GAMMA*RCOS3T/(DEUX*HTQE**CINQ*QIIE**DEUX)

           DHDL = ZERO
           DO 135 I = 1, NDT
             DHDL = DHDL + ( COEF5*DQE(I) + COEF6*QE(I) ) * DQDL(I)
 135       CONTINUE
           DFDDL = HTQE*DQIIDL + QII*DHDL + RD*DI1DL + (I1D+QINIT)*DRDL
        ELSE
           DQIIDL = ZERO
           DO 120 I = 1, NDT
             DQIIDL = DQIIDL + Q(I)/QII * DQDL(I)
 120       CONTINUE

           RCOS3T = COS3T(NDT, Q, PREF, EPSSIG)
           CALL     CJST (Q,DQ)
           COEF5  = SQRT(TROIS/DEUX)*GAMMA/HTQ**CINQ/QII**TROIS
           COEF6  = - GAMMA*RCOS3T/(DEUX*HTQ**CINQ*QII**DEUX)

           DHDL = ZERO
           DO 130 I = 1, NDT
              DHDL = DHDL + ( COEF5*DQ(I) + COEF6*Q(I) ) * DQDL(I)
 130       CONTINUE
           DFDDL = HTQ*DQIIDL + QII*DHDL + RD*DI1DL + (I1D+QINIT)*DRDL
        ENDIF

        DLAMBD = - FD/DFDDL
C ======================================================================
C --- CALCUL DES INCREMENTS DE DEFORMATIONS ELASTIQUE ------------------
C ======================================================================
        DO 140 I=1, NDT
           DEPSE(I) = DEPS(I) - DLAMBD*GD(I)
 140    CONTINUE
C ======================================================================
C --- CALCUL INCREMENT DE CONTRAINTES  DSIG = HOOKNL.DEPSE -------------
C ======================================================================
        CALL LCPRMV ( HOOKNL, DEPSE, DSIG )
C ======================================================================
C --- CALCUL INCREMENT DE LA VARIABLE INTERNE R ------------------------
C ======================================================================
        DR =  DLAMBD*DRDL
C ======================================================================
C --- CALCUL INCREMENT DE LA VARIABLE INTERNE X ------------------------
C ======================================================================
        DO 150 I=1, NDT
        DX(I) =  DLAMBD*GX(I)
 150    CONTINUE
C ======================================================================
C --- SOLUTION D ESSAI -------------------------------------------------
C ======================================================================
        DO 160 I=1, NDT
           DY(I) = DSIG(I)
 160    CONTINUE
        DY(NDT+1) = DR
        DO 170 I=1, NDT
        DY(NDT+1+I) = DX(I)
 170    CONTINUE
        DY(2*NDT+2) = DLAMBD
C ======================================================================
        CALL JEDEMA ()
C ======================================================================
        END

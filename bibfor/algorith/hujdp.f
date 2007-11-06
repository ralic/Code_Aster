        SUBROUTINE HUJDP (MOD, DEPS, SIGD, SIGF, MATER, VIN)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/11/2007   AUTEUR KHAM M.KHAM 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       ---------------------------------------------------------------
C       TESTS SUR LES CRITERES D'EVOLUTION RELATIFS AUX CONTRAINTES
C       IN  DEPS   :  INCREMENT DE DEFORMATION
C           SIGD   :  CONTRAINTE  A T
C           SIGF   :  CONTRAINTE ELASTIQUE A T+DT
C           MATER  :  PROPRIETES MATERIAU
C       OUT DEPS   :  INCREMENT DE DEFORMATION REAJUSTE
C       ---------------------------------------------------------------
        INTEGER       NDT, NDI, I, J, NP
        REAL*8        I1D, N, PREF, K0, IRET, NI
        REAL*8        DEPS(6), SIGD(6), SIGF(6)
        REAL*8        MATER(22,2), TOLE, RP, RI, VIN(*)
        REAL*8        ZERO, UN, DEUX, DEPSV, COEF 
        REAL*8        EPSVP, BETA, D, FR, I1E, TETA 
        REAL*8        DFDS(6), E, NU, AL, LA, DEMU 
        REAL*8        HOOKNL(6,6), DSIG(6), FIDSIG   
        REAL*8        PCO, RATIO, P, QF, QD, TAUF(3)
        REAL*8        TAUD(3), RELA1, RELA2  
        CHARACTER*8   MOD
        
        COMMON /TDIM/ NDT, NDI

        DATA          ZERO  / 0.D0 /
        DATA          UN    / 1.D0 /
        DATA          DEUX  / 2.D0 /

C       ---------------------------------------------------------------
        PREF  = MATER(8,2)
        N     = MATER(1,2)
        BETA  = MATER(2,2)
        D     = MATER(3,2)
        PCO   = MATER(7,2)
        EPSVP = VIN(23)
        
        DEPSV = DEPS(1)+DEPS(2)+DEPS(3)
        I1D   = (SIGD(1)+SIGD(2)+SIGD(3))/3.D0    
         
        TOLE = 1.D-1

C ----------------------------------------------------
C 1 --- CRITERE LIMITANT L EVOLUTION DE P: DP/P < TOLE
C ----------------------------------------------------

        K0   = MATER(1,1)/(3.D0*(UN-DEUX*MATER(2,1)))
        COEF = (I1D/PREF)**N
        RP   = K0*COEF*DEPSV/(TOLE*I1D)
        IF (RP .GT. 10) RP = 10.D0     
        NP = NINT(RP)   
        IF(NP.LT.1) NP=1
        RP = ABS(DEPSV*BETA/LOG(UN+TOLE))

C ----------------------------------------------------------------      
C 2 --- CRITERE A RESPECTER POUR L EVOLUTION DU MECANISME ISOTROPE
C   ---         FR/(DFDS*C*DEPS)*TOLE > TETA = 1/RI             
C ----------------------------------------------------------------
        FR = D*PCO*EXP(-BETA*EPSVP)
        DO 10 I = 1, NDI
          DFDS(I) = - UN/3.D0
 10     CONTINUE
        
C ======================================================================
C -------------------- 2.1 CONSTRUCTION DE C ---------------------------
C ======================================================================

        CALL LCINMA (ZERO, HOOKNL)
        
        I1E  = (SIGF(1)+SIGF(2)+SIGF(3))/3.D0
        E    = MATER(1,1) * (I1E/PREF)**N
        NU   = MATER(2,1)
        AL   = E *(UN-NU) /(UN+NU) /(UN-DEUX*NU)
        DEMU = E        /(UN+NU)
        LA   = E*NU/(UN+NU)/(UN-DEUX*NU)
        
C ====================================================================
C --- 3D/DP/AX -------------------------------------------------------
C ====================================================================

        IF (MOD(1:2) .EQ. '3D'     .OR.
     &      MOD(1:6) .EQ. 'D_PLAN' .OR.
     &      MOD(1:4) .EQ. 'AXIS') THEN
          DO 30 I = 1, NDI
          DO 30 J = 1, NDI
            IF (I.EQ.J) HOOKNL(I,J) = AL
            IF (I.NE.J) HOOKNL(I,J) = LA
 30       CONTINUE
          DO 35 I = NDI+1, NDT
             HOOKNL(I,I) = DEMU
 35       CONTINUE
 
C ====================================================================
C --- CP/1D ----------------------------------------------------------
C ====================================================================

        ELSEIF (MOD(1:6) .EQ. 'C_PLAN' .OR.
     &            MOD(1:2) .EQ. '1D')   THEN
          CALL U2MESS('F','COMPOR1_4')
        ENDIF

C ====================================================================
C -------------- 2.2 CALCUL DE DSIGMA = C*DEPSILON -------------------
C ====================================================================

        CALL LCPRMV (HOOKNL, DEPS, DSIG)

C ====================================================================
C -------------- 2.3 CALCUL DE FIDSIG = DFDS*DSIG --------------------
C ====================================================================
        FIDSIG = ZERO
        DO 40 I = 1, NDI
          FIDSIG = FIDSIG + DSIG(I) * DFDS(I)
 40     CONTINUE
        IF(FIDSIG.EQ.ZERO)THEN
          TETA = 1.D0
        ELSE
          TETA = ABS(FR/FIDSIG)*TOLE*1.D-1
        ENDIF
        RI = UN/TETA
        IF (RI .GT. 10.D0) RI = 10.D0     
        NI = NINT(RI)   
        IF(NI.LT.1) NI=1

        IF(NP.LT.NI)NP = NI

C -------------------------------------------------------
C 1 --- CRITERE LIMITANT L EVOLUTION DE Q: DQ/PREF < TOLE
C -------------------------------------------------------

        DO 45 I = 1, 3
          CALL HUJPRJ(I, SIGF, TAUF, P, QF)
          CALL HUJPRJ(I, SIGD, TAUD, P, QD)
          IF(ABS(QD).LT.TOLE)THEN
            RATIO = ZERO
          ELSE  
            RATIO = ABS(QF-QD)/ABS(PREF)
          ENDIF  
          RELA1 = ZERO
          RELA2 = ZERO
          IF(ABS(TAUD(1)).GT.TOLE)
     &            RELA1 = ABS((TAUF(1)-TAUD(1))/PREF)
          IF(ABS(TAUD(3)).GT.TOLE)
     &            RELA2 = ABS((TAUF(3)-TAUD(3))/PREF)
          IF(RELA1.GT.RATIO)RATIO = RELA1
          IF(RELA2.GT.RATIO)RATIO = RELA2
C         
          IF (RATIO.GT.TOLE)THEN
            RI = RATIO / TOLE
          ENDIF
          IF (RI .GT. 10.D0) RI = 10.D0
          NI = NINT(RI) 
          IF(NI.LT.1) NI=1
C
          IF(NP.LT.NI)NP = NI
 45     CONTINUE 
        
C
C --- INCREMENT DE DEFORMATION CHOISI : DEPS = DEPS / (N = MAX(RI,RP))
C       
        DO 50 I = 1, NDT
          DEPS(I) = DEPS(I)/NP
  50    CONTINUE
        END

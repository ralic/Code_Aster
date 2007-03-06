        SUBROUTINE HUJIID (MOD, MATER, INDI, DEPS, YD, DY)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/03/2007   AUTEUR KHAM M.KHAM 
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
C          DEPS     :  INCREMENT DE DEFORMATION
C          YD       :  VARIABLES A T = (SIGD, VIND, DLAMB)
C     OUT  DY       :  SOLUTION D ESSAI (DSIG, DVIN, DDLAMB)
C     ----------------------------------------------------------------
C     Y CONTIENT LES CONTRAINTES : SIG
C                LES VARIABLES D'ECROUISSAGE : R, EPSI_VOLU_P
C                LES MULTIPLICATEURS PLASTIQUES : DLAMBDA
C ====================================================================
        INTEGER     NDT, NDI, I, J, K, KK, L, LL
        INTEGER     NBMECA, INDI(4), IRET
        INTEGER     IFM, NIV
        REAL*8      DEPS(6), DEPSE(6), HOOKNL(6,6)
        REAL*8      DSIG(6), SIGD(3), P(3), Q(3), PE(3), QE(3)
        REAL*8      YD(15), YE(15), DY(15), F2(4)
        REAL*8      MATER(20,2), N, BETA, B, D, M, PCO, PC, PREF
        REAL*8      PHI, ANGDIL, MDIL, ACYC, AMON, CMON
        REAL*8      RC(4), TRACE, EPSVP, AD(4), KSI(4)
        REAL*8      E, NU, AL, LA, MU, I1D, I1DE
        REAL*8      DFDL(4,4), DR(4), DEPSVP
        REAL*8      DEGR, ZERO, UN, D12, D13, DEUX
        REAL*8      DET, TOLE, COEF
        REAL*8      PSI(24), DFDS(6), DPSIDS(6,6)
        CHARACTER*8 MOD
        LOGICAL     DEBUG
C ====================================================================
        PARAMETER   ( D12  = .5D0 )
        PARAMETER   ( D13  = .3333333333334D0 )
        PARAMETER   ( UN   = 1.D0 )
        PARAMETER   ( ZERO = 0.D0 )
        PARAMETER   ( DEUX = 2.D0 )
C        PARAMETER   ( EPSSIG = 1.D-8 )
        PARAMETER   ( TOLE = 1.D-6 )
        PARAMETER   ( DEGR = 0.0174532925199D0 )
C ====================================================================
        COMMON /TDIM/   NDT, NDI
        COMMON /MESHUJ/ DEBUG
C ====================================================================
C        CALL JEMARQ ()
        CALL INFNIV (IFM, NIV)


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
C        CCYC   = MATER(11,2)
        CMON   = MATER(12,2)
        M      = SIN(DEGR*PHI)
        MDIL   = SIN(DEGR*ANGDIL)


C ====================================================================
C --- PREMIER INVARIANT ET AUTRES GRANDEURS UTILES -------------------
C ====================================================================
        IRET   = 0
        I1D    = D13*TRACE(NDI,YD)
        NBMECA = 0
        DO 4 K = 1, 4
          IF (INDI(K).GT.0) NBMECA=NBMECA+1
 4        CONTINUE


C ====================================================================
C --- OPERATEUR DE RIGIDITE NON LINEAIRE -----------------------------
C ====================================================================
C --- OPERATEUR LINEAIRE NON LINEAIRE --------------------------------
C ====================================================================
        CALL LCINMA (ZERO, HOOKNL)

        E    = MATER(1,1) * (I1D/PREF)**N
        NU   = MATER(2,1)
        AL   = E *(UN-NU) /(UN+NU) /(UN-DEUX*NU)
        LA   = NU*E       /(UN+NU) /(UN-DEUX*NU)
        MU   = D12*E      /(UN+NU)


C ====================================================================
C --- 3D/DP/AX -------------------------------------------------------
C ====================================================================
        IF (MOD(1:2) .EQ. '3D'     .OR.
     &      MOD(1:6) .EQ. 'D_PLAN' .OR.
     &      MOD(1:4) .EQ. 'AXIS') THEN
          DO 20 I = 1, NDI
          DO 20 J = 1, NDI
            IF (I.EQ.J) HOOKNL(I,J) = AL
            IF (I.NE.J) HOOKNL(I,J) = LA
 20         CONTINUE

          DO 25 I = NDI+1, NDT
            HOOKNL(I,I) = DEUX* MU
 25         CONTINUE


C ====================================================================
C --- CP/1D ----------------------------------------------------------
C ====================================================================
        ELSEIF (MOD(1:6) .EQ. 'C_PLAN' .OR.
     &          MOD(1:2) .EQ. '1D')   THEN
          CALL U2MESS('F','COMPOR1_4')
        ENDIF


C ====================================================================
C --- ON CALCULE DE TOUTES FACONS UNE PREDICTION ---------------------
C --- ELASTIQUE EN TANT QUE DE BESOIN --------------------------------
C ====================================================================
        CALL LCPRMV (HOOKNL, DEPS, DSIG)
        CALL LCSOVE (YD, DSIG, YE)


C ---- SI PAS DE MECANISME DEVIATOIRE, LA SOLUTION D'ESSAI EST
C       SIG=SIGE ,  R4 = R4- , EPSVP = EPSVP- , DLAMBDA = 0.
        IF ((NBMECA .EQ. 1) .AND. (INDI(NBMECA) .EQ. 4)) THEN

          DO 31 I = NDT+1, 15
            DY(I)=ZERO
 31         CONTINUE
           
          DO 32 I = 1, NDT
            DY(I) = DSIG(I)
 32         CONTINUE

          GOTO 9999

        ENDIF

        DO 5 K = 1, NBMECA
          RC(K) = YD(NDT+1+K)
          CALL HUJDDD('PSI   ', INDI(K), MATER, INDI, YD,
     &                 PSI((K-1)*NDT+1), DPSIDS,IRET)
          IF (IRET.EQ.1) GOTO 999
          IF (INDI(K) .LT. 4) THEN
            CALL HUJPRJ (INDI(K), YD, SIGD, P(K), Q(K))
            CALL HUJPRJ (INDI(K), YE, SIGD, PE(K), QE(K))
            IF ((P(K)/PREF) .LE. TOLE .OR.
     &         (PE(K)/PREF) .LE. TOLE) GOTO 999
            CALL HUJKSI ('KSI   ', MATER, RC(K), KSI(K))
            AD(K) = ACYC+KSI(K)*(AMON-ACYC)
          ENDIF
            YE(NDT+1+K) = YD(NDT+1+K)
 5        CONTINUE

        EPSVP     = YD(NDT+1)
        YE(NDT+1) = YD(NDT+1)
        PC        = PCO*EXP(-BETA*EPSVP)
        
        CMON = CMON * PC/PREF

        COEF = UN


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
       DO 45 K = 1, NBMECA
         DO 45 L = 1, NBMECA
           DFDL(K,L) = ZERO
 45        CONTINUE


C ---> I. CALCUL DE DF. / DDLAMB. POUR DDLAMB. = 0        
C ---> I.1. CALCUL DE DFDSE(K)*HOOKNL*PSI-(L)
       DO 40 K = 1, NBMECA
         KK = INDI(K)
         CALL HUJDDD ('DFDS  ', KK, MATER, INDI, YE,
     &                DFDS, DPSIDS,IRET)
         IF (IRET.EQ.1) GOTO 999
         DO 40 L = 1, NBMECA
           LL = (L-1)*NDT
           DO 40 I = 1, NDT
             DO 40 J = 1, NDT
               DFDL(K,L) = DFDL(K,L) - HOOKNL(I,J)*DFDS(I)*PSI(LL+J)
 40            CONTINUE


C ---- FIN I.1.
C ---> I.2. CALCUL DE DFDEVPE(K)*DEVPDDLAMB-(L)     
C ----  I.2.1. MECANISME DEVIATOIRE
        DO 11 K = 1, NBMECA
        
          KK = INDI(K)
          IF (KK .LT. 4) THEN
          
            F2(K) = -QE(K) - M*PE(K)*RC(K) * ( UN - B*LOG(PE(K)/PC) )

            DO 12 L = 1, NBMECA
            
              LL = INDI(L)
              IF (LL .LT. 4) THEN
                DFDL(K,L) = DFDL(K,L) + B*M*PE(K)*RC(K)*BETA
     &                      * KSI(L)*COEF*(MDIL+Q(L)/P(L))
              ELSEIF (LL .EQ. 4) THEN
                DFDL(K,L) = DFDL(K,L) + B*M*PE(K)*RC(K)*BETA
              ENDIF

 12        CONTINUE


C ---- I.2.2. MECANISME ISOTROPE
          ELSEIF (KK .EQ. 4) THEN

            IF (K .NE. NBMECA) CALL U2MESS ('F', 'COMPOR1_5')

            I1DE  = D13*TRACE(NDI,YE)
            F2(K) = -ABS(I1DE) - RC(K)*D*PC
            DFDL(K,K) = DFDL(K,K) + RC(K)*D*PC*BETA
            DO 13 L = 1, NBMECA-1, 1
              LL = INDI(L)
              DFDL(K,L) = DFDL(K,L) + RC(K)*D*PC*BETA
     &                    * KSI(L)*COEF*(MDIL+Q(L)/P(L))
 13           CONTINUE

          ENDIF
 11       CONTINUE


C ---- FIN I.2.
C ---> I.3. CALCUL DE DFDRE(K)*DRDLAMB-(K)     
          DO 14 K = 1, NBMECA, 1
            KK = INDI(K)
            IF (KK .LT. 4) THEN
              DFDL(K,K) = DFDL(K,K) + M*PE(K)*(UN-B*LOG(PE(K)/PC))
     &                    * (UN-RC(K))**DEUX /AD(K)
            ELSEIF (KK .EQ. 4) THEN
              DFDL(K,K) = DFDL(K,K) + D*PC * (UN-RC(K))**DEUX /CMON
            ENDIF
 14         CONTINUE


C ---- RESOLUTION PAR PIVOT DE GAUSS      
       CALL MGAUSS ('NFVP', DFDL, F2, 4, NBMECA, 1, DET, IRET)
       IF (IRET.EQ.1)
     & CALL U2MESS ('F', 'COMPOR1_6')

C --- MULTIPLICATEUR PLASTIQUE NEGATIF NON AUTORISE
C          DO 15 K=1,NBMECA
C            IF (F2(K) .LT. ZERO) F2(K)=ZERO
C 15       CONTINUE


C ====================================================================
C --- CALCUL DES INCREMENTS DE DEFORMATIONS ELASTIQUE ----------------
C ====================================================================
       DO 240 I = 1, NDT
         DEPSE(I) = DEPS(I)
 240     CONTINUE
 
       DO 242 K = 1, NBMECA
         KK = (K-1)*NDT
         DO 242 I = 1, NDT
           DEPSE(I) = DEPSE(I) - F2(K)*PSI(KK+I)
 242       CONTINUE


C ====================================================================
C --- CALCUL INCREMENT DE CONTRAINTES  DSIG = HOOKNL-.DEPSE ----------
C ====================================================================
       CALL LCPRMV (HOOKNL, DEPSE, DSIG)


C ====================================================================
C --- CALCUL INCREMENT DE LA VARIABLE INTERNE RC ---------------------
C ====================================================================
       DO 250 K = 1, NBMECA
          KK = INDI(K)
          IF (KK.LT.4) THEN
            DR(K) =  F2(K) *(UN-RC(K))**DEUX /AD(K)
          ELSEIF(KK.EQ.4) THEN
            DR(K) =  F2(K) *(UN-RC(K))**DEUX /CMON
          ENDIF
 250      CONTINUE


C ====================================================================
C --- CALCUL INCREMENT DE LA VARIABLE INTERNE DEPSVP -----------------
C ====================================================================
        DEPSVP = ZERO
        DO 251 K = 1, NBMECA
          KK=INDI(K)
          IF (KK .LT. 4) THEN
            DEPSVP = DEPSVP - F2(K)*COEF*KSI(K)*(MDIL+Q(K)/P(K))
          ELSEIF (KK .EQ. 4) THEN
            DEPSVP = DEPSVP - F2(K)
          ENDIF
 251      CONTINUE


C ====================================================================
C --- SOLUTION D ESSAI -----------------------------------------------
C ====================================================================
        DO 259 I = 1, 15
          DY(I) = ZERO
 259      CONTINUE
           
        DO 260 I = 1, NDT
          DY(I) = DSIG(I)
 260      CONTINUE
 
        DY(NDT+1) = DEPSVP
        
        DO 270 K = 1, NBMECA
          DY(NDT+1+K)        = DR(K)
          DY(NDT+1+NBMECA+K) = F2(K)
 270      CONTINUE

        GOTO 9999

 999    CONTINUE

        IF (DEBUG) THEN
          WRITE (IFM,'(A)') 'HUJJID :: LOG(PK/PC) NON DEFINI'
          WRITE (IFM,'(A)') '- ON NE FAIT PAS LA PREDICTION -'
        ENDIF

        DO 300 I = 1, 15
          DY(I) = ZERO
 300      CONTINUE

 9999   CONTINUE
C         CALL JEDEMA ()
        END

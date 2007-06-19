        SUBROUTINE HUJJID( MOD, MATER, INDI, DEPS,
     &                     YD, YF, R, SIGNE, DRDY, IRET )
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/05/2007   AUTEUR KHAM M.KHAM 
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
C  --------------------------------------------------------------------
C  INTEGRATION PLASTIQUE (MECANISME DEVIATOIRE SEUL) DE LA LOI HUJEUX
C
C  RESOLUTION PAR METHODE DE NEWTON   DRDY(DY).DDY = - R(DY)
C
C  CALCUL DU SECOND MEMBRE : - R(DY)
C  CALCUL DU JACOBIEN	   : DRDY(DY)
C  DY	=  ( SIG     , ESPVP	 , R	   , LAMBDA   )
C  R	= -( LE      , LEVP	 , LR	   , LF       )
C  DRDY =  ( DLEDS   , DLEDEVP   , DLEDR   , DLEDLA   )
C  	   ( DLEVPDS , DLEVPDEVP , DLEVPDR , DLEVPDLA )
C  	   ( DLRDS   , DLRDEVP   , DLRDR   , DLRDLA   )
C  	   ( DLFDS   , DLFDEVP   , DLFDR   , DLFDLA   )
C =====================================================================
C  IN	MOD   :  MODELISATION
C  	MATER :  COEFFICIENTS MATERIAU
C  	DEPS  :  INCREMENT DE DEFORMATION
C  	YD    :  VARIABLES A T = (SIGD, VIND, DLAMBDAD)
C  	YF    :  VARIABLES A T+DT = (SIGF, VINF, DLAMBDAF)
C  VAR  IND   :  TABLEAU DES NUMEROS DE MECANISMES ACTIFS
C  OUT  R     :  SECOND MEMBRE
C  	SIGNE :  SIGNE DE SK:DEPSDP POUR LE MECANISME K
C  	DRDY  :  JACOBIEN
C  	IRET  :  CODE RETOUR
C  	         = 0 OK
C  	         = 1 NOOK : SI LA SUBDIVISION DU PAS DE TEMPS EST ACTIV
C  		 	    DANS STAT_NON_LINE, IL Y A SUBDIVISION
C =====================================================================
        INTEGER     NDT, NDI, NMOD, I, II, J, K, KK, L
        INTEGER     INDI(4), NBMECA, IRET
        INTEGER     IFM, NIV
        PARAMETER   (NMOD = 15)
        REAL*8      DEPSP(6), DEPSPK(3), DEPSE(6)
        REAL*8      SIGD(3),SIGF(6),P(3),Q(3),SIGNE(4)
        REAL*8      YD(NMOD), YF(NMOD), DRDY(NMOD,NMOD)
        REAL*8      MATER(20,2), N, BETA, D, M, PCO, PREF, PC
        REAL*8      DEGR, PHI, ANGDIL, MDIL, B, DKSIDR(3)
        REAL*8      RC(4), DLAMBD(4), DEPSDS(6,6)
        REAL*8      HOOKNL(6,6), HOOK(6,6), DHOKDS(6,6)
        REAL*8      I1F, E, NU, AL, DEMU, COEF0, DCOEF0
        REAL*8      LE(6), LEVP, LR(4), LF(4), R(NMOD), DELTA(6)
        REAL*8      DLEDS(6,6), DLEDEV(6), DLEDR(6,4), DLEDLA(6,4)
        REAL*8      DLEVDS(6), DLEVDE, DLEVDR(4), DLEVDL(4)
        REAL*8      DLRDS(4,6), DLRDLE(4), DLRDR(4,4), DLRDLA(4,4)
        REAL*8      DLFDS(4,6), DLFDLE(4), DLFDR(4,4), DLFDLA(4,4)
        REAL*8      CDE(6), CTILD(6), CD2FDS(6,6)
        REAL*8      DLADR(6), DLEDR1(6), PSI(24), AD(3), KSI(4)
        REAL*8      DPSIDS(6,6), DFDS(6), DLEK(6)
        REAL*8      TRACE, EPSVP, DEPS(6)
        REAL*8      LEPV, ACYC, AMON, CMON
        REAL*8      ZERO, UN, D12, D13, DEUX, TRUC
        REAL*8      TOLE, COEF, MUL, CCOND
        CHARACTER*8 MOD
        LOGICAL     DEBUG
C =====================================================================
C        PARAMETER   ( DSQR   = 1.41421356237D0  )
        PARAMETER   ( D12    = 0.5D0  )
        PARAMETER   ( D13    = 0.333333333334D0  )
        PARAMETER   ( UN     = 1.D0   )
        PARAMETER   ( ZERO   = 0.D0   )
        PARAMETER   ( DEUX   = 2.D0   )
        PARAMETER   ( TOLE   = 1.D-6 )
        PARAMETER   ( DEGR = 0.0174532925199D0 )
C =====================================================================
        COMMON /TDIM/   NDT, NDI
        COMMON /MESHUJ/ DEBUG
C =====================================================================
C        CALL JEMARQ ()
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
C        CCYC   = MATER(11,2)
        CMON   = MATER(12,2)
        M      = SIN(DEGR*PHI)
        MDIL   = SIN(DEGR*ANGDIL)

 
C =====================================================================
C --- PREMIER INVARIANT ET AUTRES GRANDEURS UTILES --------------------
C =====================================================================
        I1F  = D13 * TRACE(NDI,YF)
        IF ( I1F .GT. ZERO ) I1F = 1.D-12*PREF
        
        DO 6 I = 1, NDT
          SIGF(I) = YF(I)
 6       CONTINUE
        
        NBMECA = 0
        DO 4 K = 1, 4
          IF (INDI(K) .GT. 0) NBMECA = NBMECA + 1
 4        CONTINUE
 
         DO 5 K = 1, NBMECA
          RC(K) = YF(NDT+1+K)
          CALL HUJDDD('PSI   ', INDI(K), MATER, INDI, YF,
     &                PSI((K-1)*NDT+1), DPSIDS, IRET)
          IF (IRET .EQ. 1) GOTO 1000
          IF (INDI(K) .LT. 4) THEN
            CALL HUJPRJ (INDI(K), SIGF, SIGD, P(K), Q(K))
            IF ((P(K)/PREF) .LE. TOLE) GOTO 999
            CALL HUJKSI('DKSIDR', MATER, RC(K), DKSIDR(K), IRET)
            CALL HUJKSI('KSI   ', MATER, RC(K), KSI(K), IRET)
            IF (IRET .EQ. 1) GOTO 1000
            AD(K)  = ACYC+KSI(K)*(AMON-ACYC)
          ELSEIF (INDI(K) .EQ. 4) THEN
            KSI(K) = UN
          ELSE
            CALL U2MESS('F', 'COMPOR1_8')
          ENDIF
          DLAMBD(K) = YF(NDT+1+NBMECA+K)
 5        CONTINUE
 
        EPSVP = YF(NDT+1)
        PC    = PCO*EXP(-BETA*EPSVP)
        
        CMON = CMON * PC/PREF
        
        COEF = UN


C --- CONDITIONNEMENT DE LA MATRICE JACOBIENNE
        CCOND= MATER(1,1)


C =====================================================================
C --- OPERATEURS DE RIGIDITE ET DE SOUPLESSE (LINEAIRES OU NON LINEA.) 
C =====================================================================
C --- OPERATEURS LINEAIRES --------------------------------------------
C =====================================================================
        CALL LCINMA (ZERO, HOOK)
        E  = MATER(1,1)
        NU = MATER(2,1)
        AL = E*(UN-NU) /(UN+NU) /(UN-DEUX*NU)
        DEMU = E       /(UN+NU)
                
        
C =====================================================================
C --- 3D/DP/AX --------------------------------------------------------
C =====================================================================
        IF (MOD(1:2) .EQ. '3D'     .OR.
     &      MOD(1:6) .EQ. 'D_PLAN' .OR.
     &      MOD(1:4) .EQ. 'AXIS')  THEN
     
           DO 30 I = 1, NDI
             DO 30 J = 1, NDI
               IF (I.EQ.J) THEN
                  HOOK(I,J) = AL
               ENDIF
               IF (I.NE.J) THEN
                  HOOK(I,J) = DEMU
               ENDIF
 30            CONTINUE
           DO 35 I = NDI+1, NDT
             HOOK(I,I) = DEMU
 35          CONTINUE
 
 
C =====================================================================
C --- CP/1D -----------------------------------------------------------
C =====================================================================
        ELSEIF (MOD(1:6) .EQ. 'C_PLAN' .OR.
     &          MOD(1:2) .EQ. '1D')   THEN
     
          CALL U2MESS('F', 'COMPOR1_4')
     
        ENDIF
        
        
C =====================================================================
C --- OPERATEUR NON LINEAIRE ------------------------------------------
C =====================================================================
        COEF0 = (I1F/PREF) ** N
        DO 40 I = 1, NDT
          DO 40 J = 1, NDT
            HOOKNL(I,J) = COEF0*HOOK(I,J)
 40         CONTINUE
 
 
C =====================================================================
C --- DERIVEE PAR RAPPORT A DS DE L'OPERATEUR NON LINEAIRE: DHOOKDS ---
C =====================================================================
        DCOEF0 = D13*N/PREF * (I1F/PREF)**(N-1)
        DO 41 I = 1, NDT
          DO 41 J = 1, NDT
            DHOKDS(I,J) = DCOEF0*HOOK(I,J)
 41         CONTINUE


C =====================================================================
C --- I. CALCUL DE DLEDS (6X6) ----------------------------------------
C =====================================================================
C ---> I.1. CALCUL DE CTILD = DHOOKDS*(DEPS - DEPSP)
C ---> I.1.1. CALCUL DE DEPSP A T+DT
        DO 50 I = 1,NDT
          DEPSP(I) = ZERO
  50     CONTINUE

       DO 51 K = 1, NBMECA
         KK = (K-1)*NDT
         DO 53 I = 1, NDT
           DEPSP(I) = DEPSP(I) + DLAMBD(K)*PSI(KK+I)
 53        CONTINUE
           
C ---- SIGNE DE S:DEPSDP POUR CHAQUE MECANISME
C          CALL HUJPRJ ( KK, DEPSP, DEPSPK, PK, QK)
C          CALL HUJPRJ ( KK, SIGF, SIGDK, PK, QK )
C          TRUC = DEPSPK(1)*SIGDK(1)+DEPSPK(2)*SIGDK(2)+DEPSPK(3)*SIGDK
C          IF(TRUC .GE. ZERO) THEN
C            SIGNE(K) = UN
C          ELSE
C            SIGNE(K) = -UN
C          ENDIF
        
 51      CONTINUE
  
 
C ------------ FIN I.1.1.
       DO 52 I = 1, NDT
         DEPSE(I) = DEPS(I) - DEPSP(I)
 52      CONTINUE
 
       CALL LCPRMV (DHOKDS, DEPSE, CTILD)
       
       
C ------------ FIN I.1.
C ---> I.2. CALCUL DE CD2FDS = HOOK * DEPSDS
C                     (6X6)    (6X6)  (6X6)
       CALL LCINMA (ZERO, DEPSDS)

       DO 60 K = 1, NBMECA
         KK = INDI(K)
         IF (KK .EQ. 4) GOTO 610
         CALL HUJDDD('DPSIDS', KK, MATER, INDI, YF,
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
          DLEDS(I,I) = UN /CCOND
 63       CONTINUE
 
       DO 61 I = 1, NDT
         DO 62 J = 1, NDI
           DLEDS(I,J) = DLEDS(I,J) - (CTILD(I) - CD2FDS(I,J)) /CCOND
 62        CONTINUE
          DO 61 J = NDI+1, NDT
            DLEDS(I,J) = DLEDS(I,J) + CD2FDS(I,J) /CCOND
  61        CONTINUE
         
         
C =====================================================================
C --- II. CALCUL DE DLEDR (6XNBMEC) -----------------------------------
C =====================================================================
      DO 70 I = 1, NDT
        DO 70 K = 1, NBMECA
           DLEDR(I,K) = ZERO
 70        CONTINUE

      DO 71 K = 1, NBMECA
         KK = INDI(K)
         IF (KK .EQ. 4) GOTO 710
         MUL = - DLAMBD(K)*COEF*D12*(MDIL+Q(K)/P(K))*DKSIDR(K)
         
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

         CALL LCPRMV (HOOKNL, DELTA, DLEDR1)
         DO 71 I = 1, NDT
           DLEDR(I,K) = DLEDR1(I) /CCOND
 71        CONTINUE
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
      DO 91 K = 1, NBMECA
         KK = (K-1)*NDT+1
         CALL LCPRMV (HOOKNL, PSI(KK), DLEK)         
         DO 91 I = 1, NDT
           DLEDLA(I,K) = DLEK(I) /CCOND
 91        CONTINUE

       
C =====================================================================
C --- V. CALCUL DE DLRDS (NBMECX6) ------------------------------------
C =====================================================================
      DO 100 K = 1, NBMECA
        DO 100 I = 1, NDT
          DLRDS(K,I) = ZERO
 100      CONTINUE


C =====================================================================
C --- VI. CALCUL DE DLRDR (NBMECXNBMEC) -------------------------------
C =====================================================================
      DO 110 K = 1, NBMECA
        DO 110 L = 1, NBMECA
          DLRDR(K,L) = ZERO
 110    CONTINUE
 
      DO 111 K = 1, NBMECA
        KK = INDI(K)
        IF (KK .LT. 4) THEN
          MUL        = (UN-RC(K))/AD(K)
          DLRDR(K,K) = UN + DEUX*DLAMBD(K)*MUL
     &  + DLAMBD(K)*DKSIDR(K)*(AMON-ACYC)*MUL**DEUX
        ELSEIF (KK.EQ.4) THEN
          DLRDR(K,K) = UN + DEUX*DLAMBD(K)*(UN-RC(K))/CMON
        ENDIF
 111    CONTINUE

 
C =====================================================================
C --- VII. CALCUL DE DLRDLA (NBMECXNBMEC) -----------------------------
C =====================================================================
      DO 112 K = 1, NBMECA
        DO 112 L = 1, NBMECA
          DLRDLA(K,L) = ZERO
 112      CONTINUE
 
      DO 113 K = 1, NBMECA
        KK = INDI(K)
        IF (KK .LT. 4) THEN
          DLRDLA(K,K) = -( UN-RC(K) )**DEUX /AD(K)
        ELSEIF (KK.EQ.4) THEN
          DLRDLA(K,K) = -( UN-RC(K) )**DEUX /CMON
        ENDIF
 113    CONTINUE
 
 
C =====================================================================
C --- VIII. CALCUL DE DLRDEVP (NBMECX1) -------------------------------
C =====================================================================
      DO 120 K = 1, NBMECA
        KK = INDI(K)
        IF (KK .LT. 4) THEN
          DLRDLE(K) = ZERO
        ELSEIF (KK .EQ. 4) THEN
          DLRDLE(K) = -DLAMBD(K)*BETA*( UN-RC(K) )**DEUX /CMON
        ENDIF
 120    CONTINUE

 
C =====================================================================
C --- IX. CALCUL DE DLEVPDS (1X6) -------------------------------------
C =====================================================================
       DO 130 I = 1, NDT
         DLEVDS(I) = ZERO
 130     CONTINUE
 
      DO 131 K = 1, NBMECA
        KK = INDI(K)
        IF (KK .EQ. 4) GOTO 1310
        CALL HUJPRJ (KK, SIGF, SIGD, COEF0, MUL)
        IF ((-Q(K)/PREF) .LE. TOLE) GOTO 131
        DLEVDS(NDT+1-KK) = DLEVDS(NDT+1-KK) +
     &  DLAMBD(K) * KSI(K)*COEF*SIGD(NDT+1-KK) /P(K)/Q(K)
        DO 132 I = 1, NDI
          IF (I .NE. KK) THEN
            DLEVDS(I) = DLEVDS(I) +
     &      DLAMBD(K)*KSI(K)*COEF*(SIGD(I) /P(K)/Q(K) - 
     &      D12*Q(K) /P(K)**DEUX)
          ENDIF
 132      CONTINUE
 131    CONTINUE
 1310 CONTINUE
 

C =====================================================================
C --- X. CALCUL DE DLEVPDEVP (1X1) ------------------------------------
C =====================================================================
       DLEVDE = UN
 
 
C =====================================================================
C --- XI. CALCUL DE DLEVPDR (1XNBMEC) ---------------------------------
C =====================================================================
      DO 151 K = 1, NBMECA
        KK = INDI(K)
        IF (KK .LT. 4) THEN
          DLEVDR(K) =
     &    DLAMBD(K)*COEF*DKSIDR(K)*(MDIL+Q(K)/P(K))
        ELSEIF (KK .EQ. 4) THEN
          DLEVDR(K) = ZERO
        ENDIF
 151    CONTINUE
  
 
C =====================================================================
C --- XII. CALCUL DE DLEVPDLA (1XNBMEC) -------------------------------
C =====================================================================
      DO 161 K = 1, NBMECA
        KK = INDI(K)
        IF (KK .LT. 4) THEN
          DLEVDL(K) = KSI(K)*COEF*(MDIL+Q(K)/P(K))
        ELSEIF (KK .EQ. 4) THEN
          DLEVDL(K) = UN
        ENDIF
 161    CONTINUE
   
 
C =====================================================================
C --- XIII. CALCUL DE DLFDS (NBMECX6) ---------------------------------
C =====================================================================
      DO 171 K = 1, NBMECA
        KK = INDI(K)
        CALL HUJDDD('DFDS  ', KK, MATER, INDI, YF,
     &              DFDS, DPSIDS, IRET)
        IF (IRET.EQ.1) GOTO 1000
        DO 171 I = 1, NDT
          DLFDS(K,I) = DFDS(I) /CCOND
 171      CONTINUE
 
 
C =====================================================================
C --- XIV. CALCUL DE DLFDR (NBMECXNBMEC) ------------------------------
C =====================================================================
      DO 180 K = 1, NBMECA
        DO 180 L = 1, NBMECA
          DLFDR(K,L) = ZERO
 180      CONTINUE
 
      DO 181 K = 1, NBMECA
        KK = INDI(K)
        IF (KK .LT. 4) THEN
          DLFDR(K,K) = M*P(K)*( UN-B*LOG(P(K)/PC) ) /CCOND
        ELSEIF (KK .EQ. 4) THEN
          DLFDR(K,K) = D*PC /CCOND
        ENDIF
181     CONTINUE


C =====================================================================
C --- XV. CALCUL DE DLFDEVP (NBMECX1) ---------------------------------
C =====================================================================
       DO 190 K = 1, NBMECA
         KK = INDI(K)
         IF (KK .LT. 4) THEN
           DLFDLE(K) = -M*B*P(K)*RC(K)*BETA /CCOND
         ELSEIF (KK .EQ. 4) THEN
           DLFDLE(K) = -RC(K)*D*PC*BETA /CCOND
         ENDIF
 190     CONTINUE


C =====================================================================
C --- XVI. CALCUL DE DLFDLA (NBMECXNBMEC) -----------------------------
C =====================================================================
       DO 200 K = 1, NBMECA
         DO 200 L = 1, NBMECA
           DLFDLA(K,L) = ZERO
 200       CONTINUE


C =====================================================================
C --- XVII. CALCUL DE LE (6x6) ---------------------------------------
C =====================================================================
C ---- XVII.1. CALCUL DE CDE = C*DEPSE
C                        6X1
C REMARQUE: ON A DEJA DEPSE CALCULE AU I.1.
       CALL LCPRMV (HOOKNL, DEPSE, CDE)
       
       DO 210 I = 1, NDT
         LE(I) = YF(I) - YD(I) - CDE(I)
 210     CONTINUE


C =====================================================================
C --- XVIII. CALCUL DE LEPV (1X1) -------------------------------------
C =====================================================================
        LEVP = YF(NDT+1) - YD(NDT+1)
        DO 220 K = 1, NBMECA
          KK = INDI(K)
          IF (KK .LT. 4) THEN
            LEVP = LEVP + COEF*DLAMBD(K)*KSI(K)*(MDIL+Q(K)/P(K))
          ELSEIF (KK .EQ. 4) THEN
            LEVP = LEVP + DLAMBD(K)
          ENDIF
 220      CONTINUE


C =====================================================================
C --- XIX. CALCUL DE LR (NBMECX1) -------------------------------------
C =====================================================================
        DO 230 K = 1, NBMECA
          KK = INDI(K)
          IF (KK .LT. 4) THEN
            LR(K) = YF(NDT+1+K) - YD(NDT+1+K) - 
     &              DLAMBD(K)/AD(K)*(UN-RC(K))**DEUX
          ELSEIF (KK .EQ. 4) THEN
            LR(K) = YF(NDT+1+K) - YD(NDT+1+K) - 
     &              DLAMBD(K)/CMON*(UN-RC(K))**DEUX
          ENDIF
 230      CONTINUE


C =====================================================================
C --- XX. CALCUL DE LF (NBMECX1) --------------------------------------
C =====================================================================
        DO 240 K = 1, NBMECA
          KK = INDI(K)
          IF (KK .LT. 4) THEN
            LF(K) = Q(K) + M*P(K)*RC(K)*( UN-B*LOG(P(K)/PC) )
          ELSEIF (KK .EQ. 4) THEN
            LF(K) = ABS(I1F) + RC(K)*D*PC
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
        DO 950 K = 1, NBMECA
           R(NDT+1+K)        = -LR(K)
           R(NDT+1+NBMECA+K) = -LF(K) /CCOND
 950       CONTINUE
 
 
C =====================================================================
C --- ASSEMBLAGE DE DRDY ----------------------------------------------
C =====================================================================
C DLEDDY
        CALL LCICMA (DLEDS,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,1)
        CALL LCICMA (DLEDEV,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,1,NDT+1)
        CALL LCICMA (DLEDR,6,4,NDT,NBMECA,1,1,DRDY,NMOD,NMOD,1,NDT+2)
        CALL LCICMA (DLEDLA,6,4,NDT,NBMECA,1,1,DRDY,NMOD,NMOD,1,
     &               NDT+2+NBMECA)
C DLEVPDDY        
        CALL LCICMA (DLEVDS,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,1)
        DRDY(NDT+1,NDT+1) = DLEVDE
        CALL LCICMA (DLEVDR,1,4,1,NBMECA,1,1,DRDY,NMOD,NMOD,
     &               NDT+1,NDT+2)
        CALL LCICMA (DLEVDL,1,4,1,NBMECA,1,1,DRDY,NMOD,NMOD,NDT+1,
     &               NDT+2+NBMECA)
C DLRDDY        
        CALL LCICMA (DLRDS,4,6,NBMECA,NDT,1,1,DRDY,NMOD,NMOD,NDT+2,1)
        CALL LCICMA (DLRDLE,4,1,NBMECA,1,1,1,DRDY,NMOD,NMOD,
     &               NDT+2,NDT+1)
        CALL LCICMA (DLRDR,4,4,NBMECA,NBMECA,1,1,DRDY,NMOD,NMOD,
     &               NDT+2,NDT+2)
        CALL LCICMA (DLRDLA,4,4,NBMECA,NBMECA,1,1,DRDY,NMOD,NMOD,NDT+2,
     &               NDT+2+NBMECA)
C DLFDDY
        CALL LCICMA (DLFDS,4,6,NBMECA,NDT,1,1,DRDY,NMOD,NMOD,
     &               NDT+2+NBMECA,1)
        CALL LCICMA (DLFDLE,4,1,NBMECA,1,1,1,DRDY,NMOD,NMOD,
     &               NDT+2+NBMECA,NDT+1)
        CALL LCICMA (DLFDR,4,4,NBMECA,NBMECA,1,1,DRDY,NMOD,NMOD,
     &               NDT+2+NBMECA,NDT+2)
        CALL LCICMA (DLFDLA,4,4,NBMECA,NBMECA,1,1,DRDY,NMOD,NMOD,
     &               NDT+2+NBMECA,NDT+2+NBMECA)
     
        GOTO 1000

 999    CONTINUE
        IF (DEBUG) WRITE (IFM,'(A)') 'HUJJID :: LOG(PK/PC) NON DEFINI'
        IRET=1
 1000   CONTINUE


C =====================================================================
C        CALL JEDEMA ()
C =====================================================================
        END

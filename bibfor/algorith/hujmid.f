        SUBROUTINE HUJMID ( MOD, CRIT, MATER, NVI, EPSD, DEPS,
     &  SIGD, SIGF, VIND, VINF, NOCONV, AREDEC, STOPNC, 
     &  NEGMUL, IRET, SUBD, LOOP, NDEC0, INDI,MECTRA)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/10/2010   AUTEUR FOUCAULT A.FOUCAULT 
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
C ---------------------------------------------------------------------
C INTEGRATION PLASTIQUE (MECANISMES ISOTROPE ET DEVIATOIRE) DE HUJEUX
C IN   MOD     :  MODELISATION
C      CRIT    :  CRITERES DE CONVERGENCE
C      MATER   :  COEFFICIENTS MATERIAU A T+DT
C      NVI     :  NB DE VARIABLES INTERNES
C      EPSD    :  DEFORMATIONS A T
C      DEPS    :  INCREMENTS DE DEFORMATION
C      SIGD    :  CONTRAINTE  A T
C      SIGF    :  PREDICTION ELASTIQUE OU PLASTIQUE
C      VIND    :  VARIABLES INTERNES  A T
C      AREDEC   =.TRUE.  ARRET DES DECOUPAGES
C      STOPNC   =.TRUE.  ARRET EN CAS DE NON CONVERGENCE
C      LOOP     =.TRUE.  PREDICTION PLASTIQUE DES CONTRAINTES      
C               =.FALSE. PREDICTION ELASTIQUE DES CONTRAINTES
C VAR  SIGF    :  CONTRAINTE  A T+DT
C      VINF    :  VARIABLES INTERNES  A T+DT
C      NOCONV   =.TRUE.  NON CONVERGENCE
C      NEGMUL   =.TRUE.  MULTIPLICATEUR NEGATIF
C      SUBD     =.TRUE. SUBDIVISION DU A (DR/R) < CRIT
C      NDEC0   :  NOMBRE D'INCREMENTS DE SUBDIVISION LIE A SUBD
C      IRET    :  CODE RETOUR
C   -------------------------------------------------------------------
      INTEGER   NDT, NDI, NVI, NR, NMOD, IRET, JJ, NBMECT
      INTEGER   I, J, K, KK, ITER, INDI(7), NDEC0, NDEC
      INTEGER   NITIMP, NBMECA, COMPT, MSUP(2)
      INTEGER   UMESS, IUNIFI, IFM, NIV
      INTEGER   ESSAI, ESSMAX, RESI, NMAX
      LOGICAL   DEBUG, NOCONV, AREDEC, STOPNC, NEGMUL(8), SUBD
      LOGICAL   LOOP, EULER
       
      COMMON    /TDIM/ NDT, NDI
      COMMON    /MESHUJ/ DEBUG
      
      PARAMETER (NMOD   = 18)
      PARAMETER (NITIMP = 200)
      PARAMETER (ESSMAX = 10)

      REAL*8    EPSD(6), DEPS(6), I1F, DEUX
      REAL*8    SIGD(6), SIGF(6), GD(6)
      REAL*8    VIND(*), VINF(*)
      REAL*8    CRIT(*), MATER(22,2)
      REAL*8    R(NMOD), DRDY(NMOD,NMOD)
      REAL*8    DDY(NMOD), DY(NMOD), YD(NMOD), YF(NMOD)
      REAL*8    ERR, ERR1, ERR2, DSIG(6)
      REAL*8    DET, ZERO, UN, RATIO, MAXI
      REAL*8    EVOL, KSI, ACYC, AMON, AD
      REAL*8    RDEC, PCO, BETA, CMON, CCYC, TOLE2
      
      REAL*8    RELAX(ESSMAX+1), ROTAGD(ESSMAX+1), NOR1(7), NOR2(7)
      REAL*8    ERIMP(NITIMP,4), PREF, DEV(3), PF, QF
      REAL*8    R8PREM, PTRAC, YE(NMOD), PSF, TOLE1, RTRAC

      REAL*8    PREDI0(6), SIGD0(6), DEPS0(6), VIND0(50), PROB(4)
      LOGICAL   AREDE0, STOPN0, LOOP0, PROX(4), PROBT, PROXC(4)
      LOGICAL   TRACTI, CYCL, NEGTRA, BNEWS(3), NODEF
      LOGICAL   NEGLAM(3), MECTRA, LTRY, MODIF, MTRAC
      
      CHARACTER*8 MOD

      DATA   ZERO, UN, DEUX, TOLE1 / 0.D0, 1.D0, 2.D0, 1.D-6/

C ====================================================================
C ---- PROPRIETES MATERIAU
C -------------------------
      PREF = MATER(8,2)
      RTRAC = ABS(PREF*1.D-6)
      TOLE2 = UN/(PREF**2)
C ----------------------------------------------------------------
C --- INITIALISATION VECTEUR GESTION MECANISMES TRACTION: PK-DP<=0
C ----------------------------------------------------------------
      DO 3 I = 1, 3
        BNEWS(I)  = .TRUE.
        DEV(I)    = ZERO
   3    CONTINUE

      MECTRA = .FALSE.
      MTRAC  = .FALSE.
C --------------------------------------------------
C ----  SAUVEGARDE DES GRANDEURS D ENTREE INITIALES
C --------------------------------------------------
C --- LIMITATION DU REDECOUPAGE DU A UNE EVOLUTION TROP RAPIDE 
C     DES VARIABLES INTERNES RDEV, RISO OU EPSVP
      NMAX = 3

      CALL LCEQVE(SIGF,PREDI0)
      CALL LCEQVE(SIGD,SIGD0)
      CALL LCEQVE(DEPS,DEPS0)
      CALL LCEQVN(NVI,VIND,VIND0)
      AREDE0 = AREDEC
      STOPN0 = STOPNC
      LOOP0   = LOOP
      COMPT   = 0
      PROBT   = .FALSE.
      DO 12 I = 1, 4
        PROX(I)  = .FALSE.
        PROXC(I) = .FALSE.
 12     CONTINUE

 1    CONTINUE
      IF(COMPT.GT.5)GOTO 9999
      COMPT = COMPT + 1 
      IF (DEBUG) WRITE(6,*)'DEBUT --- VINF =',(VINF(I),I=24,31)

C --------------------------------------------------
C ---> DIMENSION DU PROBLEME:
C      NR = NDT(SIG)+ 1(EVP)+ NBMECA(R)+ NBMEC(DLAMB)
C --------------------------------------------------

      UMESS  = IUNIFI('MESSAGE')
      CALL INFNIV(IFM,NIV)

      NOCONV = .FALSE.
      TRACTI = .FALSE.
      NODEF  = .FALSE.

      DO 2 K =1 ,4
        PROB(K) = ZERO
  2     CONTINUE

      PTRAC = MATER(21,2)

      NBMECA = 0
      DO 5 K = 1, 8
        IF (VIND(23+K) .EQ. UN) NBMECA = NBMECA + 1
        NEGMUL(K) = .FALSE.
 5      CONTINUE
      NR = NDT + 1 + 2*NBMECA
      
C ----------------------------
C ---> MISE A ZERO DES DATAS
C ----------------------------
      DO 10 I = 1, NMOD
        DDY(I) = ZERO
        DY(I)  = ZERO
        YD(I)  = ZERO
        YF(I)  = ZERO
        R(I)   = ZERO
 10     CONTINUE


C --------------------------------------------------
C ---> INITIALISATION DE YD = (SIGD, VIND, ZERO)
C --------------------------------------------------
      CALL LCEQVN (NDT, SIGD, YD)
      
      YD(NDT+1) = VIND(23)

      DO 15 K = 1, 7
        INDI(K)=0
 15     CONTINUE          

      KK = 1
      DO 16 K = 1, 8
        IF (VIND(23+K) .EQ. UN) THEN
        
          IF (K .NE. 4) THEN
            INDI(KK)            = K
            YD(NDT+1+KK)        = VIND(K)
            YD(NDT+1+NBMECA+KK) = ZERO
            KK                  = KK + 1
          ELSE
            INDI(NBMECA)        = K
            YD(NDT+1+NBMECA)    = VIND(K)
            YD(NDT+1+2*NBMECA)  = ZERO
          ENDIF  
          
        ENDIF
 16     CONTINUE

        IF (DEBUG) THEN
          WRITE(6,*)'INDI = ',(INDI(I),I=1,NBMECA)
          WRITE(6,*)'SIGD = ',(SIGD(I),I=1,NDT)
          WRITE(6,*)'SIGF = ',(SIGF(I),I=1,NDT)
          WRITE(6,*)'DEPS = ',(DEPS(I),I=1,NDT)
          WRITE(6,*)'VIND = ',(VIND(I),I=24,31)
          WRITE(6,*)'VIND = ',(VIND(I),I=21,22)
          WRITE(6,*)'LOOP = ',LOOP
          WRITE(6,*)
        ENDIF         

       I1F = (SIGF(1) + SIGF(2) + SIGF(3))/3.D0

C ------------------------------------------------------------
C --- APRES CHGT DE MECANISMES AU NIVEAU DE HUJACT
C     LOOP = .TRUE. --> RECUPERE ETAT DE CONTRAINTES CONVERGE
C     COMME PREDICTEUR D'EULER
C ------------------------------------------------------------

       IF (LOOP) THEN
         DO 17 I = 1, NDT
           DSIG(I) = SIGF(I) - SIGD(I)
  17       CONTINUE
       ELSE
         DO 18 I = 1, NDT
           DSIG(I) = ZERO
  18       CONTINUE
       ENDIF

C ------------------------------------------------------------------
C ---> INITIALISATION : DY : CALCUL DE LA SOLUTION D ESSAI INITIALE 
C      (SOLUTION EXPLICITE)
C ------------------------------------------------------------------
      CALL HUJIID (MOD, MATER, INDI, DEPS, I1F, YD, VIND, DY,
     &              LOOP, DSIG, BNEWS, MTRAC, IRET)

      IF(DEBUG)WRITE(6,*)'INDI =',(INDI(I),I=1,7)

C -------------------------------------------
C ---> MECANISMES DE TRACTION A CONSIDERER ?
C ---> SI OUI, REDIMENSIONNEMENT DE YF
C -------------------------------------------
      NBMECT = NBMECA
      DO 19 I = 1, 7
        IF (INDI(I).GT.8) THEN
          NR = NR + 1
          NBMECT = NBMECT + 1
        ENDIF
  19    CONTINUE
      IF(NBMECT.NE.NBMECA)MECTRA = .TRUE.
C ------------------------------------
C ---> INCREMENTATION DE YF = YD + DY
C ------------------------------------

      CALL LCSOVN (NR, YD, DY, YF)
      CALL LCEQVN (NMOD,YF,YE)

      IF (IRET.EQ.1) GOTO 9999

      IF (DEBUG) THEN
      WRITE(6,*)'NR = ',NR
      WRITE (IFM,'(A)') '------------------------------------------'
      WRITE(6,*)'INDI =',(INDI(I),I=1,NBMECT)
      WRITE (IFM,'(A)') '- SIXX - SIYY - SIZZ - SIXY - SIXZ - SIYZ -
     &EPSVP - R1 - R2 - R3 - R4 - DLA1 - DLA2 - DLA3 - DLA4 -'
      WRITE (IFM,1000) '  > ESSAI :: YF=',(YF(I),I=1,NR)
      
      ENDIF

C ----------------------------------------------------
C ---> RESTRICTION DES VALEURS DE SIGE A PREF**2 
C ---  SINON RENVOI EN ECHEC OU LES MECA DE TRACTION 
C ---  SONT DESACTIVES
C ----------------------------------------------------
      IF (NBMECA.NE.NBMECT) THEN
        DO 20 I = 1, NDI
          IF (ABS(YE(I)).GT.PREF**2.D0) NODEF = .TRUE.
  20      CONTINUE 
        IF (NODEF) THEN
          IRET = 1
          GOTO 9999
        ENDIF
      ENDIF


C----------------------------------------------------------
C ---> BOUCLE SUR LES ITERATIONS DE NEWTON
C----------------------------------------------------------
      ITER = 0
 100  CONTINUE

      ITER = ITER + 1
      DO 50 I = 1, NMOD
        R(I) = ZERO
        DO 60 J = 1, NMOD
          DRDY(I,J) = ZERO
 60       CONTINUE
 50     CONTINUE
C ---> CALCUL DU SECOND MEMBRE A T+DT : -R(DY)
C      ET CALCUL DU JACOBIEN DU SYSTEME A T+DT : DRDY(DY)

      CALL HUJJID (MOD, MATER, INDI, DEPS, PROX, PROXC,
     &             YD, YF, VIND, R, DRDY, IRET)

C -----------------------------------------------------
C ---> CRITERE DE PROXIMITE ENTRE SURFACES DEVIATOIRES
C --- PROB = UN ---> PROXIMITE CYCLIQUE/MONOTONE
C --- PROB = DEUX ---> PROXIMITE CYCLIQUES: FILS/PERE
C -----------------------------------------------------
      DO 11 I = 1, 3
        NEGLAM(I) = .FALSE.
        IF(PROX(I))THEN
          PROB(I) = UN
          PROBT   = .TRUE.
        ELSEIF(PROXC(I))THEN
          PROB(I) = DEUX
          PROBT   = .TRUE.
        ENDIF
 11     CONTINUE        


C ------------------------------------------------------------
C ---> SI ECHEC DANS LE CALCUL DE LA JACOBIENNE DR/DY
C ---  ON VERIFIE LES ETATS DE CONTRAINTES DE YF A L'ITERATION
C ---  DE CORRECTION PRECEDENTE. SI TRACTION IL Y A, ON TRAITE 
C ---  LE PB APRES L'ETIQUETTE 9999
C ------------------------------------------------------------
      IF (IRET.EQ.1) THEN
        IF (DEBUG) WRITE(6,'(A)')'HUJMID :: ERREUR DANS HUJJID'
        DO 255 I = 1, 3
          CALL HUJPRJ(I,YF,DEV,PF,QF)
          IF (((RTRAC+PF-PTRAC)/ABS(PREF)).GE.-R8PREM())THEN
            TRACTI = .TRUE.
          ENDIF
 255      CONTINUE          
        GOTO 9999
      ENDIF

C ---> RESOLUTION DU SYSTEME LINEAIRE : DRDY(DY).DDY = -R(DY)
      CALL LCEQVN (NR, R, DDY)
      CALL MGAUSS ('NCVP', DRDY, DDY, NMOD, NR, 1, DET, IRET)

C ----------------------------------------------------
C ---> SI ECHEC DANS LA RESOLUTION DU SYSTEME LINEAIRE
C ---  RENVOI A L'ETIQUETTE 9999
C ----------------------------------------------------
      IF (IRET.EQ.1) THEN 
        IF (DEBUG) WRITE(6,'(A)')'HUJMID :: ERREUR DANS MGAUSS'
        GOTO 9999
      ENDIF
      RELAX(1) = UN
      ESSAI    = 1

C ------------------------
C ---> 2) ERREUR = RESIDU
C ------------------------
C - TEST SUR LES VALEURS MAXIMUM DU RESIDU
C SI LES VALEURS SONT TROP GRANDES(>PREF**2), ON ASSUME L ECHEC
C DE L INTEGRATION LOCALE AVEC UN RETOUR A 9999
      DO 350 I = 1, NR
        IF(ABS(R(I)).GT.PREF**2)THEN
          IRET = 1
          GOTO 9999
        ENDIF
 350  CONTINUE       
      CALL LCNRVN (NR, R, ERR)
      IF(DEBUG)WRITE(6,*)'ERREUR =',ERR
      
      IF (ITER .LE. NITIMP) THEN
        ERIMP(ITER,1) = ERR
        ERIMP(ITER,2) = RELAX(ESSAI)
      ENDIF

C ----------------------------------------------------------------
C     SI ON N'A PAS ATTEINT LE NB MAX D'ITERATION: RESI_INTE_MAXI
C ----------------------------------------------------------------
      IF ( ITER .LE. INT(ABS(CRIT(1))) ) THEN

C -------------------------
C ----   CONVERVENCE   ----
C -------------------------
        IF ((ERR .LT. CRIT(3)).AND.(ITER.GT.1)) THEN
          GOTO 200

C ------------------------------------------------
C ----  NON CONVERVENCE : ITERATION SUIVANTE  ----
C ------------------------------------------------
        ELSE
C ------------------------------------
C --- REDIMENSIONNEMENT DES INCONNUES
C --- SIGMA * E0, R * E0/PREF
C ------------------------------------

          DO 41 I = 1, NDT
            DDY(I) = DDY(I)*MATER(1,1)
 41       CONTINUE
          DO 43 I = 1, NBMECA
            DDY(NDT+1+I) = DDY(NDT+1+I)*MATER(1,1)/
     &                     ABS(MATER(8,2))
 43       CONTINUE

C -----------------------------------
C --- MISE A JOUR DU VECTEUR SOLUTION
C -----------------------------------
          DO 42 I = 1, NR
            DY(I) = DY(I) + DDY(I)
            YF(I) = YD(I) + DY(I)
 42       CONTINUE
 
          IF (DEBUG) THEN
      
            WRITE(IFM,*)
            WRITE(IFM,1001) '  $$ ITER=',ITER
C        WRITE(IFM,1000) '     DDY=',(DDY(I),I=1,NR)
C        WRITE(IFM,1000) '     DY =',(DY(I),I=1,NR)
            WRITE(IFM,1000) '     YF =',(YF(I),I=1,NR)
            WRITE(IFM,1000) '     R  =',(R(I),I=1,NR)
        
          ENDIF
C -----------------------------------------------------
C --- CONTROLE DE L'ETAT DE CONTRAINTE PAR RAPPORT A LA 
C     LIMITE DE TRACTION A NE PAS DEPASSEE
C -----------------------------------------------------
          IF ((NBMECA.NE.NBMECT).AND.(NBMECA.EQ.0)) THEN
            IF(ERR.GT.1D5)THEN
              IRET = 1
              GOTO 9999
            ENDIF
            GOTO 45
          ELSE
            DO 44 I = 1, 3
              CALL HUJPRJ(I,YF,DEV,PF,QF)
              IF (((PF+RTRAC-PTRAC)/ABS(PREF)).GE.-R8PREM()) THEN
                DO 445 J = 1, NBMECA
                  IF((INDI(J).EQ.I).OR.(INDI(J).EQ.(I+4)))THEN
                    TRACTI = .TRUE.
                    GOTO 9999
                  ENDIF
 445            CONTINUE
              ENDIF
  44        CONTINUE        
          ENDIF

  45      CONTINUE  

          GOTO 100
        ENDIF

      ELSE

C ----------------------------------------------------
C ----  NON CONVERVENCE: ITERATION MAXI ATTEINTE  ----
C ----------------------------------------------------
         IF (AREDEC .AND. STOPNC) THEN
           CALL HUJNCV ('HUJMID', NITIMP, ITER, NDT, NVI, UMESS,
     &                 ERIMP, EPSD, DEPS, SIGD, VIND)
         ELSE
           IRET = 1
           GOTO 9999
         ENDIF
        
      ENDIF
 200  CONTINUE
 
C ---------------------------------------------------
C --- CONTROLE DES RESULTATS OBTENUS APRES RESOLUTION 
C     DU SYSTEME NON LINEAIRE LOCAL
C ---------------------------------------------------

C -------------------------------------------------
C ---- VERIFICATION DES MULTIPLICATEURS PLASTIQUES
C -------------------------------------------------
      MAXI = ABS(CRIT(3))
      DO 205 K = 1, NBMECT
        IF (YF(NDT+1+NBMECA+K).GT.MAXI)  MAXI = YF(NDT+1+NBMECA+K)
 205    CONTINUE

      NEGTRA = .FALSE.

      DO 210 K = 1, NBMECT
        RATIO = YF(NDT+1+NBMECA+K)/MAXI
        IF (RATIO .LT. (-TOLE1)) THEN 
          IF (INDI(K).LE.8) THEN
            NEGMUL(INDI(K)) = .TRUE.
          ELSE
C ----------------------------------------------
C ---> MECANISME DE TRACTION
C LAMBDA < 0 --> DESACTIVATION DU MECANISME POUR 
C LA PROCHAINE TENTATIVE D'INTEGRATION
C ----------------------------------------------
            BNEWS(INDI(K)-8) = .TRUE.              
            NEGTRA = .TRUE.
          ENDIF
        ENDIF
 210    CONTINUE    
      

C -------------------------------------------------------
C ---> MECANISME DE TRACTION
C LAMBDA < 0 --> TENTATIVE SUPPL D'INTEGRATION SI COMPT<5
C -------------------------------------------------------
      IF (NEGTRA) THEN
        IF (COMPT.GT.5) THEN
          NOCONV = .TRUE.
          GOTO 2000
        ELSEIF(NBMECA.EQ.0)THEN
          CALL LCEQVE(PREDI0, SIGF)
          CALL LCEQVE(DEPS0, DEPS)
          AREDEC = AREDE0
          STOPNC = STOPN0
          LOOP   = LOOP0
          IRET   = 0
          PROBT  = .FALSE.
          GOTO 1
        ELSE
          CALL LCEQVE(PREDI0, SIGF)
          CALL LCEQVE(SIGD0, SIGD)
          CALL LCEQVE(DEPS0, DEPS)
          CALL LCEQVN(NVI,VIND0, VIND)
          AREDEC = AREDE0
          STOPNC = STOPN0
          LOOP   = LOOP0
          IRET   = 0
          PROBT  = .FALSE.
          CALL LCEQVN(NVI,VIND, VINF)
          GOTO 1
        ENDIF
      ENDIF

C -------------------------------------------------------
C ---> MISE A JOUR DES CONTRAINTES ET VARIABLES INTERNES
C -------------------------------------------------------

      CALL LCEQVN (NDT, YF, SIGF)
      DO 46 I = 1, 3
        CALL HUJPRJ(I,SIGF,DEV,PF,QF)
C ------------------------------------------------------
C ---> CONTROLE QUE MECANISME DE TRACTION RESPECTE MEME
C      S'IL N'ETAIT PAS ACTIVE
C ------------------------------------------------------
        IF (((PF+DEUX*RTRAC-PTRAC)/ABS(PREF)).GT.TOLE2)THEN
          BNEWS(I) = .FALSE.
          TRACTI = .TRUE.
        ENDIF
  46  CONTINUE
      IF ((TRACTI).AND.(NBMECA.GT.0))THEN
        IRET = 1
        GOTO 9999
      ELSEIF(TRACTI)THEN
C --- SI IL N Y A QUE DES MECANISMES DE TRACTION ACTIFS
C --- ALORS ON DEMANDE DIRECTEMENT SON ACTIVATION SANS 
C --- REPASSER PAR L'ETAT INITIAL STANDARD
        CALL LCEQVE(DEPS0, DEPS)
C        CALL LCEQVE(PREDI0, SIGF)
        AREDEC = AREDE0
        STOPNC = STOPN0
        LOOP   = LOOP0
        IRET   = 0
        PROBT  = .FALSE.
        GOTO 1
      ENDIF
Caf 15/05/07 Debut
      VINF(23) = YF(NDT+1)
Caf 15/05/07 Fin        

C ----------------------------------------------
C ---> AFFECTATION DES RAYONS DE YF VERS VINF
C --- ON S'ASSURE QUE (R+>=R-) ET (R+CYC<=RMON)
C ----------------------------------------------
      DO 250 K = 1, NBMECA
        KK = INDI(K)
        IF (YF(NDT+1+K) .GT. VIND(KK)) THEN
          IF ((KK.GT.4).AND.(KK.LT.8)) THEN
            IF (YF(NDT+1+K).LE.VIND(KK-4)) THEN
              VINF(KK) = YF(NDT+1+K)
            ELSE
              VINF(KK) = VIND(KK-4)
            ENDIF
          ELSE
            VINF(KK) = YF(NDT+1+K)
          ENDIF
        ELSE 
          VINF(KK) = VIND(KK)
        ENDIF   
 250  CONTINUE
 
C -------------------------------------
C --- CONTROLE DE L'EVOLUTION DE R(K)
C     SI DR/R > TOLE ---> SUBD = .TRUE.
C -------------------------------------
      EVOL = 0.1D0
      SUBD = .FALSE.
      NDEC0 = 1
      DO 251 K= 1, NBMECA
        KK   = INDI(K)
        RATIO = (VINF(KK)-VIND(KK))/VIND(KK)
        IF (RATIO .GT. EVOL) THEN
          RDEC = (VINF(KK)-VIND(KK))/(EVOL*VIND(KK))
          NDEC = NINT(RDEC)
          IF (NDEC.LT.1) NDEC=1
          IF (NDEC.GT.NMAX) NDEC=NMAX
          NDEC0 = MAX(NDEC, NDEC0)
        ENDIF
 251  CONTINUE
      
C -------------------------------------------------
C --- CONTROLE DE L'EVOLUTION DE EPS_V^P
C     SI DEPS_V^P/EPS_V^P > TOLE ---> SUBD = .TRUE.
C -------------------------------------------------
      RATIO = ZERO
      IF (ABS(VIND(23)).GT.ABS(CRIT(3)))
     &   RATIO = (VINF(23)-VIND(23))/VIND(23)
      IF ((RATIO.GT.EVOL).AND.(ABS(VIND(23)).GT.CRIT(3))) THEN
        RDEC = (VINF(23)-VIND(23))/(EVOL*ABS(VIND(23)))
        NDEC = NINT(RDEC)
        IF (NDEC.LT.1) NDEC=1
        IF (NDEC.GT.NMAX) NDEC=NMAX
        NDEC0 = MAX(NDEC, NDEC0)
      ENDIF
      IF (NDEC0.GT.1) SUBD=.TRUE.
      GOTO 2000

C ----------------------------------------------------------
C ETIQUETTE 9999 ---> GESTION DES NON CONVERGENCES LOCALES
C                     LIMITEES A 5 TENTATIVES 
C ----------------------------------------------------------
9999  CONTINUE
      IF (COMPT.GT.5) THEN
        NOCONV = .TRUE.
C --- ON REGARDE SI L'ETAT INITIAL MATERIAU AVAIT SOLLICITE
C --- UN MECANISME DE TRACTION : ETAT INIT = SIGD0
        DO 49 I = 1, NDI
          CALL HUJPRJ(I, SIGD0, DEV, PF, QF)
          IF (((PF+DEUX*RTRAC-PTRAC)/ABS(PREF)).GT.-R8PREM()) THEN
            NOCONV=.FALSE.
            IRET = 0            
          ENDIF
 49     CONTINUE
        IF(.NOT.NOCONV)THEN
C --- EN POSANT NOCONV = .TRUE., ON CONDUIT L'ALGORITHME PRESENT
C --- DANS HUJRES A IMPOSER UN ETAT DE CONTRAINTES ISOTROPE COMMUN 
C --- AUX 3 SEUILS PLASTIQUES DE TRACTION 

          NOCONV=.TRUE.
          CALL LCEQVE(SIGD0,SIGD)
          CALL LCEQVE(SIGD0,SIGF)
          CALL LCEQVN(NVI,VIND0,VIND)
          CALL LCEQVN(NVI,VIND0,VINF)
        ENDIF
        IF(DEBUG)WRITE(6,*)'NOCONV =',NOCONV
        IF(DEBUG)WRITE(6,*)'MECTRA =',MECTRA
        GOTO 2000        
      ENDIF

      IF (PROBT) THEN
        IF (DEBUG) WRITE(6,'(A)')'HUJMID :: 9999 PROBT'
        CALL LCEQVE(PREDI0,SIGF)
        CALL LCEQVE(SIGD0,SIGD)
        CALL LCEQVE(DEPS0,DEPS)
        CALL LCEQVN(NVI,VIND0,VIND)
        AREDEC = AREDE0
        STOPNC = STOPN0
        LOOP   = LOOP0
        DO 252 I = 1, 3
          IF (PROB(I).EQ.UN) THEN
            VIND(I+4)    = MATER(18,2)
            VIND(23+I)   = UN
            VIND(27+I)   = ZERO
            VIND(4*I+5)  = ZERO
            VIND(4*I+6)  = ZERO
            VIND(4*I+7)  = ZERO
            VIND(4*I+8)  = ZERO
            VIND(5*I+31) = ZERO
            VIND(5*I+32) = ZERO
            VIND(5*I+33) = ZERO
            VIND(5*I+34) = ZERO
            VIND(5*I+35) = MATER(18,2)
          ELSEIF (PROB(I).EQ.DEUX) THEN
            VIND(27+I)   = ZERO
          ENDIF
 252    CONTINUE
        IRET = 0
        PROBT = .FALSE.
        CALL LCEQVN(NVI,VIND,VINF)
        GOTO 1
      ENDIF
      IF (TRACTI) THEN
        IF (DEBUG) WRITE(6,'(A)') 'HUJMID :: 9999 TRACTI'
        CALL LCEQVE(DEPS0,DEPS)
        CALL LCEQVN(NVI,VIND0,VIND)
        MODIF = .FALSE.
        DO 254 I = 1, NBMECT
          IF (YE(NDT+1+NBMECA+I).EQ.ZERO) THEN
            MODIF = .TRUE.
            IF (INDI(I).LE.8) THEN
              IF (INDI(I).LT.5) THEN
                IF ((ABS(VIND(4*INDI(I)+5)).GT.R8PREM()).OR.
     &              (ABS(VIND(4*INDI(I)+6)).GT.R8PREM())) THEN
                  VIND(23+INDI(I)) = -UN
                ELSE
                  VIND(23+INDI(I)) = ZERO
                ENDIF
              ELSE
                VIND(23+INDI(I)) = ZERO
              ENDIF
            ELSE
              BNEWS(INDI(I)-8) = .TRUE.
              NEGLAM(INDI(I)-8) = .TRUE.
            ENDIF
            TRACTI = .FALSE.
          ENDIF
 254    CONTINUE 
        IF(DEBUG)WRITE(6,*)'NEGLAM =',(NEGLAM(I),I=1,3)
        MTRAC = .FALSE.
        DO 253 I = 1, 3
C --- ON NE DOIT PAS REACTIVE UN MECANISME DE TRACTION QUI DONNE 
C     COMME PREDICTEUR UN MULTIPLICATEUR PLASTIQUE NEGATIF
          IF(.NOT.NEGLAM(I))THEN
            CALL HUJPRJ(I,YF,DEV,PF,QF)
C ----------------------------------------------------
C ---> ACTIVATION MECANISMES DE TRACTION NECESSAIRES
C ----------------------------------------------------
            IF(DEBUG)WRITE(6,*)'I=',I
            IF(DEBUG)WRITE(6,*)'PK =',PF
            IF (((PF+DEUX*RTRAC-PTRAC)/ABS(PREF)).GT.-R8PREM())THEN
              BNEWS(I) = .FALSE.
              IF(.NOT.MODIF)MTRAC = .TRUE.
            ENDIF
          ENDIF
 253    CONTINUE
        CALL LCEQVE(PREDI0,SIGF)
        CALL LCEQVE(SIGD0,SIGD)
        CALL LCEQVN(NVI,VIND,VINF)
        AREDEC = AREDE0
        STOPNC = STOPN0
        LOOP   = LOOP0
        IRET   = 0
        PROBT  = .FALSE.
        GOTO 1
      ENDIF

C-----------------------------------------------------------
C --- ESSAIS HEURISTIQUES POUR RELANCER LA RESOLUTION LOCALE
C-----------------------------------------------------------
      MAXI = ZERO
      RESI = 0
      DO 256 I = 1, NR
        IF (ABS(R(I)).GT.MAXI) THEN
          MAXI = ABS(R(I))
          RESI = I
        ENDIF
 256  CONTINUE
      CYCL = .FALSE.
      DO 260 I = 1, NBMECA
        IF ((INDI(I).GT.4) .AND. (INDI(I).LT.8)
     &       .AND. (VIND(INDI(I)).EQ.MATER(18,2))) THEN
          CYCL = .TRUE.
        ENDIF
 260  CONTINUE
      IF (DEBUG) WRITE(6,*) '9999 RESI:',RESI

C ---------------------------------------------------------------
C --- SI RESIDU LOCAL MAXI PORTE PAR RDEV_CYC => MECANISME RETIRE
C ---------------------------------------------------------------

      IF ((RESI.GT.7).AND.(RESI.LE.7+NBMECA)) THEN
        RESI = RESI - 7
        IF ((INDI(RESI).GT.4).AND.(INDI(RESI).LT.8)) THEN

C --- Y AVAIT IL UN MECANISME CYCLIQUE DEVIATOIRE DEJA DESACTIVE
C     DURANT CETTE TENTATIVE?
          MSUP(1) = 0
          MSUP(2) = 0
          J = 0
          DO 258 I=5,7
            IF(VIND(23+I).NE.VIND0(23+I))THEN
              IF((I.NE.INDI(RESI)).AND.(VIND(I).EQ.MATER(18,2)))THEN
                J = J+1
                MSUP(J) = I
              ENDIF
            ENDIF     
 258      CONTINUE
 
          CALL LCEQVE(PREDI0,SIGF)
          CALL LCEQVE(SIGD0,SIGD)
          CALL LCEQVE(DEPS0,DEPS)
          CALL LCEQVN(NVI,VIND0,VIND)
          AREDEC = AREDE0
          STOPNC = STOPN0
          LOOP   = LOOP0
          VIND(23+INDI(RESI)) = ZERO
          IF(J.NE.0)THEN
             DO 26 I=1,J
                VIND(23+MSUP(I)) = ZERO
  26         CONTINUE
          ENDIF

C --- EXISTE-T-IL UN MECANISME DEVIATOIRE AYANT LE MEME COMPORTEMENT 
C     QUE CELUI IDENTIFIE PRECEDEMMENT COMME POSANT PROBLEME ? 
          DO 257 I = 1, NBMECA
            IF ((INDI(I).GT.4).AND.(INDI(I).LT.8).AND.
     &         (((MAXI-ABS(R(7+I)))/TOLE1).LT.TOLE1).AND.
     &         (I.NE.RESI)) THEN
              VIND(23+INDI(I)) = ZERO
            ENDIF
 257      CONTINUE


          IRET = 0
          PROBT = .FALSE.
          CALL LCEQVN(NVI,VIND,VINF)
          GOTO 1
        ELSE
          NOCONV = .TRUE.
          IF(DEBUG)WRITE(6,*)'NOCONV2 =',NOCONV
          IF(DEBUG)WRITE(6,*)'MECTRA2 =',MECTRA
        ENDIF
      ENDIF

C ---------------------------------------------------------------
C --- SI MECA CYCLIQUE ALORS ILS SONT RETIRES
C ---------------------------------------------------------------

      IF (CYCL) THEN
        IF (DEBUG) WRITE(6,'(A)')'HUJMID :: 9999 CYCL'
        CALL LCEQVE(PREDI0,SIGF)
        CALL LCEQVE(SIGD0,SIGD)
        CALL LCEQVE(DEPS0,DEPS)
        CALL LCEQVN(NVI,VIND0,VIND)
        AREDEC = AREDE0
        STOPNC = STOPN0
        LOOP   = LOOP0
        DO 261 I = 1, NBMECA
          IF ((INDI(I).GT.4) .AND. (INDI(I).LT.8)
     &       .AND. (VIND(INDI(I)).EQ.MATER(18,2))) THEN
            VIND(23+INDI(I)) = ZERO
          ENDIF
 261    CONTINUE
        IRET = 0
        PROBT = .FALSE.
        CALL LCEQVN(NVI,VIND,VINF)
        GOTO 1
      ENDIF
      
C ---------------------------------------------------------------
C --- SI MECANISME TRACTION ACTIF => RETIRE DE MPOT
C ---------------------------------------------------------------

      IF (NBMECT.NE.NBMECA) THEN
        IF (DEBUG) WRITE(6,'(A)') '9999 FTRAC'
        CALL LCEQVE(PREDI0,SIGF)
        CALL LCEQVE(SIGD0,SIGD)
        CALL LCEQVE(DEPS0,DEPS)
        CALL LCEQVN(NVI,VIND0,VIND)
        AREDEC = AREDE0
        STOPNC = STOPN0
        LOOP   = LOOP0
        IRET   = 0
        DO 262 I = NBMECA+1, NBMECT
          IF (YE(NDT+1+NBMECA+I).EQ.ZERO) THEN
            BNEWS(INDI(I)-8) = .TRUE.              
          ENDIF
 262    CONTINUE 
        PROBT = .FALSE.
        CALL LCEQVN(NVI,VIND,VINF)
        GOTO 1
      ENDIF

C ---------------------------------------------------------------
C --- CONTROLE DU PREDICTEUR ELASTIQUE: YE(LAMBDA)
C ---------------------------------------------------------------

      CALL LCEQVE(PREDI0,SIGF)
      CALL LCEQVE(SIGD0,SIGD)
      CALL LCEQVE(DEPS0,DEPS)
      CALL LCEQVN(NVI,VIND0,VIND)
      AREDEC = AREDE0
      STOPNC = STOPN0
      LOOP   = LOOP0
      PROBT = .FALSE.
      EULER = .TRUE.
      DO 263 I = 1, NBMECA
        IF (YE(NDT+1+NBMECA+I).EQ.ZERO) THEN
          IF ((INDI(I).GT.4).AND.(INDI(I).LT.9)) THEN
            VIND(INDI(I)+23) = 0
            EULER = .FALSE.
          ELSEIF (INDI(I).LT.5) THEN
            IF ((ABS(VIND(4*INDI(I)+5)).GT.R8PREM()).OR.
     &       (ABS(VIND(4*INDI(I)+6)).GT.R8PREM())) THEN
              VIND(23+INDI(I)) = -UN
            ELSE
              VIND(23+INDI(I)) = ZERO
            ENDIF
            EULER = .FALSE.
          ENDIF
        ENDIF
 263  CONTINUE
      IF (.NOT.EULER) THEN
        CALL LCEQVN(NVI,VIND,VINF)              
        IRET = 0
        GOTO 1
      ENDIF

C ---------------------------------------------------------------
C --- DERNIER ESSAI: VALEUR DES CONTRAINTES PRE, DURANT ET POST
C ---------------------------------------------------------------
      LTRY = .FALSE.
      DO 264 I = 1, NDI
        CALL HUJPRJ(I, SIGD0, DEV, PF, QF)
        IF (((PF+DEUX*RTRAC-PTRAC)/ABS(PREF)).GT.-R8PREM())THEN
          NOCONV=.FALSE.
          IRET = 0     
          BNEWS(I) = .FALSE.       
          LTRY = .TRUE.
        ENDIF
        CALL HUJPRJ(I, YE, DEV, PF, QF)
        IF (((PF+DEUX*RTRAC-PTRAC)/ABS(PREF)).GT.-R8PREM())THEN
          NOCONV=.FALSE.
          IRET = 0     
          BNEWS(I) = .FALSE.       
          LTRY = .TRUE.
        ENDIF
        CALL HUJPRJ(I, YF, DEV, PF, QF)
        IF (((PF+DEUX*RTRAC-PTRAC)/ABS(PREF)).GT.-R8PREM())THEN
          NOCONV=.FALSE.
          IRET = 0     
          BNEWS(I) = .FALSE.       
          LTRY = .TRUE.
        ENDIF
        CALL HUJPRJ(I, PREDI0, DEV, PF, QF)
        IF (((PF+RTRAC-PTRAC)/ABS(PREF)).GT.-R8PREM())THEN
          NOCONV=.FALSE.
          IRET = 0     
          BNEWS(I) = .FALSE.       
          LTRY = .TRUE.
        ENDIF
 264  CONTINUE

      IF(LTRY)THEN
        CALL LCEQVN(NVI,VIND,VINF)              
        IRET = 0
        GOTO 1              
      ELSE
        NOCONV = .TRUE.
      ENDIF
      
       
 1000 FORMAT(A,15(1X,E12.5))
 1001 FORMAT(A,2(I3))

 2000 CONTINUE

      IF (DEBUG) WRITE(6,*)'HUJMID --- VINF =',(VINF(I),I=24,31)
      IF(DEBUG)WRITE(6,*)'IRET - HUJMID =',IRET
      END

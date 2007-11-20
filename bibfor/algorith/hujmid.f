        SUBROUTINE HUJMID ( MOD, CRIT, MATER, NVI, EPSD, DEPS,
     &  SIGD, SIGF, VIND, VINF, NOCONV, AREDEC, STOPNC, 
     &  NEGMUL, NITER, EPSCON, IRET, SUBD, LOOP, NDEC0 )
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/11/2007   AUTEUR KHAM M.KHAM 
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
C      NITER   :  NOMBRE D ITERATIONS A CONVERGENCE
C      EPSCON  :  VALEUR ERR FINALE
C      SUBD     =.TRUE. SUBDIVISION DU A (DR/R) < CRIT
C      NDEC0   :  NOMBRE D'INCREMENTS DE SUBDIVISION LIE A SUBD
C      IRET    :  CODE RETOUR
C   -------------------------------------------------------------------
        INTEGER   NDT, NDI, NVI, NR, NMOD, NITER, IRET
        INTEGER   I, J, K, KK, ITER, INDI(4), NDEC0, NDEC
        INTEGER   NITIMP, NBMECA
        INTEGER   UMESS, IUNIFI, IFM, NIV
        INTEGER   ESSAI, ESSMAX
        LOGICAL   DEBUG, NOCONV, AREDEC, STOPNC, NEGMUL(8), SUBD
        LOGICAL   LOOP
         
        COMMON    /TDIM/ NDT, NDI
        COMMON    /MESHUJ/ DEBUG
        
        PARAMETER (NMOD   = 15)
        PARAMETER (NITIMP = 200)
        PARAMETER (ESSMAX = 10)

        REAL*8    EPSD(6), DEPS(6), I1F
        REAL*8    SIGD(6), SIGF(6), GD(6)
        REAL*8    VIND(*), VINF(*), EPSCON
        REAL*8    CRIT(*), MATER(22,2)
        REAL*8    R(NMOD), DRDY(NMOD,NMOD)
        REAL*8    DDY(NMOD), DY(NMOD), YD(NMOD), YF(NMOD)
        REAL*8    ERR, ERR1, ERR2, SIGNE(4), DSIG(6)
        REAL*8    DET, ZERO, UN, RATIO
        REAL*8    EVOL, KSI, ACYC, AMON, AD
        REAL*8    RDEC, PCO, BETA, CMON, CCYC
        
        REAL*8    RELAX(ESSMAX+1), ROTAGD(ESSMAX+1), NOR1(7), NOR2(7)
        REAL*8    ERIMP(NITIMP,4), YOUNG, PREF
       
        CHARACTER*8 MOD

        DATA   ZERO, UN / 0.D0, 1.D0/

C ====================================================================

        PREF   = MATER(8,2)
        YOUNG  = MATER(1,1) 
        
        
C ---> DIMENSION DU PROBLEME:
C      NR = NDT(SIG)+ 1(EVP)+ NBMECA(R)+ NBMEC(DLAMB)
        UMESS  = IUNIFI('MESSAGE')
        CALL INFNIV(IFM,NIV)
        NOCONV = .FALSE.

Caf 30/04/07 debut
        NBMECA = 0
        DO 5 K = 1, 8
          IF(VIND(23+K) .EQ. UN) THEN
            NBMECA = NBMECA + 1
          ENDIF
          NEGMUL(K) = .FALSE.
 5      CONTINUE
        NR = NDT + 1 + 2*NBMECA
        
Caf 30/04/07 fin

C ---> MISE A ZERO DES DATAS
        DO 10 I = 1, NR
          DDY(I) = ZERO
          DY(I)  = ZERO
          YD(I)  = ZERO
          YF(I)  = ZERO
 10       CONTINUE


C ---> INITIALISATION DE YD = (SIGD, VIND, ZERO)
        CALL LCEQVN (NDT, SIGD, YD)
        
Caf 30/04/07 debut
        YD(NDT+1) = VIND(23)

        DO 15 K = 1, 4
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
 16       CONTINUE
          
C         WRITE(6,'(A,4(1X,I3))')'INDI =',(INDI(I),I=1,4)
 
Caf 30/04/07 fin
         I1F = (SIGF(1) + SIGF(2) + SIGF(3))/3
         IF(LOOP)THEN
           DO 17 I = 1, NDT
             DSIG(I) = SIGF(I) - SIGD(I)
  17       CONTINUE
         ELSE
           DO 18 I = 1, NDT
             DSIG(I) = ZERO
  18       CONTINUE
         ENDIF

C ---> INITIALISATION : DY : CALCUL DE LA SOLUTION D ESSAI INITIALE 
C      (SOLUTION EXPLICITE)
C       WRITE(6,*)'DSIG =',(DSIG(I),I=1,6)
        CALL HUJIID (MOD, MATER, INDI, DEPS, I1F, YD, VIND, DY,
     &                LOOP, DSIG)
C ---> INCREMENTATION DE YF = YD + DY
        CALL LCSOVN (NR, YD, DY, YF)

        IF (DEBUG) THEN

        WRITE (IFM,'(A)') '------------------------------------------'
        WRITE (IFM,'(A)') '- SIXX - SIYY - SIZZ - SIXY - SIXZ - SIYZ -
     &  EPSVP - R1 - R2 - R3 - R4 - DLA1 - DLA2 - DLA3 - DLA4 -'
        WRITE (IFM,1000) '  > ESSAI :: YF=',(YF(I),I=1,NR)
      
        ENDIF
C       WRITE (IFM,1000) '  > ESSAI :: YF=',(YF(I),I=1,NR)
C----------------------------------------------------------
C ---> BOUCLE SUR LES ITERATIONS DE NEWTON
C----------------------------------------------------------
        ITER = 0
 100    CONTINUE

        ITER = ITER + 1
C        WRITE(6,*)'ITER =',ITER
        DO 50 I = 1, NR
          R(I) = ZERO
          DO 60 J = 1, NR
            DRDY(I,J) = ZERO
 60         CONTINUE
 50       CONTINUE
C ---> CALCUL DU SECOND MEMBRE A T+DT : -R(DY)
C      CALCUL DE SIGNE(S:DEPSDP)
C      ET CALCUL DU JACOBIEN DU SYSTEME A T+DT : DRDY(DY)

        CALL HUJJID (MOD, MATER, INDI, DEPS, YD, YF, VIND,
     &               R, SIGNE, DRDY, IRET)
        IF (IRET.EQ.1) GOTO 9999

C ---> RESOLUTION DU SYSTEME LINEAIRE : DRDY(DY).DDY = -R(DY)
        CALL LCEQVN (NR, R, DDY)
        CALL MGAUSS ('NFVP', DRDY, DDY, NMOD, NR, 1, DET, IRET)
        IF (IRET.EQ.1) THEN
          WRITE(6,'(A,I3)')'MGAUSS --- IRET =',IRET
          GOTO 9999
        ENDIF
        RELAX(1) = UN
        ESSAI    = 1

        DO 42 I = 1, NR
          DY(I) = DY(I) + DDY(I)
          YF(I) = YD(I) + DY(I)
 42     CONTINUE
 
        IF (DEBUG) THEN
        
          WRITE(IFM,*)
          WRITE(IFM,1001) '  $$ ITER=',ITER
          WRITE(IFM,1000) '     DDY=',(DDY(I),I=1,NR)
          WRITE(IFM,1000) '     DY =',(DY(I),I=1,NR)
          WRITE(IFM,1000) '     YF =',(YF(I),I=1,NR)
          
        ENDIF
C       WRITE(IFM,1000) '     YF =',(YF(I),I=1,NR)
C       WRITE(IFM,1000) '     R =',(R(I),I=1,NR)  
C ---> 2) ERREUR = RESIDU
        CALL LCNRVN (NR, R, ERR)
        
C       WRITE(6,'(A,E12.5)')'ERR =',ERR
         
        IF (ITER .LE. NITIMP) THEN
          ERIMP(ITER,1) = ERR
          ERIMP(ITER,2) = RELAX(ESSAI)
        ENDIF

        IF( ITER .LE. INT(ABS(CRIT(1))) ) THEN


C ----   CONVERVENCE   ----
           IF (ERR .LT. CRIT(3)) THEN
             GOTO 200


C ----  NON CONVERVENCE : ITERATION SUIVANTE  ----
           ELSE
             GOTO 100
           ENDIF

        ELSE


C ----  NON CONVERVENCE: ITERATION MAXI ATTEINTE  ----
           IF (AREDEC .AND. STOPNC) THEN
             CALL HUJNCV ('HUJMID', NITIMP, ITER, NDT, NVI, UMESS,
     &                   ERIMP, EPSD, DEPS, SIGD, VIND)
           ELSE
             IRET = 1
             WRITE(6,'(A,I3)')'NB ITERATIONS MAXI --- IRET =',IRET
             GOTO 9999
           ENDIF
          
        ENDIF
 200    CONTINUE
 

C ---- VERIFICATION DES MULTIPLICATEURS PLASTIQUES
        DO 210 K = 1, NBMECA
C          WRITE(6,'(A,15(1X,E12.5))')'YF =',(YF(I),I=1,15)
          RATIO = YF(NDT+1+NBMECA+K)/ABS(PREF/YOUNG)
          IF (RATIO .LT. ZERO) 
     &    NEGMUL(INDI(K)) = .TRUE.
 210    CONTINUE    
        
        NITER  = ITER
        EPSCON = ERR
        
        
C ---> MISE A JOUR DES CONTRAINTES ET VARIABLES INTERNES
        CALL LCEQVN (NDT, YF, SIGF)
C        IF(NBMECA.EQ.1)WRITE(6,'(A,E16.9)')'HUJMID --- LAMBDA =',YF(9)
Caf 15/05/07 Debut
        VINF(23) = YF(NDT+1)
Caf 15/05/07 Fin        
        DO 250 K = 1, NBMECA
          KK       = INDI(K)
          IF (YF(NDT+1+K) .GT. VIND(KK)) THEN
            VINF(KK) = YF(NDT+1+K)
          ELSE 
            VINF(KK) = VIND(KK)
          ENDIF   
 250    CONTINUE
 
Caf 12/06/07 Debut
C --- CONTROLE DE L'EVOLUTION DE R(K)
C     SI DR/R > TOLE ---> SUBD = .TRUE.
        EVOL = 1.D-2
        SUBD = .FALSE.
        NDEC0 = 1
        IF(INT(CRIT(5)).LT.-1)THEN
          DO 251 K= 1, NBMECA
            KK    = INDI(K)
            RATIO = (VINF(KK)-VIND(KK))/VIND(KK)
            IF (RATIO .GT. EVOL)THEN
              IF((INDI(K).LT.4).OR.((INDI(K).GT.4)
     &           .AND.(INDI(K).LT.8)))THEN
                ACYC   = MATER(9,2)
                AMON   = MATER(10,2)
                CALL HUJKSI ('KSI   ',MATER,YF(NDT+1+K),KSI,IRET)
                IF(IRET.EQ.1)WRITE(6,'(A,E16.9)')'HUJKSI --- R =',
     &                                           YF(NDT+1+K)
                IF(INDI(K).LT.4)THEN
                  AD     = ACYC+KSI*(AMON-ACYC)
                ELSE
                  AD     = 2.D0*(ACYC+KSI*(AMON-ACYC))
                ENDIF
                RDEC   = ((UN-YF(NDT+1+K))**2)/AD/YF(NDT+1+K)/EVOL*
     &                    YF(NDT+1+NBMECA+K)
                IF(RDEC.GT.-CRIT(5))RDEC=-CRIT(5)
                NDEC   = NINT(RDEC)
                IF(NDEC.LT.1)NDEC=1
              
              ELSEIF(INDI(K).EQ.4)THEN
                CMON   = MATER(12,2)
                BETA   = MATER(2,2)
                PREF   = MATER(8,2)
                PCO    = MATER(7,2)
                CMON   = CMON*PCO*EXP(-BETA*YF(NDT+1))
                RDEC   = ((UN-YF(NDT+1+K))**2)/CMON/YF(NDT+1+K)/EVOL*
     &                    YF(NDT+1+NBMECA+K)
                IF(RDEC.GT.-CRIT(5))RDEC=-CRIT(5)
                NDEC   = NINT(RDEC)
                IF(NDEC.LT.1)NDEC=1
              
              ELSE
                CCYC   = MATER(11,2)
                BETA   = MATER(2,2)
                PREF   = MATER(8,2)
                PCO    = MATER(7,2)
                CCYC   = CCYC*PCO*EXP(-BETA*YF(NDT+1))
                RDEC   = ((UN-YF(NDT+1+K))**2)/CCYC/YF(NDT+1+K)/EVOL*
     &                    YF(NDT+1+NBMECA+K)
                IF(RDEC.GT.-CRIT(5))RDEC=-CRIT(5)
                NDEC   = NINT(RDEC)
                IF(NDEC.LT.1)NDEC=1
              
              ENDIF
              NDEC0 = MAX(NDEC, NDEC0)
            ENDIF
            IF(NDEC0.GT.1)SUBD=.TRUE.
 251      CONTINUE
        ENDIF
Caf 12/06/07 fin 
        
        GOTO 2000

 9999   CONTINUE
        NOCONV = .TRUE.
 
 1000   FORMAT(A,15(1X,E12.5))
 1001   FORMAT(A,2(I3))

 2000   CONTINUE

        END

        SUBROUTINE HUJMID ( MOD, CRIT, MATER, NVI, EPSD, DEPS,
     &  SIGD, SIGF, VIND, VINF, NOCONV, AREDEC, STOPNC, 
     &  NEGMUL, NITER, EPSCON, IRET )
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/02/2007   AUTEUR KHAM M.KHAM 
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
C      VIND    :  VARIABLES INTERNES  A T
C      AREDEC	=.TRUE.  ARRET DES DECOUPAGES
C      STOPNC	=.TRUE.  ARRET EN CAS DE NON CONVERGENCE
C VAR  SIGF    :  CONTRAINTE  A T+DT
C      VINF    :  VARIABLES INTERNES  A T+DT
C      NOCONV	=.TRUE.  NON CONVERGENCE
C      NEGMUL	=.TRUE.  MULTIPLICATEUR NEGATIF
C      NITER   :  NOMBRE D ITERATIONS A CONVERGENCE
C      EPSCON  :  VALEUR ERR FINALE
C      IRET    :  CODE RETOUR
C   -------------------------------------------------------------------
        INTEGER   NDT, NDI, NVI, NR, NMOD, NITER, IRET
        INTEGER   I, J, K, KK, ITER, INDI(4)
        INTEGER   NITIMP, NBMECA
        INTEGER   UMESS, IUNIFI
        INTEGER   ESSAI, ESSMAX

        COMMON    /TDIM/ NDT, NDI

        PARAMETER (NMOD   = 15)
        PARAMETER (NITIMP = 200)
        PARAMETER (ESSMAX = 10)

        REAL*8    EPSD(6), DEPS(6)
        REAL*8    SIGD(6), SIGF(6), GD(6)
        REAL*8    VIND(*), VINF(*), EPSCON
        REAL*8    CRIT(*), MATER(20,2)
        REAL*8    R(NMOD), DRDY(NMOD,NMOD)
        REAL*8    DDY(NMOD), DY(NMOD), YD(NMOD), YF(NMOD)
        REAL*8    ERR, ERR1, ERR2, SIGNE(4)
        REAL*8    DET, ZERO, UN
C    SI ABS(COS_NORMALES) < TOLROT RELAX = RELAX*DECREL
C        REAL*8    TOLROT, DECREL
        REAL*8    RELAX(ESSMAX+1), ROTAGD(ESSMAX+1),
     &            NOR1(7), NOR2(7)
        REAL*8    ERIMP(NITIMP,4)

C         PARAMETER (TOLROT = 0.8D0)
C         PARAMETER (DECREL = 0.5D0)

        LOGICAL   NOCONV, AREDEC, STOPNC, NEGMUL(4)
        LOGICAL DEVNU1, DEVNU2, TRA1, TRA2

        CHARACTER*8 MOD

        DATA   ZERO, UN / 0.D0, 1.D0 /


C ---> DIMENSION DU PROBLEME:
C      NR = NDT(SIG)+ 1(EVP)+ NBMECA(R)+ NBMEC(DLAMB)
        UMESS  = IUNIFI('MESSAGE')
        NOCONV = .FALSE.

        NBMECA = 0
        DO 5 K = 1, 4
          NBMECA = NBMECA + INT(VIND(5+K))
 5        CONTINUE
        NR = NDT + 1 + 2*NBMECA


C ---> MISE A ZERO DES DATAS
        DO 10 I = 1, NR
          DDY(I) = ZERO
          DY(I)  = ZERO
          YD(I)  = ZERO
          YF(I)  = ZERO
 10       CONTINUE


C ---> INITIALISATION DE YD = (SIGD, VIND, ZERO)
        CALL LCEQVN (NDT, SIGD, YD)

        YD(NDT+1) = VIND(5)
                
        KK = 1
        DO 16 K = 1, 4
          INDI(K) = 0
          IF (VIND(5+K) .EQ. UN) THEN
            INDI(KK)            = K
            YD(NDT+1+KK)        = VIND(K)
            YD(NDT+1+NBMECA+KK) = ZERO
            KK                  = KK + 1
          ENDIF
 16       CONTINUE


C ---> INITIALISATION : DY : CALCUL DE LA SOLUTION D ESSAI INITIALE 
C      (SOLUTION EXPLICITE)
         CALL HUJIID (MOD, MATER, INDI, DEPS, YD, DY)


C ---> INCREMENTATION DE YF = YD + DY
        CALL LCSOVN (NR, YD, DY, YF)


C----------------------------------------------------------
C ---> BOUCLE SUR LES ITERATIONS DE NEWTON
C----------------------------------------------------------
        ITER = 0
 100    CONTINUE

        ITER = ITER + 1

        DO 50 I = 1, NR
          R(I) = ZERO
          DO 60 J = 1, NR
            DRDY(I,J) = ZERO
 60         CONTINUE
 50       CONTINUE


C ---> CALCUL DU SECOND MEMBRE A T+DT : -R(DY)
C      CALCUL DE SIGNE(S:DEPSDP)
C      ET CALCUL DU JACOBIEN DU SYSTEME A T+DT : DRDY(DY)
        CALL HUJJID (MOD, MATER, INDI, DEPS, YD, YF,
     &               R, SIGNE, DRDY, IRET)
        IF (IRET.EQ.1.D0) GOTO 9999


C ---> RESOLUTION DU SYSTEME LINEAIRE : DRDY(DY).DDY = -R(DY)
        CALL LCEQVN (NR, R, DDY)
        CALL MGAUSS ('NFVP', DRDY, DDY, NMOD, NR, 1, DET, IRET)
        IF (IRET.EQ.1) GOTO 9999
        RELAX(1) = UN
        ESSAI    = 1

        DO 42 I = 1, NR
          DY(I) = DY(I) + DDY(I)
          YF(I) = YD(I) + DY(I)
   42     CONTINUE


C ---> VERIFICATION DE LA CONVERGENCE:
C        1) ERREUR = !!DDY!!/!!DY!! < TOLER
C         CALL LCNRVN(NR,DDY,ERR1)
C         CALL LCNRVN(NR,DY,ERR2)
C         IF(ERR2.EQ.0.D0) THEN
C             ERR = ERR1
C           ELSE
C             ERR = ERR1 /ERR2
C           ENDIF


C ---> 2) ERREUR = RESIDU
        CALL LCNRVN (NR, R, ERR)
         
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
             NOCONV = .TRUE.
           ENDIF
          
        ENDIF

 200    CONTINUE
 
C ---- VERIFICATION DES MULTIPLICATEURS PLASTIQUES
        DO 210 K = 1, NBMECA
          IF (YF(NDT+1+NBMECA+K) .LT. ZERO) 
     &    NEGMUL(INDI(K)) = .TRUE.
210       CONTINUE
        
        NITER  = ITER
        EPSCON = ERR
        
        
C ---> MISE A JOUR DES CONTRAINTES ET VARIABLES INTERNES
        CALL LCEQVN (NDT, YF, SIGF)
        VINF(5) = YF(NDT+1)
        DO 250 K = 1, NBMECA
          KK       = INDI(K)
          VINF(KK) = YF(NDT+1+K)
 250      CONTINUE

 9999   CONTINUE
        END

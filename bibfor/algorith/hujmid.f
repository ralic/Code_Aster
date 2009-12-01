        SUBROUTINE HUJMID ( MOD, CRIT, MATER, NVI, EPSD, DEPS,
     &  SIGD, SIGF, VIND, VINF, NOCONV, AREDEC, STOPNC, 
     &  NEGMUL, IRET, SUBD, LOOP, NDEC0)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/02/2009   AUTEUR FOUCAULT A.FOUCAULT 
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
        INTEGER   NDT, NDI, NVI, NR, NMOD, IRET, JJ
        INTEGER   I, J, K, KK, ITER, INDI(4), NDEC0, NDEC
        INTEGER   NITIMP, NBMECA, COMPT
        INTEGER   UMESS, IUNIFI, IFM, NIV
        INTEGER   ESSAI, ESSMAX, RESI
        LOGICAL   DEBUG, NOCONV, AREDEC, STOPNC, NEGMUL(8), SUBD
        LOGICAL   LOOP
         
        COMMON    /TDIM/ NDT, NDI
        COMMON    /MESHUJ/ DEBUG
        
        PARAMETER (NMOD   = 15)
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
        REAL*8    RDEC, PCO, BETA, CMON, CCYC
        
        REAL*8    RELAX(ESSMAX+1), ROTAGD(ESSMAX+1), NOR1(7), NOR2(7)
        REAL*8    ERIMP(NITIMP,4)
        REAL*8    R8PREM, PTRAC, YE(NMOD), PSF

        REAL*8    PREDI0(6), SIGD0(6), DEPS0(6), VIND0(50), PROB(4)
        LOGICAL   AREDE0, STOPN0, LOOP0, PROX(4), PROBT, PROXC(4)
        LOGICAL   TRACTI, CYCL
       
        CHARACTER*8 MOD

        DATA   ZERO, UN, DEUX / 0.D0, 1.D0, 2.D0/

C ====================================================================
C ----  SAUVEGARDE DES GRANDEURS D ENTREE INITIALES

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

 1      CONTINUE
        COMPT = COMPT + 1 
C ---> DIMENSION DU PROBLEME:
C      NR = NDT(SIG)+ 1(EVP)+ NBMECA(R)+ NBMEC(DLAMB)
        UMESS  = IUNIFI('MESSAGE')
        CALL INFNIV(IFM,NIV)
        NOCONV   = .FALSE.
        TRACTI = .FALSE.
        DO 2 K =1 ,4
          PROB(K) = ZERO
  2     CONTINUE

        PTRAC = MATER(21,2)

        NBMECA = 0
        DO 5 K = 1, 8
          IF(VIND(23+K) .EQ. UN) THEN
            NBMECA = NBMECA + 1
          ENDIF
          NEGMUL(K) = .FALSE.
 5      CONTINUE
        NR = NDT + 1 + 2*NBMECA
        
C ---> MISE A ZERO DES DATAS
        DO 10 I = 1, NMOD
          DDY(I) = ZERO
          DY(I)  = ZERO
          YD(I)  = ZERO
          YF(I)  = ZERO
 10       CONTINUE


C ---> INITIALISATION DE YD = (SIGD, VIND, ZERO)
        CALL LCEQVN (NDT, SIGD, YD)
        
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

          IF(DEBUG)THEN
            WRITE(6,*)'INDI = ',(INDI(I),I=1,NBMECA)
            WRITE(6,*)'SIGD = ',(SIGD(I),I=1,NDT)
            WRITE(6,*)'VIND = ',(VIND(I),I=1,50)
            WRITE(6,*)'LOOP = ',LOOP
            WRITE(6,*)
          ENDIF         

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
        CALL HUJIID (MOD, MATER, INDI, DEPS, I1F, YD, VIND, DY,
     &                LOOP, DSIG, IRET)

C ---> INCREMENTATION DE YF = YD + DY
        CALL LCSOVN (NR, YD, DY, YF)
        CALL LCEQVN(NMOD,YF,YE)

        IF(IRET.EQ.1)GOTO 9999
        
        IF (DEBUG) THEN

        WRITE (IFM,'(A)') '------------------------------------------'
        WRITE (IFM,'(A)') '- SIXX - SIYY - SIZZ - SIXY - SIXZ - SIYZ -
     &  EPSVP - R1 - R2 - R3 - R4 - DLA1 - DLA2 - DLA3 - DLA4 -'
        WRITE (IFM,1000) '  > ESSAI :: YF=',(YF(I),I=1,NR)
      
        ENDIF

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
C      ET CALCUL DU JACOBIEN DU SYSTEME A T+DT : DRDY(DY)

        CALL HUJJID (MOD, MATER, INDI, DEPS, PROX, PROXC,
     &               YD, YF, VIND, R, DRDY, IRET)

        DO 11 I = 1, 3
          IF(PROX(I))THEN
            PROB(I) = UN
            PROBT   = .TRUE.
          ELSEIF(PROXC(I))THEN
            PROB(I) = DEUX
            PROBT   = .TRUE.
          ENDIF
 11     CONTINUE        

        IF (IRET.EQ.1) THEN
          DO 255 I = 1, NDI
            IF(YF(I).GT.PTRAC)THEN
              TRACTI = .TRUE.
            ENDIF
 255      CONTINUE          
          GOTO 9999
        ENDIF

C ---> RESOLUTION DU SYSTEME LINEAIRE : DRDY(DY).DDY = -R(DY)
        CALL LCEQVN (NR, R, DDY)
        CALL MGAUSS ('NCVP', DRDY, DDY, NMOD, NR, 1, DET, IRET)

        IF (IRET.EQ.1) GOTO 9999
        RELAX(1) = UN
        ESSAI    = 1

C --- DIMENSIONNEMENT DES CONTRAINTES
        DO 41 I = 1, NDT
          DDY(I) = DDY(I)*MATER(1,1)
 41     CONTINUE
        DO 43 I = 1, NBMECA
          DDY(NDT+1+I) = DDY(NDT+1+I)*MATER(1,1)/MATER(8,2)
 43     CONTINUE

C --- MISE A JOUR DU VECTEUR SOLUTION
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
          WRITE(IFM,1000) '     R  =',(R(I),I=1,NR)
          
        ENDIF

         DO 44 I = 1, NDI
           IF(YF(I).GT.PTRAC)THEN
             TRACTI = .TRUE.
             GOTO 9999
           ENDIF
 44      CONTINUE

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
             IRET = 1

             GOTO 9999
           ENDIF
          
        ENDIF
 200    CONTINUE
 
C ---------------------------------------------------
C --- CONTROLE DES RESULTATS OBTENUS APRES RESOLUTION 
C     DU SYSTEME NON LINEAIRE LOCAL
C ---------------------------------------------------

C ---- VERIFICATION DES MULTIPLICATEURS PLASTIQUES
        MAXI = ABS(CRIT(3))
        DO 205 K = 1, NBMECA
          IF(YF(NDT+1+NBMECA+K).GT.MAXI) 
     &      MAXI = YF(NDT+1+NBMECA+K)
 205    CONTINUE

        DO 210 K = 1, NBMECA
          RATIO = YF(NDT+1+NBMECA+K)/MAXI
          IF (RATIO .LT. (-ABS(CRIT(3))))THEN 
            NEGMUL(INDI(K)) = .TRUE.
          ENDIF
 210    CONTINUE    
        
C ---> MISE A JOUR DES CONTRAINTES ET VARIABLES INTERNES
        CALL LCEQVN (NDT, YF, SIGF)

Caf 15/05/07 Debut
        VINF(23) = YF(NDT+1)
Caf 15/05/07 Fin        

        DO 250 K = 1, NBMECA
          KK       = INDI(K)
          IF (YF(NDT+1+K) .GT. VIND(KK)) THEN
            IF((KK.GT.4).AND.(KK.LT.8))THEN
              IF(YF(NDT+1+K).LE.VIND(KK-4))THEN
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
            
                IF(IRET.EQ.1)THEN
                  IF(DEBUG) WRITE(6,'(A,E16.9)')'HUJKSI --- R =',
     &                                           YF(NDT+1+K)
                ENDIF
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
                PCO    = MATER(7,2)
                CMON   = CMON*PCO*EXP(-BETA*YF(NDT+1))
                RDEC   = ((UN-YF(NDT+1+K))**2)/CMON/YF(NDT+1+K)/EVOL*
     &                    YF(NDT+1+NBMECA+K)
                IF(RDEC.GT.-CRIT(5))RDEC=-CRIT(5)
                NDEC   = NINT(RDEC)
                IF(NDEC.LT.1)NDEC=1
              
              ELSE
                CCYC   = DEUX*MATER(11,2)
                BETA   = MATER(2,2)
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
        
        GOTO 2000

 9999   CONTINUE
        IF(COMPT.GT.5)THEN
          NOCONV = .TRUE.
          GOTO 2000
        ENDIF

        IF(PROBT)THEN
          CALL LCEQVE(PREDI0,SIGF)
          CALL LCEQVE(SIGD0,SIGD)
          CALL LCEQVE(DEPS0,DEPS)
          CALL LCEQVN(NVI,VIND0,VIND)
          AREDEC = AREDE0
          STOPNC = STOPN0
          LOOP   = LOOP0
          DO 252 I = 1, 3
            IF(PROB(I).EQ.UN)THEN
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
            ELSEIF(PROB(I).EQ.DEUX)THEN
              VIND(27+I)   = ZERO
            ENDIF
  252     CONTINUE
          IRET = 0
          PROBT = .FALSE.
          CALL LCEQVN(NVI,VIND,VINF)
          GOTO 1

        ELSEIF(TRACTI)THEN
          CALL LCEQVE(PREDI0,SIGF)
          CALL LCEQVE(SIGD0,SIGD)
          CALL LCEQVE(DEPS0,DEPS)
          CALL LCEQVN(NVI,VIND0,VIND)
          AREDEC = AREDE0
          STOPNC = STOPN0
          LOOP   = LOOP0
          DO 254 I = 1, NBMECA
            IF(YE(NDT+1+NBMECA+I).EQ.ZERO)THEN
              VIND(23+INDI(I)) = ZERO
              TRACTI = .FALSE.
            ENDIF
  254     CONTINUE 
          IF(TRACTI)THEN
            DO 354 I = 1, NBMECA
             IF((INDI(I).NE.4).AND.
     &         (INDI(I).NE.8)) VIND(23+INDI(I)) = ZERO
 354        CONTINUE
          ENDIF
          IRET = 0
          PROBT = .FALSE.
          CALL LCEQVN(NVI,VIND,VINF)
          GOTO 1

        ELSE
          MAXI = ZERO
          RESI = 0
          DO 256 I = 1, NR
            IF(ABS(R(I)).GT.MAXI)THEN
              MAXI = ABS(R(I))
              RESI = I
            ENDIF
 256      CONTINUE
          CYCL = .FALSE.
          DO 260 I = 1, NBMECA
            IF((INDI(I).GT.4)
     &       .AND.(INDI(I).LT.8)
     &       .AND.(VIND(INDI(I)).EQ.MATER(18,2)))THEN
              CYCL = .TRUE.
            ENDIF
  260     CONTINUE
          IF((RESI.GT.7).AND.(RESI.LE.7+NBMECA))THEN
            RESI = RESI - 7
            IF((INDI(RESI).GT.4).AND.(INDI(RESI).LT.8))THEN
              CALL LCEQVE(PREDI0,SIGF)
              CALL LCEQVE(SIGD0,SIGD)
              CALL LCEQVE(DEPS0,DEPS)
              CALL LCEQVN(NVI,VIND0,VIND)
              AREDEC = AREDE0
              STOPNC = STOPN0
              LOOP   = LOOP0
              VIND(23+INDI(RESI)) = ZERO
              IRET = 0
              PROBT = .FALSE.
              CALL LCEQVN(NVI,VIND,VINF)
              GOTO 1
            ELSE
              NOCONV = .TRUE.
            ENDIF
          ELSEIF(CYCL)THEN
            CALL LCEQVE(PREDI0,SIGF)
            CALL LCEQVE(SIGD0,SIGD)
            CALL LCEQVE(DEPS0,DEPS)
            CALL LCEQVN(NVI,VIND0,VIND)
            AREDEC = AREDE0
            STOPNC = STOPN0
            LOOP   = LOOP0
            DO 261 I = 1, NBMECA
              IF((INDI(I).GT.4)
     &       .AND.(INDI(I).LT.8)
     &       .AND.(VIND(INDI(I)).EQ.MATER(18,2)))THEN
                VIND(23+INDI(I)) = ZERO
              ENDIF
 261        CONTINUE
            IRET = 0
            PROBT = .FALSE.
            CALL LCEQVN(NVI,VIND,VINF)
            GOTO 1
          ELSE
            NOCONV = .TRUE.
          ENDIF
        ENDIF
 
 1000   FORMAT(A,15(1X,E12.5))
 1001   FORMAT(A,2(I3))

 2000   CONTINUE

        END

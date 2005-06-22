        SUBROUTINE CJSMIS( MOD, CRIT, MATER, NVI, EPSD, DEPS,
     >                  SIGD, SIGF, VIND, VINF,
     >                  NOCONV,AREDEC,STOPNC,
     >                  NITER,EPSCON)

        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/06/2005   AUTEUR REZETTE C.REZETTE 
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
C     INTEGRATION PLASTIQUE (MECANISME ISOTROPE SEUL) DE LA LOI CJS
C                 SUR DT DE Y = (SIG, VIN, LAMBI)
C
C     ON RESOUD                       R(DY) = 0
C     PAR UNE METHODE DE NEWTON       DRDY(DYI) DDYI = - R(DYI)
C                                     DYI+1 = DYI + DDYI  (DYO DEBUT)
C     ET ON REACTUALISE               YF = YD + DY
C     ------------------------------------------------------------------
C     IN   MOD      :  MODELISATION
C          CRIT     :  CRITERES DE CONVERGENCE
C          MATER    :  COEFFICIENTS MATERIAU A T+DT
C          DEPS     :  INCREMENT DE DEFORMATION
C          SIGD     :  CONTRAINTE  A T
C          VIND     :  VARIABLES INTERNES  A T
C          AREDEC   :  ARRET DES DECOUPAGES
C          STOPNC   :  ARRET EN CAS DE NON CONVERGENCE
C     VAR  SIGF     :  CONTRAINTE  A T+DT
C          VINF     :  VARIABLES INTERNES  A T+DT
C          NOCONV   :  PAS DE CONVERGENCE
C          NITER    :  NOMBRE D ITERATIONS A CONVERGENCE
C          EPSCON   :  VALEUR ERR FINALE
C     ------------------------------------------------------------------

        INTEGER       NDT, NDI, NR, NMOD,NITER,NVI,IRET
        INTEGER       NITIMP
        PARAMETER     (NMOD = 8 )
        PARAMETER     (NITIMP = 100)
        INTEGER       ITER
        LOGICAL       NOCONV, AREDEC,STOPNC


        REAL*8        EPSD(6),DEPS(6),DET
        REAL*8        SIGD(6), SIGF(6)
        REAL*8        VIND(*), VINF(*),EPSCON
        REAL*8        MATER(14,2), CRIT(*)
        REAL*8        R(NMOD), DRDY(NMOD,NMOD)
        REAL*8        DDY(NMOD), DY(NMOD), YD(NMOD), YF(NMOD)

        REAL*8        ERR, ERR1, ERR2
        INTEGER       UMESS, IUNIFI
        REAL*8        ERIMP(NITIMP,3)
        INTEGER       I,J

        CHARACTER*8   MOD
        CHARACTER*1   TRANS,KSTOP

        COMMON /TDIM/   NDT, NDI

C
C     ------------------------------------------------------------------





         UMESS = IUNIFI('MESSAGE')
         NOCONV = .FALSE.

C -> DIMENSION DU PROBLEME NR = NDT(SIG) + 1(QISO) + 1(DLAMBI)

        NR = NDT + 2



C -> MISE A ZERO DES DATAS

        DO 10 I =1, NR
          DDY(I) = 0.D0
          DY(I) = 0.D0
          YD(I) = 0.D0
          YF(I) = 0.D0
 10     CONTINUE


C -> INITIALISATION DE YD PAR LA PREDICTION ELASTIQUE (SIGF, VIND, ZERO)

        CALL LCEQVN(NDT, SIGF, YD)
        YD(NDT+1) = VIND(1)
        YD(NDT+2) = 0.D0

C -> INITIALISATION : DY : CALCUL DE LA SOLUTION D ESSAI INITIALE EN DY
C    (SOLUTION EXPLICITE)

C        CALL CJSIIS( MOD, MATER, DEPS, YD, DY )


C---------------------------------------
C -> BOUCLE SUR LES ITERATIONS DE NEWTON
C---------------------------------------

        ITER = 0
 100    CONTINUE

        ITER = ITER + 1

C -> INCREMENTATION DE YF = YD + DY

        CALL LCSOVN( NR, YD, DY, YF)


C -> CALCUL DU SECOND MEMBRE A T+DT :  -R(DY)
C    ET CALCUL DU JACOBIEN DU SYSTEME A T+DT :  DRDY(DY)

        DO 50 I =1, NR
        R(I) = 0.D0
        DO 60 J =1, NR
          DRDY(I,J) = 0.D0
 60     CONTINUE
 50     CONTINUE

        CALL CJSJIS( MOD, MATER, DEPS, YD, YF, R, DRDY)


C -> RESOLUTION DU SYSTEME LINEAIRE : DRDY(DY).DDY = -R(DY)

        CALL LCEQVN( NR, R, DDY)
        TRANS=' '
        KSTOP='S'
        CALL MGAUSS(TRANS,KSTOP,DRDY, DDY, NMOD, NR, 1, DET, IRET)


C -> REACTUALISATION DE DY = DY + DDY

        CALL LCSOVN( NR, DDY, DY, DY )


C -> VERIFICATION DE LA CONVERGENCE : ERREUR = !!DDY!!/!!DY!! < TOLER

        CALL LCNRVN ( NR , DDY , ERR1 )
        CALL LCNRVN ( NR , DY  , ERR2 )
        IF(ERR2.EQ.0.D0) THEN
          ERR = ERR1
        ELSE
          ERR = ERR1 / ERR2
        ENDIF
        IF(ITER.LE.NITIMP) THEN
         ERIMP(ITER,1) = ERR1
         ERIMP(ITER,2) = ERR2
         ERIMP(ITER,3) = ERR
        ENDIF


        IF( ITER .LE. INT(ABS(CRIT(1))) ) THEN

C          --   CONVERVENCE   --
           IF(ERR .LT.  CRIT(3) ) THEN
            GOTO 200

C          --  NON CONVERVENCE : ITERATION SUIVANTE  --
           ELSE
            GOTO 100
           ENDIF

        ELSE

C          --  NON CONVERVENCE : ITERATION MAXI ATTEINTE  --
        IF(AREDEC.AND.STOPNC) THEN
         CALL CJSNCV('CJSMIS',NITIMP,ITER,NDT,NVI,UMESS,
     >          ERIMP,
     >          EPSD,DEPS,SIGD,VIND)
        ELSE
         CALL UTMESS('A', 'CJSMIS','NON CONVERG.: NB ITER MAX ATTEINT')
         NOCONV = .TRUE.
        ENDIF
        ENDIF

 200    CONTINUE
        NITER = ITER
        EPSCON = ERR


C -> INCREMENTATION DE YF = YD + DY

        CALL LCSOVN( NR, YD, DY, YF)


C -> MISE A JOUR DES CONTRAINTES ET VARIABLES INTERNES

        CALL LCEQVN( NDT, YF(1), SIGF )
        VINF(1) = YF(NDT+1)

        END

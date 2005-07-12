        SUBROUTINE LCPLNL ( LOI,  TOLER, ITMAX, MOD,   IMAT,
     1                      NMAT, MATERD,MATERF,MATCST,NR, NVI, TEMPD,
     2                      TEMPF,TIMED, TIMEF, DEPS,  EPSD, SIGD, VIND,
     3                      COMP,NBCOMM, CPMONO, PGL,
     3                      SIGF, VINF, ICOMP, IRTETI)
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/07/2005   AUTEUR VABHHTS J.PELLET 
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
C TOLE CRP_21
C       ----------------------------------------------------------------
C       INTEGRATION ELASTO-PLASTIQUE ET VISCO-PLASTICITE
C             SUR DT DE Y = ( SIG , VIN )
C       LE SYSTEME  A RESOUDRE EN DY ETANT NON  LINEAIRE
C
C       ON RESOUD DONC                  R(DY) = 0
C       PAR UNE METHODE DE NEWTON       DRDY(DYI) DDYI = - R(DYI)
C                                       DYI+1 = DYI + DDYI  (DYO DEBUT)
C       ET ON REACTUALISE               YF = YD + DY
C       ----------------------------------------------------------------
C       ATTENTION :     ON REACTUALISE ICI DEPS DE FACON A CE QUE
C                       DEPS(3) = DY(NR) EN C_PLAN
C       ----------------------------------------------------------------
C
C       IN  LOI    :  MODELE DE COMPORTEMENT
C           TOLER  :  TOLERANCE DE CONVERGENCE LOCALE
C           ITMAX  :  NOMBRE MAXI D'ITERATIONS LOCALES
C           MOD    :  TYPE DE MODELISATION
C           IMAT   :  ADRESSE DU MATERIAU CODE
C           NMAT   :  DIMENSION MATER
C           MATERD :  COEFFICIENTS MATERIAU A T
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           MATCST :  'OUI' SI MATERIAU CONSTANT SUR DT
C           TEMPD  :  TEMPERATURE A T
C           TEMPF  :  TEMPERATURE A T+DT
C           TIMED  :  INSTANT  T
C           TIMEF  :  INSTANT T+DT
C           EPSD   :  DEFORMATION A T
C           SIGD   :  CONTRAINTE A T
C           VIND   :  VARIABLES INTERNES A T
C           NR     :  NB EQUATION DU SYSTEME R(DY)
C           NVI    :  NB VARIABLES INTERNES
C           ICOMP  :  COMPTEUR POUR LE REDECOUPAGE DU PAS DE TEMPS
C       VAR DEPS   :  INCREMENT DE DEFORMATION
C       OUT SIGF   :  CONTRAINTE A T+DT
C           VINF   :  VARIABLES INTERNES A T+DT
C           IRTETI = 1:  CONTROLE DU REDECOUPAGE DU PAS DE TEMPS
C       ----------------------------------------------------------------
C           NMOD   :  DIMENSION R , DRDY
C           R      :  VECTEUR RESIDU
C           DRDY   :  JACOBIEN
C           DY     :  INCREMENT DES VARIABLES = ( DSIG  DVIN  (DEPS3)  )
C           DDY    :  CORRECTION SUR L'INCREMENT DES VARIABLES
C                                             = ( DDSIG DDVIN (DDEPS3) )
C           YD     :  VARIABLES A T   = ( SIGD  VIND  (EPSD3)   )
C           YF     :  VARIABLES A T+DT= ( SIGF  VINF  (EPSF3)   )
C           TYPESS :  TYPE DE SOLUTION D ESSAI POUR NEWTON
C           ESSAI  :  VALEUR  SOLUTION D ESSAI POUR NEWTON
C           INTG   :  COMPTEUR DU NOMBRE DE TENTATIVES D'INTEGRATIONS
C       ----------------------------------------------------------------
        INTEGER         IMAT, NMAT,    NMOD  , ICOMP
C
C
        INTEGER         TYPESS, ITMAX, IRET
        INTEGER         NR,     NDT,    NDI,    NVI,  ITER
C
        REAL*8          TOLER,  ESSAI, RBID
        REAL*8          EPSD(6),        DEPS(6)
        REAL*8          SIGD(6),        SIGF(6)
        REAL*8          VIND(*),        VINF(*)
C      DIMENSIONNEMENT DYNAMIQUE (MERCI F90)
        REAL*8          R(NR),        DRDY(NR,NR), RINI(NR)
        REAL*8          DRDY1(NR,NR)
        REAL*8          DDY(NDT+NVI),DY(NDT+NVI),YD(NDT+NVI),YF(NDT+NVI)
        REAL*8          MATERD(NMAT,2) ,MATERF(NMAT,2)
        REAL*8          TEMPD, TEMPF,   TIMED, TIMEF
C
        CHARACTER*8     MOD,  NBITER
        CHARACTER*16    LOI
        CHARACTER*3     MATCST
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C       ----------------------------------------------------------------
        INTEGER I, INTG, IRTET, IRTETI

        INTEGER         NBCOMM(NMAT,3)
        REAL*8          PGL(3,3)
        CHARACTER*16    CPMONO(5*NMAT+1),COMP(*)

C       ----------------------------------------------------------------
C
C --    INITIALISATION YD = ( SIGD , VIND , (EPSD(3)) )
C
        ESSAI = 1.D-5

C       DIMENSION DYNAMIQUE DE YD,YF,DY,R,DDY
        DO 100  I = 1 , NR
           R( I ) = 0.D0
 100   CONTINUE

        DO 101 I = 1 , (NDT+NVI)
           DDY( I ) = 0.D0
           DY( I ) = 0.D0
           YD( I ) = 0.D0
           YF( I ) = 0.D0
 101   CONTINUE

C       ----------------------------------------------------------------
C
C --    INITIALISATION YD = ( SIGD , VIND , (EPSD(3)) )
C
        CALL LCEQVN ( NDT  ,  SIGD , YD )
        IRTETI = 0
        CALL LCEQVN ( NVI-1,  VIND , YD(NDT+1) )
        IF(MOD(1:6).EQ.'C_PLAN') YD (NR) = EPSD(3)
C
C       RESOLUTION ITERATIVE PAR NEWTON DE R(DY) = 0
C       SOIT  DRDY(DYI) DDYI = -R(DYI)  ET DYI+1 = DYI + DDYI
C       -----------------------------------------------------
C
C -     INITIALISATION DU TYPE DE SOLUTION D ESSAI (-1)
C
        TYPESS = -1
        INTG   = 0

C
 2              CONTINUE
C
C --    CALCUL DE LA SOLUTION D ESSAI INITIALE DU SYSTEME NL EN DY
C
        CALL LCINIT ( LOI,   TYPESS, ESSAI, MOD, NMAT,
     &                MATERF,TIMED,TIMEF,NR, NVI, YD,
     &                EPSD,  DEPS,   DY )
C

        ITER = 0
 1      CONTINUE
        ITER = ITER + 1

C
C --    INCREMENTATION DE  YF = YD + DY
C
        CALL LCSOVN ( NR , YD , DY , YF )
C
C --    CALCUL DES TERMES DU SYSTEME A T+DT = -R(DY)
C
        CALL LCRESI ( LOI,   MOD,   IMAT, NMAT, MATERD,MATERF,
     3                COMP,NBCOMM, CPMONO, PGL,NR,NVI,
     &                TEMPF,TIMED,TIMEF,YD,YF,DEPS,EPSD,DY,R )

C     SAUVEGARDE DE R(DY0) POUR TEST DE CONVERGENCE
        IF(ITER.EQ.1) THEN
           CALL LCEQVN ( NR ,   R ,   RINI )
        ENDIF
C
C --    CALCUL DU JACOBIEN DU SYSTEME A T+DT = DRDY(DY)
C
        CALL LCJACB ( LOI,   MOD,   IMAT, NMAT, MATERF,TEMPF,
     &                  TIMED,TIMEF,     YF,    DEPS,
     3                COMP,NBCOMM, CPMONO, PGL,NR,NVI,
     &                  EPSD,  DY,    DRDY )
C
C --    RESOLUTION DU SYSTEME LINEAIRE DRDY(DY).DDY = -R(DY)
C

        CALL LCEQMN ( NR , DRDY , DRDY1 )
        CALL LCEQVN ( NR ,   R ,   DDY )
        CALL MGAUSS ( 'NFWP',DRDY1,DDY,NR,NR,1,RBID,IRET )

C
C --    REACTUALISATION DE DY = DY + DDY
C
        CALL LCSOVN ( NR , DDY , DY , DY )

        IF ( MOD(1:6).EQ.'C_PLAN' ) DEPS(3) = DY(NR)

C
C --    VERIFICATION DE LA CONVERGENCE EN DY  ET RE-INTEGRATION ?
C
        CALL LCCONV( LOI,    DY,   DDY, NR, ITMAX, TOLER, ITER, INTG,
     &               R,RINI,TYPESS, ESSAI, ICOMP, IRTET)
        IF ( IRTET.GT.0 ) GOTO (1,2,3), IRTET
C
C --    CONVERGENCE > INCREMENTATION DE  YF = YD + DY
C
        CALL LCSOVN ( NDT+NVI , YD , DY , YF )
C
C --    MISE A JOUR DE SIGF , VINF
C
        CALL LCEQVN ( NDT ,   YF(1)     , SIGF )
        CALL LCEQVN ( NVI-1 , YF(NDT+1) , VINF )
        IF ( LOI(1:7) .EQ. 'NADAI_B' ) THEN
           DO 10  I = 3 , NVI-1
              VIND ( I ) = 0.D0
   10      CONTINUE
        ENDIF
        VINF (NVI) = 1.D0
        IF (LOI(1:8).EQ.'MONOCRIS') THEN
           VINF (NVI) = ITER
        ENDIF
C
C
        IRTETI = 0
        GOTO 9999
 3      CONTINUE
        IRTETI = 1
        GOTO 9999
C
 9999   CONTINUE
        END

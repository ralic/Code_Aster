        SUBROUTINE CJSJIS( MOD, MATER, DEPS, YD, YF, R,  DRDY)
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C
C     RESOLUTION PAR METHODE DE NEWTON       DRDY(DYI) DDYI = - R(DYI)
C
C     CALCUL DU SECOND MEMBRE : - R(DYI)
C     CALCUL DU JACOBIEN      : DRDY(DYI)
C                               Y =  ( SIG    ,  VIN    ,  LAMBI   )
C                               R = -( LE     ,  LQ     ,  FI      )
C                           DRDY  =  ( DLEDS  ,  DLEDQ  ,  DLEDL   )
C                                    ( DLQDS  ,  DLQDQ  ,  DLQDL   )
C                                    ( DFIDS  ,  DFIDQ  ,  DFIDL   )
C     ------------------------------------------------------------------
C     IN   MOD      :  MODELISATION
C          MATER    :  COEFFICIENTS MATERIAU A T+DT
C          DEPS     :  INCREMENT DE DEFORMATION
C          YD       :  VARIABLES A T = (SIGD, VIND, LAMBID)
C          YF       :  VARIABLES A T+DT = (SIGF, VINF, LAMBIF)
C     OUT  R        :  SECOND MEMBRE
C          DRDY     :  JACOBIEN
C     ------------------------------------------------------------------

        INTEGER       NDT, NDI, NMOD
        PARAMETER     (NMOD = 8 )

        REAL*8        DEPS(6)
        REAL*8        YD(NMOD), YF(NMOD), R(NMOD), DRDY(NMOD,NMOD)
        REAL*8        MATER(14,2), N, KOP, PA
        REAL*8        HOOKNL(6,6), HOOK(6,6)
        REAL*8        E, NU, AL, LA, MU, I1F
        REAL*8        DLAMBI, COEF1, COEF2
        REAL*8        LE(6), LQ, FI
        REAL*8        DLEDS(6,6), DLEDQ(6), DLEDL(6)
        REAL*8        DLQDS(6), DLQDQ, DLQDL
        REAL*8        DFIDS(6), DFIDQ, DFIDL
        REAL*8        DSIGNL(6), DSIGL(6), DEPSE(6),QINIT
        INTEGER       I,J

        REAL*8        ZERO, UN, D12, DEUX, TROIS, KRON(6), IDEN6(6,6)

        PARAMETER     ( D12  = .5D0   )
        PARAMETER     ( UN   = 1.D0   )
        PARAMETER     ( ZERO = 0.D0   )
        PARAMETER     ( DEUX = 2.D0   )
        PARAMETER     ( TROIS= 3.D0   )


        CHARACTER*8   MOD

        COMMON /TDIM/   NDT, NDI

        DATA    IDEN6  /UN     , ZERO  , ZERO  , ZERO  ,ZERO  ,ZERO,
     &                   ZERO   , UN    , ZERO  , ZERO  ,ZERO  ,ZERO,
     &                   ZERO   , ZERO  , UN    , ZERO  ,ZERO  ,ZERO,
     &                   ZERO   , ZERO  , ZERO  , UN    ,ZERO  ,ZERO,
     &                   ZERO   , ZERO  , ZERO  , ZERO  ,UN    ,ZERO,
     &                   ZERO   , ZERO  , ZERO  , ZERO  ,ZERO  ,UN/

        DATA          KRON /UN , UN , UN , ZERO ,ZERO ,ZERO/


C-----------------------------------------------------------------------
C->     PROPRIETES CJS MATERIAU
C------------------------------

        N  = MATER(3,2)
        KOP = MATER(4,2)
        PA = MATER(12,2)
        QINIT = MATER(13,2)

C-----------------------------------------------------------------------
C->     OPERATEURS DE RIGIDITE
C-----------------------------------------------------------------------


C- OPERATEUR LINEAIRE
C++++++++++++++++++++

        CALL LCINMA(ZERO,HOOK)

        E = MATER(1,1)
        NU = MATER(2,1)
        AL = E  * (UN-NU) / (UN+NU) / (UN-DEUX*NU)
        LA = NU * E       / (UN+NU) / (UN-DEUX*NU)
        MU = E  * D12     / (UN+NU)


C - 3D/DP/AX
        IF ( MOD(1:2) .EQ. '3D'     .OR.
     &       MOD(1:6) .EQ. 'D_PLAN' .OR.
     &       MOD(1:4) .EQ. 'AXIS'        )THEN
             DO 10 I = 1,NDI
             DO 10 J = 1,NDI
                 IF(I.EQ.J) HOOK(I,J) = AL
                 IF(I.NE.J) HOOK(I,J) = LA
 10          CONTINUE
             DO 15 I = NDI+1 , NDT
             DO 15 J = NDI+1 , NDT
                 IF(I.EQ.J) HOOK(I,J) = DEUX* MU
 15          CONTINUE

C - CP/1D
        ELSE IF ( MOD(1:6) .EQ. 'C_PLAN' .OR.
     &            MOD(1:2) .EQ. '1D' )THEN
             CALL U2MESS('F','ALGORITH2_15')
        ENDIF


C- OPERATEUR NON LINEAIRE
C++++++++++++++++++++++++

        I1F = ZERO
        DO 20 I=1, NDI
        I1F = I1F + YF(I)
 20     CONTINUE
        IF((I1F +QINIT) .EQ. 0.D0 ) THEN
         I1F  = -QINIT+1.D-12 * PA
        ENDIF

        COEF1 = ((I1F +QINIT)/TROIS/PA)**N

        DO 25 I=1, NDT
        DO 25 J=1, NDT
        HOOKNL(I,J) = COEF1*HOOK(I,J)
 25     CONTINUE






C--------------------------------------------------------
C->     LOI D ETAT : LE
C->  ET DERIVEE DE LA LOI D ETAT : DLEDS, DLEDQ, DLEDL
C--------------------------------------------------------

C- LOI D ETAT
C++++++++++++

        DLAMBI = YF(NDT+2) - YD(NDT+2)

        DO 40 I=1,NDT
        DEPSE(I) = DEPS(I) + DLAMBI*KRON(I)/TROIS
 40     CONTINUE

        CALL LCPRMV ( HOOKNL, DEPSE, DSIGNL )

        DO 45 I=1,NDT
        LE(I) = YF(I) - YD(I) - DSIGNL(I)
 45     CONTINUE


C- DERIVEE DE LA LOI D ETAT
C++++++++++++++++++++++++++

        COEF2 = N/TROIS/PA * (TROIS*PA/(I1F +QINIT))**(UN-N)
        CALL LCPRMV ( HOOK, DEPSE, DSIGL )
        CALL LCINMA(ZERO,DLEDS)

        DO 50 I=1,NDT

           DO 60 J=1, NDT
             DLEDS(I,J) = IDEN6(I,J) - COEF2 * DSIGL(I) * KRON(J)
 60        CONTINUE

           DLEDQ(I) = ZERO

           DLEDL(I) = ZERO
           DO 70 J=1, NDT
             DLEDL(I) = DLEDL(I) - HOOKNL(I,J)*KRON(J)/TROIS
 70        CONTINUE

 50     CONTINUE





C-----------------------------------------------------------------------
C->     LOI D ECROUISSAGE DE QISO : LQ
C->  ET DERIVEE DE LA LOI D ECROUISSAGE DE QISO : DLQDS, DLQDQ, DLQDL
C-----------------------------------------------------------------------


C- LOI D ECROUISSAGE
C+++++++++++++++++++
        LQ = YF(NDT+1) - YD(NDT+1) + DLAMBI * KOP * (YF(NDT+1)/PA)**N


C- DERIVEE DE LA LOI D ECROUISSAGE
C+++++++++++++++++++++++++++++++++

        CALL LCINVE( ZERO, DLQDS )
        DLQDQ = UN + DLAMBI * KOP * N / PA * (PA/YF(NDT+1))**(UN-N)
        DLQDL = KOP * (YF(NDT+1)/PA)**N
C------------------------------------------------------------------
C->     SEUIL ISOTROPE : FI
C->  ET DERIVEE DE LA FONCTION SEUIL ISOTROPE : DFIDS, DFIDQ, DFIDL
C------------------------------------------------------------------


C- SEUIL ISOTROPE
C++++++++++++++++

        FI = -(I1F +QINIT)/TROIS + YF(NDT+1)


C- DERIVEE DU SEUIL ISOTROPE
C+++++++++++++++++++++++++++

        DO 80 I=1,NDT
        DFIDS(I) = -KRON(I)/TROIS
 80     CONTINUE

        DFIDQ = UN
        DFIDL = ZERO




C-------------------------------------------
C->     ASSEMBLAGE DE R = - ( LE, LQ, FI )
C->  ET ASSEMBLAGE DE DRDY
C
C       DRDY  =   DLEDS   DLEDQ   DLEDL
C                 DLQDS   DLQDQ   DLQDL
C                 DFIDS   DFIDQ   DFIDL
C
C-------------------------------------------


C- ASSEMBLAGE DE R
C+++++++++++++++++

        DO 90 I=1,NDT
        R(I) = -LE(I)
 90     CONTINUE

        R(NDT+1) = - LQ
        R(NDT+2) = - FI


C- ASSEMBLAGE DE DRDY
C++++++++++++++++++++

        CALL LCICMA(DLEDS,6,6,NDT,NDT,1,1,DRDY,NMOD,NMOD,1,1)
        CALL LCICMA(DLEDQ,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,1,NDT+1)
        CALL LCICMA(DLEDL,6,1,NDT,1,1,1,DRDY,NMOD,NMOD,1,NDT+2)

        CALL LCICMA(DLQDS,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,NDT+1,1)
        DRDY(NDT+1, NDT+1) = DLQDQ
        DRDY(NDT+1, NDT+2) = DLQDL

        CALL LCICMA(DFIDS,1,6,1,NDT,1,1,DRDY,NMOD,NMOD,NDT+2,1)
        DRDY(NDT+2, NDT+1) = DFIDQ
        DRDY(NDT+2, NDT+2) = DFIDL


        END

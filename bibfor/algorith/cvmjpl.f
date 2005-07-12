        SUBROUTINE CVMJPL ( MOD, NMAT, MATER,
     &        TIMED, TIMEF,EPSD,DEPS,SIGF,VINF,SIGD,VIND,NVI,NR,DSDE)
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
C       ----------------------------------------------------------------
C       VISCOCHABOCHE  :  MATRICE SYMETRIQUE DE COMPORTEMENT TANGENT
C                         COHERENT A T OU T+DT
C       ----------------------------------------------------------------
C       IN  MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATER  :  COEFFICIENTS MATERIAU
C           NR   :  DIMENSION DRDY
C           DRDY   :  MATRICE JACOBIENNE
C
C       DRDY  = ( DGDS  DGDX1  DGDX2  DGDP  DGDR  0   (DGDE3) )
C               ( DLDS  DLDX1  DLDX2  DLDP  DLDR  0   (DLDE3) )
C               ( DJDS  DJDX1  DJDX2  DJDP  DJDR  0   (DJDE3) )
C               ( DKDS  DKDX1  DKDX2  DKDP  DKDR  0   (DKDE3) )
C               ( DRDS  DRDX1  DRDX2  DRDP  DRDR  0   (DRDE3) )
C               ( 0     0      0      0     0     1   (0)     )
C               ((DQDS)(DQDX1)(DQDX2)(DQDP)(DQDR)(0)  (DQDE3) )
C                                                     ( SI IOPTIO = 1 )
C
C
C       DRDY  = ( DGDS  DGDX1  DGDX2  DGDP  DGDR  DGDQ  DGDXXI (DGDE3) )
C               ( DLDS  DLDX1  DLDX2  DLDP  DLDR  DLDQ  DLDXXI (DLDE3) )
C               ( DJDS  DJDX1  DJDX2  DJDP  DJDR  DJDQ  DJDXXI (DJDE3) )
C               ( DKDS  DKDX1  DKDX2  DKDP  DKDR  DKDQ  DKDXXI (DKDE3) )
C               ( DRDS  DRDX1  DRDX2  DRDP  DRDR  DRDQ  DRDXXI (DRDE3) )
C               ( DTDS  DTDX1  DTDX2  DTDP  DTDR  DTDQ  DTDXXI (DTDE3) )
C               ( DXIDS DXIDX1 DXIDX2 DXIDP DXIDR DXIDQ DXIDXI(DXIDE3))
C               ((DQDS)(DQDX1)(DQDX2)(DQDP)(DQDR)(DQDQ)(DQDXXI)(DQDE3) )
C                                                     ( SI IOPTIO = 2 )
C
C       OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
C       ----------------------------------------------------------------
        INTEGER         NDT , NDI , NMAT , NR, NVI, IRET
        INTEGER         IOPTIO   , IDNR , NOPT
        REAL*8          UN , ZERO, DET
        PARAMETER       ( UN   =  1.D0   )
        PARAMETER       ( ZERO =  0.D0   )

        REAL*8          MATER(NMAT,2)
C
        REAL*8          DGDS(6,6),  DGDX1(6,6),  DGDX2(6,6),  DGDR(6)
        REAL*8          DLDS(6,6),  DLDX1(6,6),  DLDX2(6,6),  DLDR(6)
        REAL*8          DJDS(6,6),  DJDX1(6,6),  DJDX2(6,6),  DJDR(6)
        REAL*8          DKDS(6),    DKDX1(6),    DKDX2(6),    DKDR
        REAL*8          DRDS(6),    DRDX1(6),    DRDX2(6),    DRDR
        REAL*8          DTDS(6),    DTDX1(6),    DTDX2(6),    DTDR
        REAL*8          DXIDS(6,6), DXIDX1(6,6), DXIDX2(6,6), DXIDR(6)
        REAL*8          DQDS(6),    DQDX1(6),    DQDX2(6),    DQDR
C
        REAL*8          DGDQ(6),  DGDP(6),  DGDXXI(6,6),   DGDE3(6)
        REAL*8          DLDQ(6),  DLDP(6),  DLDXXI(6,6),   DLDE3(6)
        REAL*8          DJDQ(6),  DJDP(6),  DJDXXI(6,6),   DJDE3(6)
        REAL*8          DKDQ,     DKDP   ,  DKDXXI(6),     DKDE3
        REAL*8          DRDQ,     DRDP   ,  DRDXXI(6),     DRDE3
        REAL*8          DTDQ,     DTDP   ,  DTDXXI(6),     DTDE3
        REAL*8          DXIDQ(6), DXIDP(6), DXIDXI(6,6),  DXIDE3(6)
        REAL*8          DQDQ,     DQDP   ,  DQDXXI(6),     DQDE3
C
        REAL*8          HOOKF(6,6), DSDE(6,6),  I6(6,6)
C
        REAL*8          MATA(6,6),  MATB(6,6),  MATC(6,6)
        REAL*8          MATD(6,6),  MATE(6,6),  MATF(6,6)
        REAL*8          MTMP(6,6),  MTMP1(6,6), MTMP2(6,6)
        REAL*8          VTMP(6),    VTMP1(6),   VTMP2(6)
        REAL*8          DKDSET(6),  DKDX1E(6), DKDX2E(6)
        REAL*8          CONST1,     CONST2, XX

        INTEGER         N1 , N2 , N3   , N4   , N5  , N6,  N7,  N8,K
C
        CHARACTER*8     MOD
C
C DIMENSIONNEMENT DYNAMIQUE : TABLEAUX AUTOMATIQUES FORTRAN 90
        REAL*8          DRDY(NR,NR),YD(NDT+NVI),YF(NDT+NVI),DY(NDT+NVI)
        REAL*8          TIMED,TIMEF,SIGF(*),VINF(*),SIGD(*),VIND(*)
        REAL*8          EPSD(*),DEPS(*)
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
        COMMON /OPTI/   IOPTIO , IDNR
C       ----------------------------------------------------------------
        DATA  I6        /UN     , ZERO  , ZERO  , ZERO  ,ZERO  ,ZERO,
     1                   ZERO   , UN    , ZERO  , ZERO  ,ZERO  ,ZERO,
     2                   ZERO   , ZERO  , UN    , ZERO  ,ZERO  ,ZERO,
     3                   ZERO   , ZERO  , ZERO  , UN    ,ZERO  ,ZERO,
     4                   ZERO   , ZERO  , ZERO  , ZERO  ,UN    ,ZERO,
     5                   ZERO   , ZERO  , ZERO  , ZERO  ,ZERO  ,UN/
C       ----------------------------------------------------------------
C
        NOPT = 0
        DO 1 K=1,6
          DGDE3(K) = 0.D0
 1      CONTINUE
        IF ( IOPTIO .EQ. 2 ) NOPT = IDNR

        CALL LCEQVN ( NDT  ,  SIGD , YD )
        CALL LCEQVN ( NDT  ,  SIGF , YF )
        CALL LCEQVN ( NVI-1,  VIND , YD(NDT+1) )
        CALL LCEQVN ( NVI-1,  VINF , YF(NDT+1) )
        CALL LCEQVN ( NR,  YF , DY )
        CALL DAXPY( NR, -1.D0, YD, 1,DY, 1)

        CALL CVMJAC ( MOD, NMAT, MATER, TIMED,
     &                TIMEF, YF, DY, NR, EPSD, DEPS,DRDY )
C
C
C - RECUPERER LES SOUS-MATRICES BLOC
C
        N1 = 1
        N2 = NDT + 1
        N3 = 2*NDT + 1
        N4 = 3*NDT + 1
        N5 = 3*NDT + 2
        N6 = 3*NDT + 3
        N7 = 3*NDT + 4
        N8 = 3*NDT + 4 + NOPT
C
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N1,N1,DGDS  ,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N1,N2,DGDX1 ,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N1,N3,DGDX2 ,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,  1,N1,N4,DGDP  ,6,1,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,  1,N1,N5,DGDR  ,6,1,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,  1,N1,N6,DGDQ  ,6,1,1,1)
C
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N2,N1,DLDS  ,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N2,N2,DLDX1 ,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N2,N3,DLDX2 ,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,  1,N2,N4,DLDP  ,6,1,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,  1,N2,N5,DLDR  ,6,1,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,  1,N2,N6,DLDQ  ,6,1,1,1)
C
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N3,N1,DJDS  ,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N3,N2,DJDX1 ,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N3,N3,DJDX2 ,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,  1,N3,N4,DJDP  ,6,1,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,  1,N3,N5,DJDR  ,6,1,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,  1,N3,N6,DJDQ  ,6,1,1,1)
C
        CALL LCICMA (DRDY,NR,NR,  1,NDT,N4,N1,DKDS  ,1,6,1,1)
        CALL LCICMA (DRDY,NR,NR,  1,NDT,N4,N2,DKDX1 ,1,6,1,1)
        CALL LCICMA (DRDY,NR,NR,  1,NDT,N4,N3,DKDX2 ,1,6,1,1)
        CALL LCICMA (DRDY,NR,NR,  1,  1,N4,N4,DKDP  ,1,1,1,1)
        CALL LCICMA (DRDY,NR,NR,  1,  1,N4,N5,DKDR  ,1,1,1,1)
        CALL LCICMA (DRDY,NR,NR,  1,  1,N4,N6,DKDQ  ,1,1,1,1)
C
        CALL LCICMA (DRDY,NR,NR,  1,NDT,N5,N1,DRDS  ,1,6,1,1)
        CALL LCICMA (DRDY,NR,NR,  1,NDT,N5,N2,DRDX1 ,1,6,1,1)
        CALL LCICMA (DRDY,NR,NR,  1,NDT,N5,N3,DRDX2 ,1,6,1,1)
        CALL LCICMA (DRDY,NR,NR,  1,  1,N5,N4,DRDP  ,1,1,1,1)
        CALL LCICMA (DRDY,NR,NR,  1,  1,N5,N5,DRDR  ,1,1,1,1)
        CALL LCICMA (DRDY,NR,NR,  1,  1,N5,N6,DRDQ  ,1,1,1,1)
C
        CALL LCICMA (DRDY,NR,NR,  1,NDT,N6,N1,DTDS  ,1,6,1,1)
        CALL LCICMA (DRDY,NR,NR,  1,NDT,N6,N2,DTDX1 ,1,6,1,1)
        CALL LCICMA (DRDY,NR,NR,  1,NDT,N6,N3,DTDX2 ,1,6,1,1)
        CALL LCICMA (DRDY,NR,NR,  1,  1,N6,N4,DTDP  ,1,1,1,1)
        CALL LCICMA (DRDY,NR,NR,  1,  1,N6,N5,DTDR  ,1,1,1,1)
        CALL LCICMA (DRDY,NR,NR,  1,  1,N6,N6,DTDQ  ,1,1,1,1)
C
        IF ( MOD(1:6) .EQ. 'C_PLAN' ) THEN
           CALL LCICMA (DRDY,NR,NR,NDT,  1,N1,N8,DGDE3 ,6,1,1,1)
           CALL LCICMA (DRDY,NR,NR,NDT,  1,N2,N8,DLDE3 ,6,1,1,1)
           CALL LCICMA (DRDY,NR,NR,NDT,  1,N3,N8,DJDE3 ,6,1,1,1)
           CALL LCICMA (DRDY,NR,NR,  1,  1,N4,N8,DKDE3 ,1,1,1,1)
           CALL LCICMA (DRDY,NR,NR,  1,  1,N5,N8,DRDE3 ,1,1,1,1)
           CALL LCICMA (DRDY,NR,NR,  1,  1,N6,N8,DTDE3 ,1,1,1,1)
C
           CALL LCICMA (DRDY,NR,NR,  1,NDT,N8,N1,DQDS  ,1,6,1,1)
           CALL LCICMA (DRDY,NR,NR,  1,NDT,N8,N2,DQDX1 ,1,6,1,1)
           CALL LCICMA (DRDY,NR,NR,  1,NDT,N8,N3,DQDX2 ,1,6,1,1)
           CALL LCICMA (DRDY,NR,NR,  1,  1,N8,N4,DQDP  ,1,1,1,1)
           CALL LCICMA (DRDY,NR,NR,  1,  1,N8,N5,DQDR  ,1,1,1,1)
           CALL LCICMA (DRDY,NR,NR,  1,  1,N8,N6,DQDQ  ,1,1,1,1)
           CALL LCICMA (DRDY,NR,NR,  1,  1,N8,N8,DQDE3 ,1,1,1,1)
        ENDIF
C
        IF ( IOPTIO .EQ. 2) THEN
           CALL LCICMA (DRDY,NR,NR,NDT,NDT,N1,N7,DGDXXI ,6,6,1,1)
           CALL LCICMA (DRDY,NR,NR,NDT,NDT,N2,N7,DLDXXI ,6,6,1,1)
           CALL LCICMA (DRDY,NR,NR,NDT,NDT,N3,N7,DJDXXI ,6,6,1,1)
           CALL LCICMA (DRDY,NR,NR,  1,NDT,N4,N7,DKDXXI ,1,6,1,1)
           CALL LCICMA (DRDY,NR,NR,  1,NDT,N5,N7,DRDXXI ,1,6,1,1)
           CALL LCICMA (DRDY,NR,NR,  1,NDT,N6,N7,DTDXXI ,1,6,1,1)
C
           CALL LCICMA (DRDY,NR,NR,NDT,NDT,N7,N1,DXIDS  ,6,6,1,1)
           CALL LCICMA (DRDY,NR,NR,NDT,NDT,N7,N2,DXIDX1 ,6,6,1,1)
           CALL LCICMA (DRDY,NR,NR,NDT,NDT,N7,N3,DXIDX2 ,6,6,1,1)
           CALL LCICMA (DRDY,NR,NR,NDT,  1,N7,N4,DXIDP  ,6,1,1,1)
           CALL LCICMA (DRDY,NR,NR,NDT,  1,N7,N5,DXIDR  ,6,1,1,1)
           CALL LCICMA (DRDY,NR,NR,NDT,  1,N7,N6,DXIDQ  ,6,1,1,1)
           CALL LCICMA (DRDY,NR,NR,NDT,NDT,N7,N7,DXIDXI,6,6,1,1)
C
           IF ( MOD(1:6) .EQ. 'C_PLAN' ) THEN
              CALL LCICMA (DRDY,NR,NR,  1,  1,N8,N6,DQDQ   ,1,1,1,1)
              CALL LCICMA (DRDY,NR,NR,  1,NDT,N8,N7,DQDXXI ,1,6,1,1)
              CALL LCICMA (DRDY,NR,NR,NDT,  1,N7,N8,DXIDE3 ,6,1,1,1)
              CALL LCICMA (DRDY,NR,NR,  1,  1,N6,N8,DTDE3  ,1,1,1,1)
           ENDIF
        ENDIF
C
C       ----------------------------------------------------------------
C       L'OPTION 2 MODIFIE DKDS, DKDX1, DKDX2 CALCULES ICI SOUS LES NOMS
C       DE DKDSET, DKDX1E, ET DKDX2E
C       ----------------------------------------------------------------
        IF ( IOPTIO .EQ. 2) THEN
           CALL LCEQMA ( I6 , MTMP )
           CALL MGAUSS ('NFVP',DXIDXI,MTMP,6,NDT,NDT,DET,IRET)
           CALL LCPTMV ( MTMP       , DTDXXI    , VTMP2  )
           CALL LCPRSC ( VTMP2      , DXIDP     , XX     )
           CONST1 = DKDR / DRDR * DRDQ
C
           IF ( CONST1 .NE. 0.D0) THEN
             CONST2 = CONST1 / ( (DTDP - XX )* CONST1
     1              + DTDQ * ( DKDP - DKDR / DRDR * DRDP ) )
             CONST1 = 1.D0 / CONST1
C
             CALL LCPTMV ( DXIDS      , VTMP2     , VTMP   )
             CALL LCDIVE ( DTDS       , VTMP      , VTMP1  )
             CALL LCPRSV ( CONST1*DTDQ, DKDS      , VTMP   )
             CALL LCSOVE ( VTMP1      , VTMP      , VTMP1  )
             CALL LCPRSV ( CONST2     , VTMP1     , DKDSET )
C
             CALL LCPTMV ( DXIDX1     , VTMP2     , VTMP   )
             CALL LCDIVE ( DTDX1      , VTMP      , VTMP1  )
             CALL LCPRSV ( CONST1*DTDQ, DKDX1     , VTMP   )
             CALL LCSOVE ( VTMP1      , VTMP      , VTMP1  )
             CALL LCPRSV ( CONST2     , VTMP1     , DKDX1E)
C
             CALL LCPTMV ( DXIDX2     , VTMP2     , VTMP   )
             CALL LCDIVE ( DTDX2      , VTMP      , VTMP1  )
             CALL LCPRSV ( CONST1*DTDQ, DKDX2     , VTMP   )
             CALL LCSOVE ( VTMP1      , VTMP      , VTMP1  )
             CALL LCPRSV ( CONST2     , VTMP1     , DKDX2E)
C
           ELSE
             CONST1 = 1.D0 / ( DKDP - DKDR / DRDR * DRDP )
             CALL LCPRSV ( CONST1     , DKDS      , DKDSET )
             CALL LCPRSV ( CONST1     , DKDX1     , DKDX1E)
             CALL LCPRSV ( CONST1     , DKDX2     , DKDX2E)
           ENDIF
C
        ELSE
           CONST1 = 1.D0 / ( DKDP - DKDR / DRDR * DRDP )
           CALL LCPRSV ( CONST1     , DKDS      , DKDSET )
           CALL LCPRSV ( CONST1     , DKDX1     , DKDX1E)
           CALL LCPRSV ( CONST1     , DKDX2     , DKDX2E)
        ENDIF
C
C - E = ( DLDX2 - DLDP * DKDX2 ) * ( DJDX2 - DJDP * DKDX2 )-1
C
        CALL LCPRTE ( DJDP       , DKDX2E   , MTMP   )
        CALL LCDIMA ( DJDX2      , MTMP      , MTMP   )
        CALL LCEQMA ( I6         , MTMP1              )
        CALL MGAUSS ('NFVP',MTMP,MTMP1,6,NDT,NDT,DET,IRET)
        CALL LCPRTE ( DLDP       , DKDX2E   , MTMP   )
        CALL LCDIMA ( DLDX2      , MTMP      , MTMP   )
        CALL LCPRMM ( MTMP       , MTMP1     , MATE   )
C
C - F = ( DJDX1 - DJDP * DKDX1 ) * ( DLDX1 - DLDP * DKDX1 )-1
C
        CALL LCPRTE ( DLDP       , DKDX1E   , MTMP   )
        CALL LCDIMA ( DLDX1      , MTMP      , MTMP   )
        CALL LCEQMA ( I6         , MTMP1              )
        CALL MGAUSS ('NFVP',MTMP,MTMP1,6,NDT,NDT,DET,IRET)
        CALL LCPRTE ( DJDP       , DKDX1E   , MTMP   )
        CALL LCDIMA ( DJDX1      , MTMP      , MTMP   )
        CALL LCPRMM ( MTMP       , MTMP1     , MATF   )
C
C - MATRICE C  TELLE QUE    DX1 = C * DSIG
C
        CALL LCPRTE ( DJDP       , DKDX1E   , MTMP   )
        CALL LCDIMA ( DJDX1      , MTMP      , MTMP   )
        CALL LCPRMM ( MATE       , MTMP      , MTMP1  )
        CALL LCPRTE ( DLDP       , DKDX1E   , MTMP   )
        CALL LCDIMA ( DLDX1      , MTMP      , MTMP   )
        CALL LCDIMA ( MTMP       , MTMP1     , MTMP1  )
        CALL LCEQMA ( I6         , MTMP2              )
        CALL MGAUSS ('NFVP',MTMP1,MTMP2,6,NDT,NDT,DET,IRET)
C
        CALL LCPRTE ( DJDP       , DKDSET    , MTMP   )
        CALL LCDIMA ( DJDS       , MTMP      , MTMP   )
        CALL LCPRMM ( MATE       , MTMP      , MTMP1  )
        CALL LCPRTE ( DLDP       , DKDSET    , MTMP   )
        CALL LCDIMA ( DLDS       , MTMP      , MTMP   )
        CALL LCDIMA ( MTMP1      , MTMP      , MTMP   )
        CALL LCPRMM ( MTMP2      , MTMP      , MATC   )
C
C - MATRICE D  TELLE QUE    DX2 = D * DSIG
C
        CALL LCPRTE ( DLDP       , DKDX2E   , MTMP   )
        CALL LCDIMA ( DLDX2      , MTMP      , MTMP   )
        CALL LCPRMM ( MATF       , MTMP      , MTMP1  )
        CALL LCPRTE ( DJDP       , DKDX2E   , MTMP   )
        CALL LCDIMA ( DJDX2      , MTMP      , MTMP   )
        CALL LCDIMA ( MTMP       , MTMP1     , MTMP1  )
        CALL LCEQMA ( I6         , MTMP2              )
        CALL MGAUSS ('NFVP',MTMP1,MTMP2,6,NDT,NDT,DET,IRET)
C
        CALL LCPRTE ( DLDP       , DKDSET    , MTMP   )
        CALL LCDIMA ( DLDS       , MTMP      , MTMP   )
        CALL LCPRMM ( MATF       , MTMP      , MTMP1  )
        CALL LCPRTE ( DJDP       , DKDSET    , MTMP   )
        CALL LCDIMA ( DJDS       , MTMP      , MTMP   )
        CALL LCDIMA ( MTMP1      , MTMP      , MTMP   )
        CALL LCPRMM ( MTMP2      , MTMP      , MATD   )
C
C - VTMP2 = DKDS + DKDX1 * C + DKDX2 * D
C
        CALL LCPTMV ( MATD       , DKDX2E   , VTMP2  )
        CALL LCPTMV ( MATC       , DKDX1E   , VTMP1  )
        CALL LCSOVE ( VTMP1      , VTMP2     , VTMP2  )
        CALL LCSOVE ( DKDS       , VTMP2     , VTMP2  )
C
C - VTMP1 = DQDS + DQDX1 * C + DQDX2 * D - DQDP * VTMP2
C
        CALL LCINVE ( 0.D0       , VTMP1              )
        IF ( MOD(1:6) .EQ. 'C_PLAN' ) THEN
           CALL LCPRSV ( DQDP       , VTMP2     , VTMP   )
           CALL LCDIVE ( DQDS       , VTMP      , VTMP1  )
           CALL LCPTMV ( MATC       , DQDX1     , VTMP   )
           CALL LCSOVE ( VTMP1      , VTMP      , VTMP1  )
           CALL LCPTMV ( MATD       , DQDX2     , VTMP2  )
           CALL LCSOVE ( VTMP1      , VTMP      , VTMP1  )
        ENDIF
C
C - MTMP  = DGDS + DGDX1 * C + DGDX2 * D - DGDP * VTMP2 - DGDE3 * VTMP1
C
        CALL LCPRTE ( DGDE3      , VTMP1     , MTMP1  )
        CALL LCPRTE ( DGDP       , VTMP2     , MTMP2  )
        CALL LCSOMA ( MTMP1      , MTMP2     , MTMP   )
        CALL LCDIMA ( DGDS       , MTMP      , MTMP   )
        CALL LCPRMM ( DGDX1      , MATC      , MTMP1  )
        CALL LCSOMA ( MTMP       , MTMP1     , MTMP   )
        CALL LCPRMM ( DGDX2      , MATD      , MTMP1  )
        CALL LCSOMA ( MTMP       , MTMP1     , MTMP   )
C
C - DSDE = (MTMP1)-1 * H
C
        CALL LCEQMA ( I6     , MTMP1          )
        CALL MGAUSS ('NFVP',MTMP,MTMP1,6,NDT,NDT,DET,IRET)
        CALL LCOPLI ('ISOTROPE',MOD,MATER(1,1),HOOKF)
        CALL LCPRMM ( MTMP1  , HOOKF  , DSDE  )
C
C - MATRICE DE COMPORTEMENT TANGENT:  SYMETRISATION DE DSDE
C
        CALL LCTRMA ( DSDE   , MTMP           )
        CALL LCSOMA ( DSDE   , MTMP   , DSDE  )
        CALL LCPRSM ( 0.5D0  , DSDE   , DSDE  )
C
C
        END

        SUBROUTINE LMAJPL ( MOD, NMAT, MATER,
     &                TIMED, TIMEF,SIGF,VINF,SIGD,VIND,NVI,NR,DSDE )
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
C       MODELE VISCOPLASTIQUE DE BESANCON EN VITESSE
C                  :  MATRICE SYMETRIQUE DE COMPORTEMENT TANGENT
C                     COHERENT A T OU T+DT
C       ----------------------------------------------------------------
C       IN  MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATER  :  COEFFICIENTS MATERIAU
C           NR   :  DIMENSION DRDY
C           DRDY   :  MATRICE JACOBIENNE
C
C           DRDY  = ( DGDS  DGDX  DGDX1  DGDX2  DGDV  )
C                   ( DLDS  DLDX  DLDX1  DLDX2  DLDV  )
C                   ( DJDS  DJDX  DJDX1  DJDX2  DJDV  )
C                   ( DIDS  DIDX  DIDX1  DIDX2  DIDV  )
C                   ( DKDS  DKDX  DKDX1  DKDX2  DKDV  )
C
C       OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
C       ----------------------------------------------------------------
        INTEGER         NDT , NDI , NMAT , NR, NVI
        INTEGER         N1,  N2,  N3,  N4, IRET
        REAL*8          UN , ZERO, DET
        PARAMETER       ( UN   =  1.D0   )
        PARAMETER       ( ZERO =  0.D0   )

C DIMENSIONNEMENT DYNAMIQUE : TABLEAUX AUTOMATIQUES FORTRAN 90
        REAL*8          DRDY(NR,NR),YD(NR),YF(NR),DY(NR)

        REAL*8 TIMED,TIMEF,SIGF(*),VINF(*),SIGD(*),VIND(*)
        REAL*8          MATER(NMAT,2)
C
        REAL*8          DGDS(6,6) , DGDX(6,6) , DGDX1(6,6)
        REAL*8          DLDS(6,6) , DLDX(6,6) , DLDX1(6,6)
        REAL*8          DJDS(6,6) , DJDX(6,6) , DJDX1(6,6)
        REAL*8          DIDS(6,6) , DIDX(6,6) , DIDX1(6,6)
        REAL*8          DKDS(6)   , DKDX(6)   , DKDX1(6)
C
        REAL*8          DGDX2(6,6), DGDV(6)
        REAL*8          DLDX2(6,6), DLDV(6)
        REAL*8          DJDX2(6,6), DJDV(6)
        REAL*8          DIDX2(6,6), DIDV(6)
        REAL*8          DKDX2(6)  , DKDV
C
        REAL*8          HOOKF(6,6), DSDE(6,6),  I4(6,6)
C
C
        REAL*8          MATA(6,6),  MATB(6,6),  MATC(6,6),  MATD(6,6)
        REAL*8          MATE(6,6),  MTMP(6,6),  MTMP1(6,6), MTMP2(6,6)
        REAL*8          VTMP(6),    VTMP1(6),   VTMF(6)
C
        CHARACTER*8     MOD
C
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT   , NDI
C       ----------------------------------------------------------------
        DATA  I4        /UN     , ZERO  , ZERO  , ZERO  ,ZERO  ,ZERO,
     1                   ZERO   , UN    , ZERO  , ZERO  ,ZERO  ,ZERO,
     2                   ZERO   , ZERO  , UN    , ZERO  ,ZERO  ,ZERO,
     3                   ZERO   , ZERO  , ZERO  , UN    ,ZERO  ,ZERO,
     4                   ZERO   , ZERO  , ZERO  , ZERO  ,UN    ,ZERO,
     5                   ZERO   , ZERO  , ZERO  , ZERO  ,ZERO  ,UN/
C       ----------------------------------------------------------------
C

        CALL LCEQVN ( NDT  ,  SIGD , YD )
        CALL LCEQVN ( NDT  ,  SIGF , YF )
        CALL LCEQVN ( NVI-1,  VIND , YD(NDT+1) )
        CALL LCEQVN ( NVI-1,  VINF , YF(NDT+1) )
        CALL LCEQVN ( NR,  YF , DY )
        CALL DAXPY( NR, -1.D0, YD, 1,DY, 1)

C       RECALCUL DE LA MATRICE JACOBIENNE SUR LA SOLUTION FINALE
        CALL LMAJAC ( MOD, NMAT, MATER, TIMED, TIMEF,
     &                  YF,  DY,   NR,  DRDY )

C
C - RECUPERER LES SOUS-MATRICES BLOC
C
        N1 =   NDT + 1
        N2 = 2*NDT + 1
        N3 = 3*NDT + 1
        N4 = 4*NDT + 1
C
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,1,1 ,DGDS ,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,1,N1,DGDX ,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,1,N2,DGDX1,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,1,N3,DGDX2,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,1  ,1,N4,DGDV ,6,1,1,1)
C
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N1,1 ,DLDS ,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N1,N1,DLDX ,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N1,N2,DLDX1,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N1,N3,DLDX2,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,1  ,N1,N4,DLDV ,6,1,1,1)
C
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N2,1 ,DJDS ,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N2,N1,DJDX ,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N2,N2,DJDX1,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N2,N3,DJDX2,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,1  ,N2,N4,DJDV ,6,1,1,1)
C
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N3,1 ,DIDS ,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N3,N1,DIDX ,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N3,N2,DIDX1,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,NDT,N3,N3,DIDX2,6,6,1,1)
        CALL LCICMA (DRDY,NR,NR,NDT,1  ,N3,N4,DIDV ,6,1,1,1)
C
        CALL LCICMA (DRDY,NR,NR,  1,NDT,N4,1 ,DKDS ,1,6,1,1)
        CALL LCICMA (DRDY,NR,NR,  1,NDT,N4,N1,DKDX ,1,6,1,1)
        CALL LCICMA (DRDY,NR,NR,  1,NDT,N4,N2,DKDX1,1,6,1,1)
        CALL LCICMA (DRDY,NR,NR,  1,NDT,N4,N3,DKDX2,1,6,1,1)
        CALL LCICMA (DRDY,NR,NR,  1,  1,N4,N4,DKDV ,1,1,1,1)
C
C       ----------------------------------------------------------------
C
C
C - A = MATA = DLDX2 (DIDX2)-1
C - B = MATB = DJDX2 (DIDX2)-1
        CALL LCEQMA ( DIDX2  , MTMP1          )
        CALL LCEQMA ( I4     , MTMP           )
        CALL MGAUSS ('NFVP',MTMP1,MTMP,6,NDT,NDT,DET,IRET)
        CALL LCPRMM ( DLDX2  , MTMP   , MATA  )
        CALL LCPRMM ( DJDX2  , MTMP   , MATB  )
C
C - C = MATC = (DLDX1 - A*DIDX1)*(DJDX1 - B*DIDX1)-1
        CALL LCPRMM ( MATB   , DIDX1  , MTMP1 )
        CALL LCDIMA ( DJDX1  , MTMP1  , MTMP1 )
        CALL LCEQMA ( I4     , MTMP           )
        CALL MGAUSS ('NFVP',MTMP1,MTMP,6,NDT,NDT,DET,IRET)
        CALL LCPRMM ( MATA   , DIDX1  , MTMP1 )
        CALL LCDIMA ( DLDX1  , MTMP1  , MTMP1 )
        CALL LCPRMM ( MTMP1  , MTMP   , MATC  )
C
C - F = VTMF = DLDV - A*DIDV - C*(DJDV - B*DIDV)
        CALL LCPRMV ( MATB   , DIDV   , VTMP  )
        CALL LCDIVE ( DJDV   , VTMP   , VTMP  )
        CALL LCPRMV ( MATC   , VTMP   , VTMP1 )
        CALL LCDIVE ( DLDV   , VTMP1  , VTMF  )
        CALL LCPRMV ( MATA   , DIDV   , VTMP  )
        CALL LCDIVE ( VTMF   , VTMP   , VTMF  )
C
C - D = MATD = DLDS - A*DIDS - C*(DJDS - B*DIDS) - F*DKDS
        CALL LCPRMM ( MATA   , DIDS   , MTMP  )
        CALL LCDIMA ( DLDS   , MTMP   , MATD  )
        CALL LCPRMM ( MATB   , DIDS   , MTMP  )
        CALL LCDIMA ( DJDS   , MTMP   , MTMP1 )
        CALL LCPRMM ( MATC   , MTMP1  , MTMP  )
        CALL LCDIMA ( MATD   , MTMP   , MATD  )
        CALL LCPRTE ( VTMF   , DKDS   , MTMP  )
        CALL LCDIMA ( MATD   , MTMP   , MATD  )
C
C - E = MATE = DLDX - A*DIDX - C*(DJDX - B*DIDX) - F*DKDX
        CALL LCPRMM ( MATA   , DIDX   , MTMP  )
        CALL LCDIMA ( DLDX   , MTMP   , MATE  )
        CALL LCPRMM ( MATB   , DIDX   , MTMP  )
        CALL LCDIMA ( DJDX   , MTMP   , MTMP1 )
        CALL LCPRMM ( MATC   , MTMP1  , MTMP  )
        CALL LCDIMA ( MATE   , MTMP   , MATE  )
        CALL LCPRTE ( VTMF   , DKDX   , MTMP  )
        CALL LCDIMA ( MATE   , MTMP   , MATE  )
C
C - MTMP2 =  ( DGDX - DGDV*DKDX ) *(E)-1*D
        CALL LCEQMA ( I4     , MTMP           )
        CALL LCEQMA ( MATE   , MTMP1          )
        CALL MGAUSS ('NFVP',MTMP1,MTMP,6,NDT,NDT,DET,IRET)
        CALL LCPRMM ( MTMP   , MATD   , MTMP1 )
C
        CALL LCPRTE ( DGDV   , DKDX   , MTMP  )
        CALL LCDIMA ( DGDX   , MTMP   , MTMP  )
        CALL LCPRMM ( MTMP   , MTMP1  , MTMP2 )
C
C - MTMP1 = DGDS - DGDV*DKDS - MTMP2
        CALL LCPRTE ( DGDV   , DKDS   , MTMP1 )
        CALL LCDIMA ( DGDS   , MTMP1  , MTMP1 )
        CALL LCDIMA ( MTMP1  , MTMP2  , MTMP1 )
C
C - DSDE = (MTMP1)-1 * H
        CALL LCEQMA ( I4     , MTMP           )
        CALL MGAUSS ('NFVP',MTMP1,MTMP,6,NDT,NDT,DET,IRET)
        CALL LCOPLI ('ISOTROPE',MOD,MATER(1,1),HOOKF)
        CALL LCPRMM ( MTMP   , HOOKF  , DSDE  )
C
C - MATRICE DE COMPORTEMENT TANGENT:  SYMETRISATION DE DSDE
        CALL LCTRMA ( DSDE   , MTMP           )
        CALL LCSOMA ( DSDE   , MTMP   , DSDE  )
        CALL LCPRSM ( .5D0   , DSDE   , DSDE  )
C
        END

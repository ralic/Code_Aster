subroutine cvmjpl(mod, nmat, mater, timed, timef,&
                  epsd, deps, sigf, vinf, sigd,&
                  vind, nvi, nr, dsde)
! aslint: disable=W1306
    implicit none
!       ================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       ----------------------------------------------------------------
!       VISCOCHABOCHE  :  MATRICE SYMETRIQUE DE COMPORTEMENT TANGENT
!                         COHERENT A T OU T+DT
!       ----------------------------------------------------------------
!       IN  MOD    :  TYPE DE MODELISATION
!           NMAT   :  DIMENSION MATER
!           MATER  :  COEFFICIENTS MATERIAU
!           NR   :  DIMENSION DRDY
!           DRDY   :  MATRICE JACOBIENNE
!
!       DRDY  = ( DGDS  DGDX1  DGDX2  DGDP  DGDR  0   (DGDE3) )
!               ( DLDS  DLDX1  DLDX2  DLDP  DLDR  0   (DLDE3) )
!               ( DJDS  DJDX1  DJDX2  DJDP  DJDR  0   (DJDE3) )
!               ( DKDS  DKDX1  DKDX2  DKDP  DKDR  0   (DKDE3) )
!               ( DRDS  DRDX1  DRDX2  DRDP  DRDR  0   (DRDE3) )
!               ( 0     0      0      0     0     1   (0)     )
!               ((DQDS)(DQDX1)(DQDX2)(DQDP)(DQDR)(0)  (DQDE3) )
!                                                     ( SI IOPTIO = 1 )
!
!
!       DRDY  = ( DGDS  DGDX1  DGDX2  DGDP  DGDR  DGDQ  DGDXXI (DGDE3) )
!               ( DLDS  DLDX1  DLDX2  DLDP  DLDR  DLDQ  DLDXXI (DLDE3) )
!               ( DJDS  DJDX1  DJDX2  DJDP  DJDR  DJDQ  DJDXXI (DJDE3) )
!               ( DKDS  DKDX1  DKDX2  DKDP  DKDR  DKDQ  DKDXXI (DKDE3) )
!               ( DRDS  DRDX1  DRDX2  DRDP  DRDR  DRDQ  DRDXXI (DRDE3) )
!               ( DTDS  DTDX1  DTDX2  DTDP  DTDR  DTDQ  DTDXXI (DTDE3) )
!               ( DXIDS DXIDX1 DXIDX2 DXIDP DXIDR DXIDQ DXIDXI(DXIDE3))
!               ((DQDS)(DQDX1)(DQDX2)(DQDP)(DQDR)(DQDQ)(DQDXXI)(DQDE3) )
!                                                     ( SI IOPTIO = 2 )
!
!       OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
!       ----------------------------------------------------------------
#include "asterfort/cvmjac.h"
#include "asterfort/lcdima.h"
#include "asterfort/lcdive.h"
#include "asterfort/lceqma.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcicma.h"
#include "asterfort/lcinve.h"
#include "asterfort/lcopli.h"
#include "asterfort/lcprmm.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lcprsm.h"
#include "asterfort/lcprsv.h"
#include "asterfort/lcprte.h"
#include "asterfort/lcptmv.h"
#include "asterfort/lcsoma.h"
#include "asterfort/lcsove.h"
#include "asterfort/lctrma.h"
#include "asterfort/mgauss.h"
#include "blas/daxpy.h"
    integer :: ndt, ndi, nmat, nr, nvi, iret
    integer :: ioptio, idnr, nopt
    real(kind=8) :: un, zero, det
    parameter       ( un   =  1.d0   )
    parameter       ( zero =  0.d0   )
!
    real(kind=8) :: mater(nmat, 2)
!
    real(kind=8) :: dgds(6, 6), dgdx1(6, 6), dgdx2(6, 6), dgdr(6)
    real(kind=8) :: dlds(6, 6), dldx1(6, 6), dldx2(6, 6), dldr(6)
    real(kind=8) :: djds(6, 6), djdx1(6, 6), djdx2(6, 6), djdr(6)
    real(kind=8) :: dkds(6), dkdx1(6), dkdx2(6), dkdr
    real(kind=8) :: drds(6), drdx1(6), drdx2(6), drdr
    real(kind=8) :: dtds(6), dtdx1(6), dtdx2(6), dtdr
    real(kind=8) :: dxids(6, 6), dxidx1(6, 6), dxidx2(6, 6), dxidr(6)
    real(kind=8) :: dqds(6), dqdx1(6), dqdx2(6), dqdr
!
    real(kind=8) :: dgdq(6), dgdp(6), dgdxxi(6, 6), dgde3(6)
    real(kind=8) :: dldq(6), dldp(6), dldxxi(6, 6), dlde3(6)
    real(kind=8) :: djdq(6), djdp(6), djdxxi(6, 6), djde3(6)
    real(kind=8) :: dkdq, dkdp, dkdxxi(6), dkde3
    real(kind=8) :: drdq, drdp, drdxxi(6), drde3
    real(kind=8) :: dtdq, dtdp, dtdxxi(6), dtde3
    real(kind=8) :: dxidq(6), dxidp(6), dxidxi(6, 6), dxide3(6)
    real(kind=8) :: dqdq, dqdp, dqdxxi(6), dqde3
!
    real(kind=8) :: hookf(6, 6), dsde(6, 6), i6(6, 6)
!
    real(kind=8) :: matc(6, 6)
    real(kind=8) :: matd(6, 6), mate(6, 6), matf(6, 6)
    real(kind=8) :: mtmp(6, 6), mtmp1(6, 6), mtmp2(6, 6)
    real(kind=8) :: vtmp(6), vtmp1(6), vtmp2(6)
    real(kind=8) :: dkdset(6), dkdx1e(6), dkdx2e(6)
    real(kind=8) :: const1, const2, xx
!
    integer :: n1, n2, n3, n4, n5, n6, n7, n8, k
!
    character(len=8) :: mod
!
! DIMENSIONNEMENT DYNAMIQUE : TABLEAUX AUTOMATIQUES FORTRAN 90
    real(kind=8) :: drdy(nr, nr), yd(ndt+nvi), yf(ndt+nvi), dy(ndt+nvi)
    real(kind=8) :: timed, timef, sigf(*), vinf(*), sigd(*), vind(*)
    real(kind=8) :: epsd(*), deps(*)
!       ----------------------------------------------------------------
    common /tdim/   ndt , ndi
    common /opti/   ioptio , idnr
!       ----------------------------------------------------------------
    data  i6        /un     , zero  , zero  , zero  ,zero  ,zero,&
     &                   zero   , un    , zero  , zero  ,zero  ,zero,&
     &                   zero   , zero  , un    , zero  ,zero  ,zero,&
     &                   zero   , zero  , zero  , un    ,zero  ,zero,&
     &                   zero   , zero  , zero  , zero  ,un    ,zero,&
     &                   zero   , zero  , zero  , zero  ,zero  ,un/
!       ----------------------------------------------------------------
!
    nopt = 0
    do 1 k = 1, 6
        dgde3(k) = 0.d0
 1  continue
    if (ioptio .eq. 2) nopt = idnr
!
    call lceqvn(ndt, sigd, yd)
    call lceqvn(ndt, sigf, yf)
    call lceqvn(nvi-1, vind, yd(ndt+1))
    call lceqvn(nvi-1, vinf, yf(ndt+1))
    call lceqvn(nr, yf, dy)
    call daxpy(nr, -1.d0, yd, 1, dy,&
               1)
!
    call cvmjac(mod, nmat, mater, timed, timef,&
                yf, dy, nr, epsd, deps,&
                drdy)
!
!
! - RECUPERER LES SOUS-MATRICES BLOC
!
    n1 = 1
    n2 = ndt + 1
    n3 = 2*ndt + 1
    n4 = 3*ndt + 1
    n5 = 3*ndt + 2
    n6 = 3*ndt + 3
    n7 = 3*ndt + 4
    n8 = 3*ndt + 4 + nopt
!
    call lcicma(drdy, nr, nr, ndt, ndt,&
                n1, n1, dgds, 6, 6,&
                1, 1)
    call lcicma(drdy, nr, nr, ndt, ndt,&
                n1, n2, dgdx1, 6, 6,&
                1, 1)
    call lcicma(drdy, nr, nr, ndt, ndt,&
                n1, n3, dgdx2, 6, 6,&
                1, 1)
    call lcicma(drdy, nr, nr, ndt, 1,&
                n1, n4, dgdp, 6, 1,&
                1, 1)
    call lcicma(drdy, nr, nr, ndt, 1,&
                n1, n5, dgdr, 6, 1,&
                1, 1)
    call lcicma(drdy, nr, nr, ndt, 1,&
                n1, n6, dgdq, 6, 1,&
                1, 1)
!
    call lcicma(drdy, nr, nr, ndt, ndt,&
                n2, n1, dlds, 6, 6,&
                1, 1)
    call lcicma(drdy, nr, nr, ndt, ndt,&
                n2, n2, dldx1, 6, 6,&
                1, 1)
    call lcicma(drdy, nr, nr, ndt, ndt,&
                n2, n3, dldx2, 6, 6,&
                1, 1)
    call lcicma(drdy, nr, nr, ndt, 1,&
                n2, n4, dldp, 6, 1,&
                1, 1)
    call lcicma(drdy, nr, nr, ndt, 1,&
                n2, n5, dldr, 6, 1,&
                1, 1)
    call lcicma(drdy, nr, nr, ndt, 1,&
                n2, n6, dldq, 6, 1,&
                1, 1)
!
    call lcicma(drdy, nr, nr, ndt, ndt,&
                n3, n1, djds, 6, 6,&
                1, 1)
    call lcicma(drdy, nr, nr, ndt, ndt,&
                n3, n2, djdx1, 6, 6,&
                1, 1)
    call lcicma(drdy, nr, nr, ndt, ndt,&
                n3, n3, djdx2, 6, 6,&
                1, 1)
    call lcicma(drdy, nr, nr, ndt, 1,&
                n3, n4, djdp, 6, 1,&
                1, 1)
    call lcicma(drdy, nr, nr, ndt, 1,&
                n3, n5, djdr, 6, 1,&
                1, 1)
    call lcicma(drdy, nr, nr, ndt, 1,&
                n3, n6, djdq, 6, 1,&
                1, 1)
!
    call lcicma(drdy, nr, nr, 1, ndt,&
                n4, n1, dkds, 1, 6,&
                1, 1)
    call lcicma(drdy, nr, nr, 1, ndt,&
                n4, n2, dkdx1, 1, 6,&
                1, 1)

    call lcicma(drdy, nr, nr, 1, ndt,&
                n4, n3, dkdx2, 1, 6,&
                1, 1)

    dkdp=drdy(n4,n4)

    dkdr=drdy(n4,n5)

    dkdq=drdy(n4,n6)

    dkdr=drdy(n4,n5)

    dkdq=drdy(n4,n6)
!
    call lcicma(drdy, nr, nr, 1, ndt,&
                n5, n1, drds, 1, 6,&
                1, 1)
    call lcicma(drdy, nr, nr, 1, ndt,&
                n5, n2, drdx1, 1, 6,&
                1, 1)
    call lcicma(drdy, nr, nr, 1, ndt,&
                n5, n3, drdx2, 1, 6,&
                1, 1)

    drdp=drdy(n5,n4)

    drdr=drdy(n5,n5)

    drdq=drdy(n5,n6)
!
    call lcicma(drdy, nr, nr, 1, ndt,&
                n6, n1, dtds, 1, 6,&
                1, 1)
    call lcicma(drdy, nr, nr, 1, ndt,&
                n6, n2, dtdx1, 1, 6,&
                1, 1)
    call lcicma(drdy, nr, nr, 1, ndt,&
                n6, n3, dtdx2, 1, 6,&
                1, 1)

    dtdp=drdy(n6,n4)

    dtdr=drdy(n6,n5)

    dtdq=drdy(n6,n6)

!
    if (mod(1:6) .eq. 'C_PLAN') then
        call lcicma(drdy, nr, nr, ndt, 1,&
                    n1, n8, dgde3, 6, 1,&
                    1, 1)
        call lcicma(drdy, nr, nr, ndt, 1,&
                    n2, n8, dlde3, 6, 1,&
                    1, 1)
        call lcicma(drdy, nr, nr, ndt, 1,&
                    n3, n8, djde3, 6, 1,&
                    1, 1)

        dkde3=drdy(n4,n8)

        drde3=drdy(n5,n8)

        dtde3=drdy(n6,n8)

!
        call lcicma(drdy, nr, nr, 1, ndt,&
                    n8, n1, dqds, 1, 6,&
                    1, 1)
        call lcicma(drdy, nr, nr, 1, ndt,&
                    n8, n2, dqdx1, 1, 6,&
                    1, 1)
        call lcicma(drdy, nr, nr, 1, ndt,&
                    n8, n3, dqdx2, 1, 6,&
                    1, 1)

        dqdp = drdy(n8, n4)


        dqdr = drdy(n8, n5)


        dqdq = drdy(n8, n6)


        dqde3 = drdy(n8, n8)

    endif
!
    if (ioptio .eq. 2) then
        call lcicma(drdy, nr, nr, ndt, ndt,&
                    n1, n7, dgdxxi, 6, 6,&
                    1, 1)
        call lcicma(drdy, nr, nr, ndt, ndt,&
                    n2, n7, dldxxi, 6, 6,&
                    1, 1)
        call lcicma(drdy, nr, nr, ndt, ndt,&
                    n3, n7, djdxxi, 6, 6,&
                    1, 1)
        call lcicma(drdy, nr, nr, 1, ndt,&
                    n4, n7, dkdxxi, 1, 6,&
                    1, 1)
        call lcicma(drdy, nr, nr, 1, ndt,&
                    n5, n7, drdxxi, 1, 6,&
                    1, 1)
        call lcicma(drdy, nr, nr, 1, ndt,&
                    n6, n7, dtdxxi, 1, 6,&
                    1, 1)
!
        call lcicma(drdy, nr, nr, ndt, ndt,&
                    n7, n1, dxids, 6, 6,&
                    1, 1)
        call lcicma(drdy, nr, nr, ndt, ndt,&
                    n7, n2, dxidx1, 6, 6,&
                    1, 1)
        call lcicma(drdy, nr, nr, ndt, ndt,&
                    n7, n3, dxidx2, 6, 6,&
                    1, 1)
        call lcicma(drdy, nr, nr, ndt, 1,&
                    n7, n4, dxidp, 6, 1,&
                    1, 1)
        call lcicma(drdy, nr, nr, ndt, 1,&
                    n7, n5, dxidr, 6, 1,&
                    1, 1)
        call lcicma(drdy, nr, nr, ndt, 1,&
                    n7, n6, dxidq, 6, 1,&
                    1, 1)
        call lcicma(drdy, nr, nr, ndt, ndt,&
                    n7, n7, dxidxi, 6, 6,&
                    1, 1)
!
        if (mod(1:6) .eq. 'C_PLAN') then

            dqdq=drdy(n8, n6)

            call lcicma(drdy, nr, nr, 1, ndt,&
                        n8, n7, dqdxxi, 1, 6,&
                        1, 1)
            call lcicma(drdy, nr, nr, ndt, 1,&
                        n7, n8, dxide3, 6, 1,&
                        1, 1)

            dtde3=drdy(n6, n8)
        endif
    endif
!
!       ----------------------------------------------------------------
!       L'OPTION 2 MODIFIE DKDS, DKDX1, DKDX2 CALCULES ICI SOUS LES NOMS
!       DE DKDSET, DKDX1E, ET DKDX2E
!       ----------------------------------------------------------------
    if (ioptio .eq. 2) then
        call lceqma(i6, mtmp)
        call mgauss('NFVP', dxidxi, mtmp, 6, ndt,&
                    ndt, det, iret)
        call lcptmv(mtmp, dtdxxi, vtmp2)
        call lcprsc(vtmp2, dxidp, xx)
        const1 = dkdr / drdr * drdq
!
        if (const1 .ne. 0.d0) then
            const2 = const1 / ( (dtdp - xx )* const1 + dtdq * ( dkdp - dkdr / drdr * drdp ) )
            const1 = 1.d0 / const1
!
            call lcptmv(dxids, vtmp2, vtmp)
            call lcdive(dtds, vtmp, vtmp1)
            call lcprsv(const1*dtdq, dkds, vtmp)
            call lcsove(vtmp1, vtmp, vtmp1)
            call lcprsv(const2, vtmp1, dkdset)
!
            call lcptmv(dxidx1, vtmp2, vtmp)
            call lcdive(dtdx1, vtmp, vtmp1)
            call lcprsv(const1*dtdq, dkdx1, vtmp)
            call lcsove(vtmp1, vtmp, vtmp1)
            call lcprsv(const2, vtmp1, dkdx1e)
!
            call lcptmv(dxidx2, vtmp2, vtmp)
            call lcdive(dtdx2, vtmp, vtmp1)
            call lcprsv(const1*dtdq, dkdx2, vtmp)
            call lcsove(vtmp1, vtmp, vtmp1)
            call lcprsv(const2, vtmp1, dkdx2e)
!
        else
            const1 = 1.d0 / ( dkdp - dkdr / drdr * drdp )
            call lcprsv(const1, dkds, dkdset)
            call lcprsv(const1, dkdx1, dkdx1e)
            call lcprsv(const1, dkdx2, dkdx2e)
        endif
!
    else
        const1 = 1.d0 / ( dkdp - dkdr / drdr * drdp )
        call lcprsv(const1, dkds, dkdset)
        call lcprsv(const1, dkdx1, dkdx1e)
        call lcprsv(const1, dkdx2, dkdx2e)
    endif
!
! - E = ( DLDX2 - DLDP * DKDX2 ) * ( DJDX2 - DJDP * DKDX2 )-1
!
    call lcprte(djdp, dkdx2e, mtmp)
    call lcdima(djdx2, mtmp, mtmp)
    call lceqma(i6, mtmp1)
    call mgauss('NFVP', mtmp, mtmp1, 6, ndt,&
                ndt, det, iret)
    call lcprte(dldp, dkdx2e, mtmp)
    call lcdima(dldx2, mtmp, mtmp)
    call lcprmm(mtmp, mtmp1, mate)
!
! - F = ( DJDX1 - DJDP * DKDX1 ) * ( DLDX1 - DLDP * DKDX1 )-1
!
    call lcprte(dldp, dkdx1e, mtmp)
    call lcdima(dldx1, mtmp, mtmp)
    call lceqma(i6, mtmp1)
    call mgauss('NFVP', mtmp, mtmp1, 6, ndt,&
                ndt, det, iret)
    call lcprte(djdp, dkdx1e, mtmp)
    call lcdima(djdx1, mtmp, mtmp)
    call lcprmm(mtmp, mtmp1, matf)
!
! - MATRICE C  TELLE QUE    DX1 = C * DSIG
!
    call lcprte(djdp, dkdx1e, mtmp)
    call lcdima(djdx1, mtmp, mtmp)
    call lcprmm(mate, mtmp, mtmp1)
    call lcprte(dldp, dkdx1e, mtmp)
    call lcdima(dldx1, mtmp, mtmp)
    call lcdima(mtmp, mtmp1, mtmp1)
    call lceqma(i6, mtmp2)
    call mgauss('NFVP', mtmp1, mtmp2, 6, ndt,&
                ndt, det, iret)
!
    call lcprte(djdp, dkdset, mtmp)
    call lcdima(djds, mtmp, mtmp)
    call lcprmm(mate, mtmp, mtmp1)
    call lcprte(dldp, dkdset, mtmp)
    call lcdima(dlds, mtmp, mtmp)
    call lcdima(mtmp1, mtmp, mtmp)
    call lcprmm(mtmp2, mtmp, matc)
!
! - MATRICE D  TELLE QUE    DX2 = D * DSIG
!
    call lcprte(dldp, dkdx2e, mtmp)
    call lcdima(dldx2, mtmp, mtmp)
    call lcprmm(matf, mtmp, mtmp1)
    call lcprte(djdp, dkdx2e, mtmp)
    call lcdima(djdx2, mtmp, mtmp)
    call lcdima(mtmp, mtmp1, mtmp1)
    call lceqma(i6, mtmp2)
    call mgauss('NFVP', mtmp1, mtmp2, 6, ndt,&
                ndt, det, iret)
!
    call lcprte(dldp, dkdset, mtmp)
    call lcdima(dlds, mtmp, mtmp)
    call lcprmm(matf, mtmp, mtmp1)
    call lcprte(djdp, dkdset, mtmp)
    call lcdima(djds, mtmp, mtmp)
    call lcdima(mtmp1, mtmp, mtmp)
    call lcprmm(mtmp2, mtmp, matd)
!
! - VTMP2 = DKDS + DKDX1 * C + DKDX2 * D
!
    call lcptmv(matd, dkdx2e, vtmp2)
    call lcptmv(matc, dkdx1e, vtmp1)
    call lcsove(vtmp1, vtmp2, vtmp2)
    call lcsove(dkds, vtmp2, vtmp2)
!
! - VTMP1 = DQDS + DQDX1 * C + DQDX2 * D - DQDP * VTMP2
!
    call lcinve(0.d0, vtmp1)
    if (mod(1:6) .eq. 'C_PLAN') then
        call lcprsv(dqdp, vtmp2, vtmp)
        call lcdive(dqds, vtmp, vtmp1)
        call lcptmv(matc, dqdx1, vtmp)
        call lcsove(vtmp1, vtmp, vtmp1)
        call lcptmv(matd, dqdx2, vtmp2)
        call lcsove(vtmp1, vtmp, vtmp1)
    endif
!
! - MTMP  = DGDS + DGDX1 * C + DGDX2 * D - DGDP * VTMP2 - DGDE3 * VTMP1
!
    call lcprte(dgde3, vtmp1, mtmp1)
    call lcprte(dgdp, vtmp2, mtmp2)
    call lcsoma(mtmp1, mtmp2, mtmp)
    call lcdima(dgds, mtmp, mtmp)
    call lcprmm(dgdx1, matc, mtmp1)
    call lcsoma(mtmp, mtmp1, mtmp)
    call lcprmm(dgdx2, matd, mtmp1)
    call lcsoma(mtmp, mtmp1, mtmp)
!
! - DSDE = (MTMP1)-1 * H
!
    call lceqma(i6, mtmp1)
    call mgauss('NFVP', mtmp, mtmp1, 6, ndt,&
                ndt, det, iret)
    call lcopli('ISOTROPE', mod, mater(1, 1), hookf)
    call lcprmm(mtmp1, hookf, dsde)
!
! - MATRICE DE COMPORTEMENT TANGENT:  SYMETRISATION DE DSDE
!
    call lctrma(dsde, mtmp)
    call lcsoma(dsde, mtmp, dsde)
    call lcprsm(0.5d0, dsde, dsde)
!
!
end subroutine

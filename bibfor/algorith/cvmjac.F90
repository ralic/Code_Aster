subroutine cvmjac(mod, nmat, materf, timed, timef,&
                  yf, dy, nmod, epsd, deps,&
                  drdy)
! aslint: disable=W1501
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
!       VISCOCHABOCHE   :
!           CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY
!                   DY  = ( DSIG DX1 DX2 DP DR 0 (DEPS3))
!                   Y   = ( SIG  X1  X2  P  R  Q (EPS3) )
!
!       DRDY  = ( DGDS  DGDX1  DGDX2  DGDP  DGDR  0   (DGDE3) )
!               ( DLDS  DLDX1  DLDX2  DLDP  DLDR  0   (DLDE3) )
!               ( DJDS  DJDX1  DJDX2  DJDP  DJDR  0   (DJDE3) )
!               ( DKDS  DKDX1  DKDX2  DKDP  DKDR  0   (DKDE3) )
!               ( DRDS  DRDX1  DRDX2  DRDP  DRDR  0   (DRDE3) )
!               ( 0     0      0      0     0     1   (0)     )
!               ((DQDS)(DQDX1)(DQDX2)(DQDP)(DQDR)(0)  (DQDE3) )
!                                                     ( SI IOPTIO = 0 )
!
!                   DY  = ( DSIG DX1 DX2 DP DR DQ DXXI (DEPS3))
!                   Y   = ( SIG  X1  X2  P  R  Q  XXI  (EPS3) )
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
!       IN  MOD    :  TYPE DE MODELISATION
!           NMAT   :  DIMENSION MATER
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!           YF     :  VARIABLES A T + DT = ( SIGF  X1F X2F PF RF QF(..))
!           DY     :  SOLUTION ESSAI     = ( DSIG  DX1 DX2 DP DR DQ(..))
!           NMOD   :  DIMENSION DECLAREE DRDY
!           EPSD   :  DEFORMATION A T
!           DEPS   :  INCREMENT DE DEFORMATION
!       OUT DRDY   :  JACOBIEN DU SYSTEME NON LINEAIRE
!       ----------------------------------------------------------------
!
#include "asterfort/chbfs.h"
#include "asterfort/chbfss.h"
#include "asterfort/chbfsx.h"
#include "asterfort/cvmcvx.h"
#include "asterfort/lcdima.h"
#include "asterfort/lcdive.h"
#include "asterfort/lceqma.h"
#include "asterfort/lceqve.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcicma.h"
#include "asterfort/lcinma.h"
#include "asterfort/lcinve.h"
#include "asterfort/lcopil.h"
#include "asterfort/lcopli.h"
#include "asterfort/lcprmm.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lcprsm.h"
#include "asterfort/lcprsv.h"
#include "asterfort/lcprte.h"
#include "asterfort/lcsoma.h"
#include "asterfort/lcsove.h"
    integer :: ndt, ndi, nmat, nmod
    integer :: ioptio, idnr, nopt
!
    real(kind=8) :: un, zero, d23, d13, mun
    parameter       ( un   =  1.d0      )
    parameter       ( mun  = -1.d0      )
    parameter       ( zero =  0.d0      )
    parameter       ( d23  =  2.d0/3.d0 )
    parameter       ( d13  = -1.d0/3.d0 )
!
    real(kind=8) :: hook(6, 6), ddfdds(6, 6), ddfdsx(6, 6), i6(6, 6)
    real(kind=8) :: deps(6), epsd(6), fkooh(6, 6), id(6, 6)
    real(kind=8) :: dfds(6)
    real(kind=8) :: sig(6), dsig(6)
    real(kind=8) :: x1(6), dx1(6), x2(6), dx2(6)
    real(kind=8) :: xxi(6), dxxi(6), p, dp
    real(kind=8) :: r, q
    real(kind=8) :: yf(*), dy(*), drdy(nmod, nmod)
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
    real(kind=8) :: materf(nmat, 2), seuil
    real(kind=8) :: timed, timef, dt
!
    real(kind=8) :: k0, ak, ar, n, alp, ww
    real(kind=8) :: b, mr, gr, mu, qm, q0
    real(kind=8) :: qr0, eta, ai
    real(kind=8) :: m1, d1, gx1, g10, c1
    real(kind=8) :: m2, d2, gx2, g20, c2
    real(kind=8) :: ccin, xx, yy, zz
    real(kind=8) :: grq, qr
    real(kind=8) :: c1d, c2d, difc1, difc2
    real(kind=8) :: vtmp(6), vtmp1(6), epsp(6), epxi(6)
    real(kind=8) :: dede3(6), mtmp(6, 6), mtmp1(6, 6)
    real(kind=8) :: x1df, jx1, x2df, jx2, dcin
    real(kind=8) :: jepxi, epxino(6), nnet
    integer :: n1, n2, n3, n4, n5, n6, n7, n8
!
    character(len=8) :: mod
!       ----------------------------------------------------------------
    common /tdim/   ndt , ndi
    common /opti/   ioptio , idnr
    common /coed/   c1d , c2d
!       ----------------------------------------------------------------
    data dede3      /zero   , zero  , mun   , zero , zero , zero/
    data  i6        /un     , zero  , zero  , zero  ,zero  ,zero,&
     &                   zero   , un    , zero  , zero  ,zero  ,zero,&
     &                   zero   , zero  , un    , zero  ,zero  ,zero,&
     &                   zero   , zero  , zero  , un    ,zero  ,zero,&
     &                   zero   , zero  , zero  , zero  ,un    ,zero,&
     &                   zero   , zero  , zero  , zero  ,zero  ,un/
    data id         / d23   , d13   , d13   , zero , zero , zero ,&
     &                    d13   , d23   , d13   , zero , zero , zero ,&
     &                    d13   , d13   , d23   , zero , zero , zero ,&
     &                    zero  , zero  , zero  , un   , zero , zero ,&
     &                    zero  , zero  , zero  , zero , un   , zero ,&
     &                    zero  , zero  , zero  , zero , zero , un /
!
    call lceqvn(ndt, yf(1), sig)
    call lceqvn(ndt, yf(ndt+1), x1)
    call lceqvn(ndt, yf(2*ndt+1), x2)
    p = yf(3*ndt+1)
    r = yf(3*ndt+2)
    q = yf(3*ndt+3)
    call lceqvn(ndt, dy(1), dsig)
    call lceqvn(ndt, dy(ndt+1), dx1)
    call lceqvn(ndt, dy(2*ndt+1), dx2)
    dp = dy(3*ndt+1)
!
    k0 = materf(1,2)
    ak = materf(2,2)
    ar = materf(3,2)
    n = materf(5,2)
    alp = materf(6,2)
    b = materf(7,2)
    mr = materf(8,2)
    gr = materf(9,2)
    mu = materf(10,2)
    qm = materf(11,2)
    q0 = materf(12,2)
    qr0 = materf(13,2)
    eta = materf(14,2)
    c1 = materf(15,2)
    m1 = materf(16,2)
    d1 = materf(17,2)
    gx1 = materf(18,2)
    g10 = materf(19,2)
    c2 = materf(20,2)
    m2 = materf(21,2)
    d2 = materf(22,2)
    gx2 = materf(23,2)
    g20 = materf(24,2)
    ai = materf(25,2)
!
    nopt = 0
    if (ioptio .eq. 2) nopt = idnr
!
    call lcopli('ISOTROPE', mod, materf(1, 1), hook)
    call chbfs(sig, x1, x2, dfds)
    call chbfss(sig, x1, x2, id, ddfdds)
    call chbfsx(sig, x1, x2, i6, ddfdsx)
!
    call cvmcvx(nmat, materf, sig, yf(ndt+1), seuil)
    if (seuil .lt. 0.d0) seuil = 0.d0
    ccin = ai + (1.d0-ai) * exp( -b*p )
    dcin = b * (ai-1.d0) * exp( -b*p )
    dt = timef - timed
!
!       ----------------------------------------------------------------
!       CALCUL DU JACOBIEN DU SYSTEME ( SIG  X1  X2  P  R  (EPS3) )
!       ----------------------------------------------------------------
!
! - DGDS(T+DT)
    call lcprmm(hook, ddfdds, dgds)
    call lcprsm(dp, dgds, dgds)
    call lcsoma(i6, dgds, dgds)
!
! - DGDX1(T+DT)
    call lcprmm(hook, ddfdsx, dgdx1)
    call lcprsm(dp, dgdx1, dgdx1)
!
! - DGDX2(T+DT)
    call lceqma(dgdx1, dgdx2)
!
! - DGDP(T+DT)
    if (seuil .lt. 0.d0) then
        call lcinve(0.d0, dgdp)
    else
        call lcprmv(hook, dfds, dgdp)
    endif
!
! - DGDR(T+DT)
    call lcinve(0.d0, dgdr)
!
! - DGDQ(T+DT)
    call lcinve(0.d0, dgdq)
!
!
! - DLDS(T+DT)
    call lcprsc(x1, x1, jx1)
    jx1 = sqrt( jx1 * 3.d0/2.d0)
    xx = c1 * dp * 2.d0/3.d0
    yy = g10 * ccin * (1.d0 - d1) * dp * 2.d0/3.d0
    if (jx1 .le. 0.d0) then
        zz= 1.d0 + dp * g10 * ccin * d1
        ww=0.d0
    else
        zz = 1.d0 + dp * g10 * ccin * d1 + dt * gx1 * jx1**(m1-1.d0)
        ww = gx1 * dt * (m1-1.d0) * jx1**(m1-3.d0) * 3.d0/2.d0
    endif
    call lcprmv(ddfdds, x1, vtmp)
    call lcprte(vtmp, dfds, mtmp)
    call lcprsc(x1, dfds, x1df)
    call lcprsm(x1df, ddfdds, mtmp1)
    call lcsoma(mtmp, mtmp1, mtmp)
    call lcprsm(yy, mtmp, dlds)
    call lcprsm(xx, ddfdds, mtmp)
    call lcdima(dlds, mtmp, dlds)
!
! - DLDX1(T+DT)
    call lcprmv(ddfdsx, x1, vtmp)
    call lcprte(vtmp, dfds, mtmp1)
    call lcprsm(x1df, ddfdsx, mtmp)
    call lcsoma(mtmp, mtmp1, mtmp1)
    call lcprte(dfds, dfds, mtmp)
    call lcsoma(mtmp, mtmp1, mtmp)
    call lcprsm(yy, mtmp, dldx1)
    call lcprte(x1, x1, mtmp)
    call lcprsm(ww, mtmp, mtmp)
    call lcsoma(dldx1, mtmp, dldx1)
    call lcprsm(zz, i6, mtmp)
    call lcsoma(dldx1, mtmp, dldx1)
    call lcprsm(xx, ddfdsx, mtmp)
    call lcdima(dldx1, mtmp, dldx1)
!
! - DLDX2(T+DT)
    call lcprsm(yy, mtmp1, dldx2)
    call lcdima(dldx2, mtmp, dldx2)
!
! -- CAS ANISOTHERME
    if (c1 .ne. 0.d0) then
        difc1 = (c1-c1d)/c1
        call lcprsm(difc1, i6, mtmp)
        call lcdima(dldx1, mtmp, dldx1)
    endif
    if (c2 .ne. 0.d0) then
        difc2 = (c2-c2d)/c2
        call lcprsm(difc2, i6, mtmp)
        call lcdima(dldx2, mtmp, dldx2)
    endif
!
! - DLDP(T+DT)
    yy = g10 * ( ccin + dcin * dp ) * d1
    zz = g10 * ( ccin + dcin * dp ) * (1.d0 - d1) * 2.d0/3.d0
    xx = x1df * zz - c1 * 2.d0/3.d0
    call lcprsv(xx, dfds, vtmp)
    call lcprsv(yy, x1, dldp)
    call lcsove(dldp, vtmp, dldp)
!
! - DLDR(T+DT)
    call lcinve(0.d0, dldr)
!
! - DLDQ(T+DT)
    call lcinve(0.d0, dldq)
!
!
! - DJDS(T+DT)
    call lcprsc(x2, x2, jx2)
    jx2 = sqrt( jx2 * 3.d0/2.d0)
    xx = c2 * dp * 2.d0/3.d0
    yy = g20 * ccin * (1.d0 - d2) * dp * 2.d0/3.d0
    if (jx2 .le. 0.d0) then
        zz= 1.d0 + dp * g20 * ccin * d2
        ww=0.d0
    else
        zz = 1.d0 + dp * g20 * ccin * d2 + dt * gx2 * jx2**(m2-1.d0)
        ww = gx2 * dt * (m2-1.d0) * jx2**(m2-3.d0) * 3.d0/2.d0
    endif
    call lcprmv(ddfdds, x2, vtmp)
    call lcprte(vtmp, dfds, mtmp)
    call lcprsc(x2, dfds, x2df)
    call lcprsm(x2df, ddfdds, mtmp1)
    call lcsoma(mtmp, mtmp1, mtmp)
    call lcprsm(yy, mtmp, djds)
    call lcprsm(xx, ddfdds, mtmp)
    call lcdima(djds, mtmp, djds)
!
! - DJDX2(T+DT)
    call lcprmv(ddfdsx, x2, vtmp)
    call lcprte(vtmp, dfds, mtmp1)
    call lcprsm(x2df, ddfdsx, mtmp)
    call lcsoma(mtmp, mtmp1, mtmp1)
    call lcprte(dfds, dfds, mtmp)
    call lcsoma(mtmp, mtmp1, mtmp)
    call lcprsm(yy, mtmp, djdx2)
    call lcprte(x2, x2, mtmp)
    call lcprsm(ww, mtmp, mtmp)
    call lcsoma(djdx2, mtmp, djdx2)
    call lcprsm(zz, i6, mtmp)
    call lcsoma(djdx2, mtmp, djdx2)
    call lcprsm(xx, ddfdsx, mtmp)
    call lcdima(djdx2, mtmp, djdx2)
!
! - DJDX1(T+DT)
    call lcprsm(yy, mtmp1, djdx1)
    call lcdima(djdx1, mtmp, djdx1)
!
! - DJDP(T+DT)
    yy = g20 * ( ccin + dcin * dp ) * d2
    zz = g20 * ( ccin + dcin * dp ) * (1.d0 - d2) * 2.d0/3.d0
    xx = x2df * zz - c2 * 2.d0/3.d0
    call lcprsv(xx, dfds, vtmp)
    call lcprsv(yy, x2, djdp)
    call lcsove(djdp, vtmp, djdp)
!
! - DJDR(T+DT)
    call lcinve(0.d0, djdr)
!
! - DJDQ(T+DT)
    call lcinve(0.d0, djdq)
!
!
! - DKDS(T+DT)
    xx = seuil / ( k0 + ak * r)
    if (xx .lt. 0.d0) xx = 0.d0
    zz = dt * (&
         (xx**(n-1.d0)) * (n + alp*(n+1)*xx**(n+1)) ) * exp( alp*(xx**(n+1)) ) / ( k0 + ak * r)
    call lcprsv(-zz, dfds, dkds)
!
! - DKDX1(T+DT)
    call lcprsv(zz, dfds, dkdx1)
!
! - DKDX2(T+DT)
    call lceqve(dkdx1, dkdx2)
!
! - DKDP(T+DT)
    dkdp = 1.d0
!
! - DKDR(T+DT)
!       DKDR = ZZ * (AR * K0 + SEUIL - AK * K) / (K0 + AK * R)
    dkdr = zz * (ar * (k0 + ak * r) + ak * seuil ) / (k0 + ak * r)
!
! - DKDQ(T+DT)
    dkdq = 0.d0
!
!
! - DRDS(T+DT)
    call lcinve(0.d0, drds)
!
! - DRDX1(T+DT)
    call lcinve(0.d0, drdx1)
!
! - DRDX2(T+DT)
    call lcinve(0.d0, drdx2)
!
! - DRDP(T+DT)
    grq = q0 + ( qm - q0 ) * ( 1.d0 - exp(-2.d0*mu*q) )
    qr = grq - qr0 * (1.d0 - ((qm-grq)/qm)**2)
    drdp = b * ( r - grq )
!
! - DRDR(T+DT)
    drdr = 1.d0 + b*dp + gr*dt*mr * (abs(qr - r))**(mr-1.d0)
!
! - DRDQ(T+DT)
    drdq = 0.d0
!
!
! - DTDS(T+DT)
    call lcinve(0.d0, dtds)
!
! - DTDX1(T+DT)
    call lcinve(0.d0, dtdx1)
!
! - DTDX2(T+DT)
    call lcinve(0.d0, dtdx2)
!
! - DTDP(T+DT)
    dtdp = 0.d0
!
! - DTDR(T+DT)
    dtdr = 0.d0
!
! - DTDQ(T+DT)
    dtdq = 1.d0
!
! - CONTRAINTES PLANES -------------------------------------------------
!
    if (mod(1:6) .eq. 'C_PLAN') then
!
! - DGDE3(T+DT)
        call lcprmv(hook, dede3, dgde3)
!
! - DLDE3(T+DT)
        call lcinve(0.d0, dlde3)
!
! - DJDE3(T+DT)
        call lcinve(0.d0, djde3)
!
! - DKDE3(T+DT)
        dkde3 = 0.d0
!
! - DRDE3(T+DT)
        drde3 = 0.d0
!
! - DTDE3(T+DT)
        dtde3 = 0.d0
!
! - DQDE3(T+DT)
        dqde3 = hook(3,3)
!
! - DQDS (T+DT)
        dqds(1) = - dp*(&
                  hook(3,3)*ddfdds(3,1) + hook(3,1)*ddfdds(1,1)+ hook(3,2)*ddfdds(2,1) + hook(3,4&
                  &)*ddfdds(4,1)&
                  )
        dqds(2) = - dp*(&
                  hook(3,3)*ddfdds(3,2) + hook(3,1)*ddfdds(1,2)+ hook(3,2)*ddfdds(2,2) + hook(3,4&
                  &)*ddfdds(4,2)&
                  )
        dqds(3) = - dp*(&
                  hook(3,3)*ddfdds(3,3) + hook(3,1)*ddfdds(1,3)+ hook(3,2)*ddfdds(2,3) + hook(3,4&
                  &)*ddfdds(4,3)&
                  )
        dqds(4) = - dp*(&
                  hook(3,3)*ddfdds(3,4) + hook(3,1)*ddfdds(1,4)+ hook(3,2)*ddfdds(2,4) + hook(3,4&
                  &)*ddfdds(4,4)&
                  )
!
! - DQDX1 (T+DT)
        dqdx1(1)= - dp*(hook(3,3)*ddfdsx(3,1) + hook(3,1)*ddfdsx(1,1)+&
        hook(3,2)*ddfdsx(2,1) + hook(3,4)*ddfdsx(4,1))
        dqdx1(2)= - dp*(hook(3,3)*ddfdsx(3,2) + hook(3,1)*ddfdsx(1,2)+&
        hook(3,2)*ddfdsx(2,2) + hook(3,4)*ddfdsx(4,2))
        dqdx1(3)= - dp*(hook(3,3)*ddfdsx(3,3) + hook(3,1)*ddfdsx(1,3)+&
        hook(3,2)*ddfdsx(2,3) + hook(3,4)*ddfdsx(4,3))
        dqdx1(4)= - dp*(hook(3,3)*ddfdsx(3,4) + hook(3,1)*ddfdsx(1,4)+&
        hook(3,2)*ddfdsx(2,4) + hook(3,4)*ddfdsx(4,4))
!
! - DQDX2 (T+DT)
        call lceqve(dqdx1, dqdx2)
!
! - DQDP (T+DT)
        dqdp = - hook(3,1)*dfds(1) - hook(3,2)*dfds(2) - hook(3,3)* dfds(3) - hook(3,4)*dfds(4)
!
! - DQDR (T+DT)
        dqdr = 0.d0
!
! - DQDQ (T+DT)
        dqdq = 0.d0
!
    endif
!
!
! - ASSEMBLAGE ---------------------------------------------------------
!
! - DRDY (T+DT) = ( DGDS  DGDX1  DGDX2  DGDP  DGDR  0   (DGDE3) )
!                 ( DLDS  DLDX1  DLDX2  DLDP  DLDR  0   (DLDE3) )
!                 ( DJDS  DJDX1  DJDX2  DJDP  DJDR  0   (DJDE3) )
!                 ( DKDS  DKDX1  DKDX2  DKDP  DKDR  0   (DKDE3) )
!                 ( DRDS  DRDX1  DRDX2  DRDP  DRDR  0   (DRDE3) )
!                 ( 0     0      0      0     0     1   (0)     )
!                 ((DQDS)(DQDX1)(DQDX2)(DQDP)(DQDR)(0)  (DQDE3) )
!
!
    n1 = 1
    n2 = ndt + 1
    n3 = 2*ndt + 1
    n4 = 3*ndt + 1
    n5 = 3*ndt + 2
    n6 = 3*ndt + 3
    n8 = 3*ndt + 4 + nopt
!
    call lcicma(dgds, 6, 6, ndt, ndt,&
                1, 1, drdy, nmod, nmod,&
                n1, n1)
    call lcicma(dgdx1, 6, 6, ndt, ndt,&
                1, 1, drdy, nmod, nmod,&
                n1, n2)
    call lcicma(dgdx2, 6, 6, ndt, ndt,&
                1, 1, drdy, nmod, nmod,&
                n1, n3)
    call lcicma(dgdp, 6, 1, ndt, 1,&
                1, 1, drdy, nmod, nmod,&
                n1, n4)
    call lcicma(dgdr, 6, 1, ndt, 1,&
                1, 1, drdy, nmod, nmod,&
                n1, n5)
    call lcicma(dgdq, 6, 1, ndt, 1,&
                1, 1, drdy, nmod, nmod,&
                n1, n6)
!
    call lcicma(dlds, 6, 6, ndt, ndt,&
                1, 1, drdy, nmod, nmod,&
                n2, n1)
    call lcicma(dldx1, 6, 6, ndt, ndt,&
                1, 1, drdy, nmod, nmod,&
                n2, n2)
    call lcicma(dldx2, 6, 6, ndt, ndt,&
                1, 1, drdy, nmod, nmod,&
                n2, n3)
    call lcicma(dldp, 6, 1, ndt, 1,&
                1, 1, drdy, nmod, nmod,&
                n2, n4)
    call lcicma(dldr, 6, 1, ndt, 1,&
                1, 1, drdy, nmod, nmod,&
                n2, n5)
    call lcicma(dldq, 6, 1, ndt, 1,&
                1, 1, drdy, nmod, nmod,&
                n2, n6)
!
    call lcicma(djds, 6, 6, ndt, ndt,&
                1, 1, drdy, nmod, nmod,&
                n3, n1)
    call lcicma(djdx1, 6, 6, ndt, ndt,&
                1, 1, drdy, nmod, nmod,&
                n3, n2)
    call lcicma(djdx2, 6, 6, ndt, ndt,&
                1, 1, drdy, nmod, nmod,&
                n3, n3)
    call lcicma(djdp, 6, 1, ndt, 1,&
                1, 1, drdy, nmod, nmod,&
                n3, n4)
    call lcicma(djdr, 6, 1, ndt, 1,&
                1, 1, drdy, nmod, nmod,&
                n3, n5)
    call lcicma(djdq, 6, 1, ndt, 1,&
                1, 1, drdy, nmod, nmod,&
                n3, n6)
!
    call lcicma(dkds, 1, 6, 1, ndt,&
                1, 1, drdy, nmod, nmod,&
                n4, n1)
    call lcicma(dkdx1, 1, 6, 1, ndt,&
                1, 1, drdy, nmod, nmod,&
                n4, n2)
    call lcicma(dkdx2, 1, 6, 1, ndt,&
                1, 1, drdy, nmod, nmod,&
                n4, n3)
    drdy(n4,n4) = dkdp
    drdy(n4,n5) = dkdr
    drdy(n4,n6) = dkdq
!
    call lcicma(drds, 1, 6, 1, ndt,&
                1, 1, drdy, nmod, nmod,&
                n5, n1)
    call lcicma(drdx1, 1, 6, 1, ndt,&
                1, 1, drdy, nmod, nmod,&
                n5, n2)
    call lcicma(drdx2, 1, 6, 1, ndt,&
                1, 1, drdy, nmod, nmod,&
                n5, n3)
    drdy(n5,n4) = drdp
    drdy(n5,n5) = drdr
    drdy(n5,n6) = drdq
!
    call lcicma(dtds, 1, 6, 1, ndt,&
                1, 1, drdy, nmod, nmod,&
                n6, n1)
    call lcicma(dtdx1, 1, 6, 1, ndt,&
                1, 1, drdy, nmod, nmod,&
                n6, n2)
    call lcicma(dtdx2, 1, 6, 1, ndt,&
                1, 1, drdy, nmod, nmod,&
                n6, n3)
    drdy(n6,n4) = dtdp
    drdy(n6,n5) = dtdr
    drdy(n6,n6) = dtdq
!
    if (mod(1:6) .eq. 'C_PLAN') then
!
        call lcicma(dgde3, 6, 1, ndt, 1,&
                    1, 1, drdy, nmod, nmod,&
                    n1, n8)
        call lcicma(dlde3, 6, 1, ndt, 1,&
                    1, 1, drdy, nmod, nmod,&
                    n2, n8)
        call lcicma(djde3, 6, 1, ndt, 1,&
                    1, 1, drdy, nmod, nmod,&
                    n3, n8)
        drdy(n4,n8) = dkde3
        drdy(n5,n8) = drde3
        drdy(n6,n8) = dtde3
!
        call lcicma(dqds, 1, 6, 1, ndt,&
                    1, 1, drdy, nmod, nmod,&
                    n8, n1)
        call lcicma(dqdx1, 1, 6, 1, ndt,&
                    1, 1, drdy, nmod, nmod,&
                    n8, n2)
        call lcicma(dqdx2, 1, 6, 1, ndt,&
                    1, 1, drdy, nmod, nmod,&
                    n8, n3)
        drdy(n8,n4) = dqdp
        drdy(n8,n5) = dqdr
        drdy(n8,n6) = dqdq
        drdy(n8,n8) = dqde3
    endif
!
!       ----------------------------------------------------------------
!       CALCUL DU JACOBIEN DU SYSTEME (SIG  X1  X2  P  R  Q XXI (EPS3))
!       ----------------------------------------------------------------
!
    if (ioptio .eq. 2) then
!
        call lceqvn(ndt, yf(3*ndt+4), xxi)
        call lceqvn(ndt, dy(3*ndt+4), dxxi)
!
! - DGDQ(T+DT)
        call lcinve(0.d0, dgdq)
!
! - DLDQ(T+DT)
        call lcinve(0.d0, dldq)
!
! - DJDQ(T+DT)
        call lcinve(0.d0, djdq)
!
! - DKDQ(T+DT)
        dkdq = 0.d0
!
! - DRDQ(T+DT)
        xx = gr * mr * ( (abs(qr-r))**(mr-1.d0)) * ( 1.d0 -2.d0 * (qm- grq) * qr0 / (qm**2) )
        drdq = (q0-qm) *2.d0 * mu * exp( -2.d0*mu*q ) * ( b * dp + xx * dt)
!
! - DTDQ(T+DT)
        dtdq = 1.d0
!
!
! - DGDXXI(T+DT)
        call lcinma(0.d0, dgdxxi)
!
! - DLDXXI(T+DT)
        call lcinma(0.d0, dldxxi)
!
! - DJDXXI(T+DT)
        call lcinma(0.d0, djdxxi)
!
! - DKDXXI(T+DT)
        call lcinve(0.d0, dkdxxi)
!
! - DRDXXI(T+DT)
        call lcinve(0.d0, drdxxi)
!
!
!
! ---- EPSP
        call lcopil('ISOTROPE', mod, materf(1, 1), fkooh)
        call lcprmv(fkooh, sig, vtmp)
        call lcsove(epsd, deps, epsp)
        call lcdive(epsp, vtmp, epsp)
! ---- JEPXI
        call lcdive(epsp, xxi, epxi)
        call lcprsc(epxi, epxi, xx)
        jepxi = sqrt( xx * 3.d0/2.d0 )
!
! --- H(F)=SEUIL2
!
!           SEUIL2 = 2.D0/3.D0 * JEPXI - Q
! --- NNET
!
        if (jepxi .eq. 0.d0) then
            nnet = 0.d0
        else
            call lcprsv(1.d0/jepxi, epxi, epxino)
            call lcprsc(dfds, epxino, nnet)
        endif
!
! -MEMORISATION
!
        if (jepxi .gt. 0.d0) then
!
            zz = -eta / jepxi
            xx = zz * dp
            call lcdive(epsp, xxi, vtmp)
            call lcprsv(dp, dfds, vtmp1)
            call lcsove(vtmp1, vtmp, vtmp)
            yy = -dp * nnet * (3.d0/2.d0) / jepxi
            call lcprsv(yy, epxi, vtmp1)
            call lcsove(vtmp1, vtmp, vtmp)
            call lcprsv(xx, vtmp, vtmp1)
!
! - DTDS(T+DT)
!
            call lcprmv(ddfdds, vtmp1, dtds)
!
! - DTDX1(T+DT)
            call lcprmv(ddfdsx, vtmp1, dtdx1)
!
! - DTDX2(T+DT)
            call lceqve(dtdx1, dtdx2)
!
! - DTDP(T+DT)
!
            call lcprsv(zz, vtmp, vtmp)
            call lcprsc(dfds, vtmp, dtdp)
!
! - DTDXXI(T+DT)
            yy = -nnet * (3.d0/2.d0) / jepxi
            call lcprsv(yy, epxi, vtmp)
            call lcsove(vtmp, dfds, vtmp)
            call lcprsv(-xx, vtmp, dtdxxi)
!
!
! - DXIDS(T+DT)
            zz = 3.d0/2.d0 * ( 1.d0 - eta ) * ( 1.d0 - dp * nnet / jepxi * 3.d0 )
            xx = 3.d0/2.d0 * ( 1.d0 - eta ) * dp / jepxi
            call lcprsv(-zz*dp, epxi, vtmp)
            call lcprsv(-xx*dp, dfds, vtmp1)
            call lcsove(vtmp, vtmp1, vtmp)
            call lcprmv(ddfdds, vtmp, vtmp1)
            call lcprte(vtmp1, epxi, mtmp)
            call lcprsm(-xx*dp*nnet, ddfdds, mtmp1)
            call lcsoma(mtmp1, mtmp, dxids)
!
! - DXIDX1(T+DT)
            call lcprmv(ddfdsx, vtmp, vtmp1)
            call lcprte(vtmp1, epxi, mtmp)
            call lcprsm(-xx*dp*nnet, ddfdsx, mtmp1)
            call lcsoma(mtmp1, mtmp, dxidx1)
!
! - DXIDX2(T+DT)
            call lceqma(dxidx1, dxidx2)
!
! - DXIDP(T+DT)
            call lcprsc(dfds, dfds, yy)
            call lcprsv(zz*nnet+xx*yy, epxi, vtmp)
            call lcprsv(-xx*nnet, dfds, vtmp1)
            call lcdive(vtmp1, vtmp, dxidp)
!
! - DXIDXI(T+DT)
            call lcprsm(1.d0+xx*nnet, i6, mtmp)
            call lcprsv(3.d0*xx*nnet, epxi, vtmp)
            call lcprsv(xx, dfds, vtmp1)
            call lcdive(vtmp1, vtmp, vtmp)
            call lcprte(vtmp, epxi, mtmp1)
            call lcsoma(mtmp1, mtmp, dxidxi)
!
        else
            call lcinma(0.d0, dgdxxi)
!
! - DTDS(T+DT)
            call lcinve(0.d0, dtds)
!
! - DTDX1(T+DT)
            call lcinve(0.d0, dtdx1)
!
! - DTDX2(T+DT)
            call lcinve(0.d0, dtdx2)
!
! - DTDP(T+DT)
            dtdp = 0.d0
!
! - DTDXXI(T+DT)
            call lcinve(0.d0, dtdxxi)
!
! - DXIDS(T+DT)
            call lcinma(0.d0, dxids)
!
! - DXIDX1(T+DT)
            call lcinma(0.d0, dxidx1)
!
! - DXIDX2(T+DT)
            call lcinma(0.d0, dxidx2)
!
! - DXIDP(T+DT)
            call lcinve(0.d0, dxidp)
!
! - DXIDXI(T+DT)
            call lcinma(0.d0, dxidxi)
!
        endif
!
! - DTDR(T+DT)
        dtdr = 0.d0
!
! - DTDQ(T+DT)
        dtdq = 1.d0
!
! - DXIDR(T+DT)
        call lcinve(0.d0, dxidr)
!
! - DXIDQ(T+DT)
        call lcinve(0.d0, dxidq)
!
!
        if (mod(1:6) .eq. 'C_PLAN') then
!
! - DQDXXI(T+DT)
            call lcinve(0.d0, dqdxxi)
!
! - DXIDE3(T+DT)
            call lcinve(0.d0, dxide3)
!
! - DQDQ(T+DT)
            dqdq = 0.d0
!
! - DTDE3(T+DT)
            dtde3 = 0.d0
        endif
!
!
! - ASSEMBLAGE ---------------------------------------------------------
!
!       DRDY  = (                                 DGDQ  DGDXXI (DGDE3) )
!               (         (...)                   DLDQ  DLDXXI (DLDE3) )
!               (         (...)                   DJDQ  DJDXXI (DJDE3) )
!               (                                 DKDQ  DKDXXI (DKDE3) )
!               (                                 DRDQ  DRDXXI (DRDE3) )
!               ( DTDS  DTDX1  DTDX2  DTDP  DTDR  DTDQ  DTDXXI (DTDE3) )
!               ( DXIDS DXIDX1 DXIDX2 DXIDP DXIDR DXIDQ DXIDXI(DXIDE3))
!               ((DQDS)(DQDX1)(DQDX2)(DQDP)(DQDR)(DQDQ)(DQDXXI)(DQDE3) )
!
!
        n7 = 3*ndt + 4
!
        call lcicma(dgdq, 6, 1, ndt, 1,&
                    1, 1, drdy, nmod, nmod,&
                    n1, n6)
        call lcicma(dgdxxi, 6, 6, ndt, ndt,&
                    1, 1, drdy, nmod, nmod,&
                    n1, n7)
!
        call lcicma(dldq, 6, 1, ndt, 1,&
                    1, 1, drdy, nmod, nmod,&
                    n2, n6)
        call lcicma(dldxxi, 6, 6, ndt, ndt,&
                    1, 1, drdy, nmod, nmod,&
                    n2, n7)
!
        call lcicma(djdq, 6, 1, ndt, 1,&
                    1, 1, drdy, nmod, nmod,&
                    n3, n6)
        call lcicma(djdxxi, 6, 6, ndt, ndt,&
                    1, 1, drdy, nmod, nmod,&
                    n3, n7)
!
        drdy(n4,n6) = dkdq
        call lcicma(dkdxxi, 1, 6, 1, ndt,&
                    1, 1, drdy, nmod, nmod,&
                    n4, n7)
!
        drdy(n5,n6) = drdq
        call lcicma(drdxxi, 1, 6, 1, ndt,&
                    1, 1, drdy, nmod, nmod,&
                    n5, n7)
!
        call lcicma(dtds, 1, 6, 1, ndt,&
                    1, 1, drdy, nmod, nmod,&
                    n6, n1)
        call lcicma(dtdx1, 1, 6, 1, ndt,&
                    1, 1, drdy, nmod, nmod,&
                    n6, n2)
        call lcicma(dtdx2, 1, 6, 1, ndt,&
                    1, 1, drdy, nmod, nmod,&
                    n6, n3)
        drdy(n6,n4) = dtdp
        drdy(n6,n5) = dtdr
        drdy(n6,n6) = dtdq
        call lcicma(dtdxxi, 1, 6, 1, ndt,&
                    1, 1, drdy, nmod, nmod,&
                    n6, n7)
!
        call lcicma(dxids, 6, 6, ndt, ndt,&
                    1, 1, drdy, nmod, nmod,&
                    n7, n1)
        call lcicma(dxidx1, 6, 6, ndt, ndt,&
                    1, 1, drdy, nmod, nmod,&
                    n7, n2)
        call lcicma(dxidx2, 6, 6, ndt, ndt,&
                    1, 1, drdy, nmod, nmod,&
                    n7, n3)
        call lcicma(dxidp, 6, 1, ndt, 1,&
                    1, 1, drdy, nmod, nmod,&
                    n7, n4)
        call lcicma(dxidr, 6, 1, ndt, 1,&
                    1, 1, drdy, nmod, nmod,&
                    n7, n5)
        call lcicma(dxidq, 6, 1, ndt, 1,&
                    1, 1, drdy, nmod, nmod,&
                    n7, n6)
        call lcicma(dxidxi, 6, 6, ndt, ndt,&
                    1, 1, drdy, nmod, nmod,&
                    n7, n7)
!
        if (mod(1:6) .eq. 'C_PLAN') then
            drdy(n8,n6) = dqdq
            call lcicma(dqdxxi, 1, 6, 1, ndt,&
                        1, 1, drdy, nmod, nmod,&
                        n8, n7)
            call lcicma(dxide3, 6, 1, ndt, 1,&
                        1, 1, drdy, nmod, nmod,&
                        n7, n8)
            drdy(n6,n8) = dtde3
        endif
!
    endif
!
end subroutine

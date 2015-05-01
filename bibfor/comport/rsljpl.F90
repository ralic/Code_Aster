subroutine rsljpl(fami, kpg, ksp, loi, imat,&
                  nmat, mater, sig, vin, vind,&
                  deps, theta, dt, dsde)
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
!       ROUSSELIER :  MATRICE SYMETRIQUE DE COMPORTEMENT TANGENT
!            COHERENT ELASTO-PLASTIQUE EN VITESSE A T+DT OU T
!       IN  FAMI   :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!           KPG,KSP:  NUMERO DU (SOUS)POINT DE GAUSS
!           IMAT   :  ADRESSE DU MATERIAU CODE
!           NMAT   :  DIMENSION MATER
!           MATER  :  COEFFICIENTS MATERIAU
!           SIG    :  CONTRAINTES
!           VIN    :  VARIABLES INTERNES
!       OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
!       ----------------------------------------------------------------
#include "asterfort/lchydr.h"
#include "asterfort/lcinma.h"
#include "asterfort/lcnrte.h"
#include "asterfort/lcnrts.h"
#include "asterfort/lcprsm.h"
#include "asterfort/lcprsv.h"
#include "asterfort/lcprte.h"
#include "asterfort/lcsoma.h"
#include "asterfort/lcsomh.h"
#include "asterfort/lcsove.h"
#include "asterfort/rsliso.h"
    integer :: kpg, ksp, imat, nmat
!
    real(kind=8) :: v1(6), v2(6), i2(6)
    real(kind=8) :: m1(6, 6), m2(6, 6), m3(6, 6), dsde(6, 6), i4(6, 6)
    real(kind=8) :: vin(*), vind(*)
    real(kind=8) :: deps(6), sig(6), rig(6), rigdv(6)
    real(kind=8) :: rigmo, rigeq, rigeq2, eps0, sig0, mexpo, acc
    real(kind=8) :: z1, z2, z3, z4, z5, z6, z7, z8, z9, expo, expe
    real(kind=8) :: x1, x2, y1, y2, y3, y4, y5, a1, a2, a3, a4
    real(kind=8) :: a5, a6, d, s1, f, fd, f0, p, rho, unf
    real(kind=8) :: nu, e, deuxmu, mu, troimu, troisk, k
    real(kind=8) :: mater(nmat, 2), rp, drdp, dp, dpm
    real(kind=8) :: zero, un, deux, trois, ann, pd, ftot, fdtot
    real(kind=8) :: ndeps, nsig, theta
    real(kind=8) :: dpuiss, puiss, dt
!
    character(len=16) :: loi
    character(len=*) :: fami
!
    parameter       ( zero  = 0.d0   )
    parameter       ( un    = 1.d0   )
    parameter       ( deux  = 2.d0   )
    parameter       ( trois = 3.d0   )
!
    data  i4        /un     , zero  , zero  , zero  ,zero  ,zero,&
     &                   zero   , un    , zero  , zero  ,zero  ,zero,&
     &                   zero   , zero  , un    , zero  ,zero  ,zero,&
     &                   zero   , zero  , zero  , un    ,zero  ,zero,&
     &                   zero   , zero  , zero  , zero  ,un    ,zero,&
     &                   zero   , zero  , zero  , zero  ,zero  ,un/
    data i2         /un     , un    , un    , zero  ,zero  ,zero/
!       ----------------------------------------------------------------
!
! -- INITIALISATION----------------------------------------------
!
    p = vin(1)
    f = vin(2)
    pd = vind(1)
    fd = vind(2)
    e = mater(1,1)
    nu = mater(2,1)
    d = mater(1,2)
    s1 = mater(2,2)
    f0 = mater(3,2)
    if (loi(1:10) .eq. 'ROUSS_VISC') then
        ann = 0.d0
        sig0 = mater(9,2)
        eps0 = mater(10,2)
        mexpo = mater(11,2)
    else if (loi(1:10).eq.'ROUSS_PR') then
        ann = mater(8,2)
        sig0 = 0.d0
        eps0 = 0.d0
        mexpo = 0.d0
    endif
    ftot = f + ann*p
    fdtot = fd + ann*pd
!
!
! -- CAS DU MATERIAU CASSE---------------------------------------
    if (fdtot .ge. mater(6,2)) then
        ndeps = lcnrte(deps)
        nsig = lcnrts(sig )
        if ((ndeps*nsig) .eq. zero) then
            call lcinma(zero, dsde)
        else
            a1 = -deux*mater(7,2)*e/(ndeps*nsig*trois)
            call lcprte(sig, deps, dsde)
            call lcprsm(a1, dsde, dsde)
        endif
!
! -- CAS DU MATERIAU NON CASSE-----------------------------------
    else
!
        call rsliso(fami, kpg, ksp, '-', imat,&
                    p, rp, drdp)
!
        unf= un-f
        rho = (unf-ann*p)/(un-f0)
        call lcprsv(un/rho, sig, rig)
        call lchydr(rig, rigmo)
        call lcsomh(rig, -rigmo, rigdv)
        rigeq = lcnrts(rigdv)
        rigeq2= rigeq*rigeq
!
        deuxmu = e/(un+nu)
        mu = deuxmu/deux
        troimu = trois*mu
        troisk = e/(un-deux*nu)
        k = troisk/trois
!
! -- SI LA POROSITE EST ACCELERE----
        if (fdtot .ge. mater(4,2)) then
            acc = mater(5,2)
! -- SINON-----------------------
        else
            acc = un
        endif
! -- ----------------------------
!
        dp = p-vind(1)
        if (dp .gt. zero) then
            dpm = theta*(f-fd)/(trois*unf*acc)
            expo = d*exp(rigmo/s1)
            expe = expo*s1*ftot
!
            if (loi(1:10) .eq. 'ROUSS_VISC') then
                puiss = (dp/(dt*eps0))**(un/mexpo)
                dpuiss = ((dp/(dt*eps0))**(un/mexpo-un))/(mexpo*(dt* eps0))
                drdp = drdp + sig0*dpuiss/sqrt(un+puiss**2)/theta
            endif
!
            z1 = un+trois*dpm*acc
            z2 = troimu + drdp
            z3 = k*ftot*z1 - s1*unf*acc
            z4 = dp*theta*drdp - rigeq
            z5 = troimu*theta*dp + rigeq
!
!
            x1 = expe*expo*z3 + expo*z2*z3*theta*dp + z1*z2*s1 - ann*z1*expo*s1*s1
            x2 = -(expe*expo*z3*dp + expo*z3*z4*theta*dp + z1*s1*z4 ) + ann*z1*expo*s1*s1*dp
!
            y1 = -troisk*expo*z1*ftot/(x1*rigeq)
            y2 = -troimu/(x1*z5*rigeq2)
            y3 = -troisk*expo*z1*ann*theta*dp/(x1*rigeq)
!
            a1 = troisk + y1*k*rigeq*(expe+dp*theta*z2)
            a2 = mu*(y1+y3)*s1
            a3 = deuxmu*(un+y2*x1*dp*theta*rigeq2)
            a4 = y2*troimu*x2
            a5 = y2*troisk*expe*z1*z5*rigeq
!
            z6 = expo
            z7 = z6*s1*ftot
            z8 = unf / (unf - ann*p)
            z9 = ann / ( unf - ann*p)
            a6 = troimu*k*theta*dp - a2*rigeq*s1
            y4 = acc*z8/z1 + z9*s1/(z7+z2*theta*dp)
            y5 = acc*a2*z8/z1 - z9*a6 / ( rigeq*(z7+z2*theta*dp) )
!
            call lcprsm(a3, i4, m1)
            call lcprsv((a1-a3)/trois, i2, v1)
            call lcprsv(a2, rigdv, v2)
            call lcsove(v1, v2, v1)
            call lcprte(i2, v1, m2)
            call lcprsv(a4, rigdv, v1)
            call lcprsv(a5/trois, i2, v2)
            call lcsove(v1, v2, v1)
            call lcprte(rigdv, v1, m3)
            call lcsoma(m1, m2, dsde)
            call lcsoma(m3, dsde, dsde)
!
! A CE STADE DSDE EST LE TENSEUR TANGENT COHERENT
! ENTRE D(SIG/RHO) ET DEPS
!
            call lcprsv(a1-troisk, i2, v1)
            call lcprsv(trois*y5/y4, rigdv, v2)
            call lcsove(v1, v2, v1)
            call lcprte(rig, v1, m1)
            call lcprsm(y4/troisk, m1, m1)
            call lcsoma(dsde, m1, dsde)
            call lcprsm(rho, dsde, dsde)
! -- CAS DP=0
        else
            call lcprte(i2, i2, m1)
            call lcprsm(un/trois, m1, m1)
            call lcprsm(rho*troisk, m1, m2)
            call lcprsm(-un, m1, m1)
            call lcsoma(i4, m1, m1)
            call lcprsm(rho*deuxmu, m1, m1)
            call lcsoma(m1, m2, dsde)
        endif
    endif
! ------------------------------------------------------------------
end subroutine

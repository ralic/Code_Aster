subroutine irrres(fami, kpg, ksp, mod, nmat,&
                  materd, materf, yd, yf, deps,&
                  dy, r)
    implicit none
!
#include "asterc/r8prem.h"
#include "asterfort/lcdevi.h"
#include "asterfort/lcdive.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcinve.h"
#include "asterfort/lcnrts.h"
#include "asterfort/lcopil.h"
#include "asterfort/lcopli.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lcprsv.h"
#include "asterfort/lcsove.h"
    character(len=8) :: mod
    character(len=*) :: fami
    integer :: nmat, kpg, ksp
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: yd(*), yf(*), deps(6), dy(*), r(*)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-luc.flejou at edf.fr
!
    real(kind=8) :: dfds(6), id3d(6), sf
    real(kind=8) :: irrad, irraf, dphi, sigd(6), sigf(6), dkooh(6, 6)
    real(kind=8) :: fkooh(6, 6), hookf(6, 6), k, n, p0, ai0, etais, p, zetaff
    real(kind=8) :: zetag
    real(kind=8) :: dp, detai, dpi, dg, etaif, seqf, epsef(6), pe, kappa, r02
    real(kind=8) :: depsa(6), depsg(6), dev(6), epsed(6)
    real(kind=8) :: rs(6), rp, re, rpi, rg, qf, r8aux
    real(kind=8) :: pk, penpe, spe, seqd
    real(kind=8) :: etaid, agdi, agfi
    integer :: ndt, ndi
!     ----------------------------------------------------------------
    common /tdim/   ndt , ndi
!     ----------------------------------------------------------------
!
    data id3d /1.0d0, 1.0d0, 1.0d0, 0.0d0, 0.0d0, 0.0d0/
!
    call lceqvn(ndt, yf(1), sigf)
    call lceqvn(ndt, yd(1), sigd)
    call lcopil('ISOTROPE', mod, materd(1, 1), dkooh)
    call lcopil('ISOTROPE', mod, materf(1, 1), fkooh)
    call lcopli('ISOTROPE', mod, materf(1, 1), hookf)
!
!     RECUPERATION DES CARACTERISTIQUES MATERIAUX A t+
    ai0 = materf(4,2)
    etais = materf(5,2)
    k = materf(7,2)
    n = materf(8,2)
    p0 = materf(9,2)
    kappa = materf(10,2)
    r02 = materf(11,2)
    zetaff= materf(12,2)
    penpe = materf(13,2)
    pk = materf(14,2)
    pe = materf(15,2)
    spe = materf(16,2)
    zetag = materf(17,2)
!     RECUPERATION DES CARACTERISTIQUES MATERIAUX A t-
!     RECUPERATION GONFLEMENT DEJA INTEGRE
    agdi = materd(19,2)
    agfi = materf(19,2)
!
!     RECUPERATION DES VARIABLES INTERNES A t+
    p = yf(ndt+1)
    etaif = yf(ndt+2)
!     RECUPERATION DES VARIABLES INTERNES A t-
    etaid = yd(ndt+2)
!
!     RECUPERATION DES INCREMENTS DES VARIABLES INTERNES
    dp = dy(ndt+1)
    detai = dy(ndt+2)
    dpi = dy(ndt+3)
    dg = dy(ndt+4)
!
!     RECUPERATION DE L'IRRADIATION
    irrad = materd(18,2)
    irraf = materf(18,2)
    dphi=irraf-irrad
!
    call lcdevi(sigf, dev)
    seqf = lcnrts(dev)
    if (seqf .eq. 0.0d0) then
        call lcinve(0.0d0, dfds)
    else
        call lcprsv(1.5d0/seqf, dev, dfds)
    endif
    call lcprmv(fkooh, sigf, epsef)
    call lcprmv(dkooh, sigd, epsed)
    call lcprsv((dp+dpi), dfds, depsa)
    call lcprsv(dg, id3d, depsg)
!
!   RESIDU EN SIGMA, HOMOGENE A DES DEFORMATIONS
    call lcdive(epsef, epsed, rs)
    call lcsove(rs, depsa, rs)
    call lcsove(rs, depsg, rs)
    call lcdive(rs, deps, rs)
    call lcprsv(-1.d0, rs, rs)
!
!  RESIDU EN DEFORMATION PLASTIQUE
    if (p .lt. pk) then
        sf = kappa*r02
    else if (p .lt. pe) then
        sf = penpe*(p - pe) + spe
    else
        sf = k*(p+p0)**n
    endif
    if (((seqf.ge.sf).and.(dp.ge.0.d0)) .or. (dp.gt.r8prem())) then
        rp = -(seqf-sf)/hookf(1,1)
    else
        rp = -dp
    endif
!
!     CONTRAINTE EQUIVALENTE A T-
    call lcdevi(sigd, dev)
    seqd = lcnrts(dev)
!
!  RESIDU EN DEFORMATION D IRRADIATION
    materf(21,2) = 0.0d0
    r8aux = 0.0d0
    if (etaid .gt. etais) then
        rpi = -( dpi - ai0*detai )
        r8aux = zetaff*(seqd+seqf)*dphi*0.5d0
    else if (etaif .le. etais) then
        rpi = - dpi
        r8aux = zetaff*(seqd+seqf)*dphi*0.5d0
    else if (detai .gt. 0.0d0) then
        rpi = -( dpi - ai0*(etaif-etais) )
        r8aux = (seqd+seqf)*dphi*0.5d0
        if (seqd .gt. 0.0d0) then
            r8aux = r8aux - (seqf-seqd)*(etais-etaid)/(2.0d0*seqd)
        endif
        r8aux = r8aux*zetaff
!        INDICATEUR DE FRANCHISSEMENT DU SEUIL ETAIS
        if (etais .gt. 0.0d0) then
            materf(21,2) = (etaif-etais)/etais
        endif
    else
        rpi = - dpi
    endif
!
!  RESIDU PAR RAPPORT A ETA, HOMOGENE A DES DEFORMATIONS : RE
    re = -(detai-r8aux)/hookf(1,1)
!
!  RESIDU PAR RAPPORT AU GONFLEMENT
    if (dphi .gt. 0.0d0) then
        rg= -( dg - zetag*(agfi - agdi) )
    else
        rg = - dg
    endif
!
!  RESIDU PAR RAPPORT A LA DEFORMATION ( C_PLAN )
    if (mod(1:6) .eq. 'C_PLAN') then
        qf = (&
             -hookf(3, 3)*epsef(3) -hookf(3, 1)*epsef(1) -hookf(3, 2)* epsef(2) -hookf(3, 4)*epse&
             &f(4))/hookf(1,&
             1&
             )
    endif
!
    call lceqvn(ndt, rs, r(1))
    r(ndt+1)=rp
    r(ndt+2)=re
    r(ndt+3)=rpi
    r(ndt+4)=rg
    if (mod(1:6) .eq. 'C_PLAN') r(ndt+5) = qf
!
end subroutine

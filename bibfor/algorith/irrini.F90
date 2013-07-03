subroutine irrini(fami, kpg, ksp, typess, essai,&
                  mod, nmat, materf, yd, deps,&
                  dy)
!
    implicit none
!
#include "asterfort/lcdevi.h"
#include "asterfort/lcdive.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcnrve.h"
#include "asterfort/lcopli.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lcprsv.h"
#include "asterfort/rcvarc.h"
#include "asterfort/vecini.h"
    integer :: typess, nmat, kpg, ksp
    real(kind=8) :: essai, materf(nmat, 2), yd(*), deps(6), dy(*)
    character(len=8) :: mod
    character(len=*) :: fami
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
!       IRRAD3M : CALCUL SOLUTION ESSAI DY = ( DSIG DX1 DX2 DP (DEPS3))
!                               AVEC     Y  = ( SIG  X1  X2  P  (EPS3))
!       IN  ESSAI  :  VALEUR DE LA SOLUTION D ESSAI
!           MOD    :  TYPE DE MODELISATION
!           NMAT   :  DIMENSION MATER
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!           YD     :  VARIABLES A T   = ( SIG  VIN  (EPS3)  )
!       VAR DEPS   :  INCREMENT DE DEFORMATION
!           TYPESS :  TYPE DE SOLUTION D ESSAI
!                               0 = NUL(0)
!                               1 = ELASTIQUE
!                               2 = EXPLICITE (=-1 INITIALEMENT)
!                               3 = ESSAI
!       OUT DY     :  SOLUTION ESSAI  = ( DSIG DVIN (DEPS3) )
!     ----------------------------------------------------------------
    common /tdim/   ndt , ndi
!     ----------------------------------------------------------------
    real(kind=8) :: hook(6, 6), dev(6), s, dfds(6), vtmp1(6), vtmp2(6), dsig(6)
    real(kind=8) :: dphi, id3d(6), nun, sig(6), p, etai
    real(kind=8) :: k, n, p0, ai0, etais, ag, irrad, irraf, zetaf, zetag
    real(kind=8) :: detai, dpi, dp, dg, yy, xx, zz
    real(kind=8) :: penpe, pe, pk
    integer :: ndt, ndi, iret, i
    data id3d /1.d0, 1.d0, 1.d0, 0.d0, 0.d0, 0.d0/
!
    if (typess .eq. -1) typess = 2
    call lceqvn(ndt, yd(1), sig)
    p = yd(ndt+1)
    etai = yd(ndt+2)
!
!     PARAMETRES MATERIAUX
    ai0 = materf(4,2)
    etais = materf(5,2)
    ag = materf(6,2)
    k = materf(7,2)
    n = materf(8,2)
    p0 = materf(9,2)
    zetaf = materf(12,2)
    penpe = materf(13,2)
    pk = materf(14,2)
    pe = materf(15,2)
    zetag = materf(17,2)
!
!     POUR LES CONTRAINTES PLANES
    nun = materf(2,1) / (1.d0-materf(2,1))
!
    typess=1
!     SOLUTION NULLE ( TYPESS=0) OU ELASTIQUE ( TYPESS=1)
    if (typess .eq. 0 .or. typess .eq. 1) then
        call vecini(ndt+4, 0.d0, dy)
        if (mod(1:6) .eq. 'C_PLAN') then
            deps(3) = 0.d0
            dy(ndt+5)=0.d0
        endif
!
        if (typess .eq. 1) then
            call lcopli('ISOTROPE', mod, materf(1, 1), hook)
            call lcprmv(hook, deps, dy)
        endif
!
!     SOLUTION EXPLICITE
    else if (typess.eq.2) then
        call lcopli('ISOTROPE', mod, materf(1, 1), hook)
        call rcvarc('F', 'IRRA', '-', fami, kpg,&
                    ksp, irrad, iret)
        call rcvarc('F', 'IRRA', '+', fami, kpg,&
                    ksp, irraf, iret)
!        ARRET DANS IRRMAT SI  IRRAD .GT. IRRAF*1.00001
        if (irrad .gt. irraf) then
            dphi = 0.0d0
        else
            dphi = irraf - irrad
        endif
!
        call lcdevi(sig, dev)
        call lcnrve(dev, s)
        s = sqrt ( 1.5d0 ) * s
!
!        DETAI
        detai=zetaf*s*dphi
!        DPI
        if ((etai+detai) .lt. etais) then
            dpi = 0.d0
        else if (etai.ge.etais) then
            dpi = ai0*detai
        else
            dpi = ai0*(detai - etais + etai)
        endif
!        DG
        dg=ag*dphi*zetag
!        DP
        if (s .eq. 0.d0) then
            dp = 0.d0
            do 10, i=1,6
            dfds(i) = 0.d0
10          continue
        else
            call lcprsv(1.5d0/s, dev, dfds)
            call lcprsv(dpi, dfds, vtmp1)
            call lcdive(deps, vtmp1, vtmp1)
            call lcprsv(dg, id3d, vtmp2)
            call lcdive(vtmp1, vtmp2, vtmp1)
            call lcprmv(hook, vtmp1, vtmp1)
            call lcprsc(dfds, vtmp1, yy)
!
            if (p .lt. pk) then
                zz = 0.0d0
            else if (p .lt. pe) then
                zz = penpe
            else
                zz = n*k*(p+p0)**(n-1.0d0)
            endif
            call lcprmv(hook, dfds, vtmp1)
            call lcprsc(dfds, vtmp1, xx)
!
            xx=xx+zz
!
            dp= yy/xx
        endif
!
!        (DEPS(3))
        if (mod(1:6) .eq. 'C_PLAN') then
            deps(3) = nun * (&
                      (dp+dpi)*(dfds(1)+dfds(2))+ 2.d0*dg-deps( 1)-deps(2))+ dfds(3)*(dp+dpi)+dg
        endif
!
!        DSIG
        call lcprsv((dpi+dp), dfds, vtmp1)
        call lcdive(deps, vtmp1, vtmp1)
        call lcprsv(dg, id3d, vtmp2)
        call lcdive(vtmp1, vtmp2, vtmp1)
        call lcprmv(hook, vtmp1, dsig)
!        DY
        call lceqvn(ndt, dsig, dy(1))
        dy(ndt+1)=dp
        dy(ndt+2)=detai
        dy(ndt+3)=dpi
        dy(ndt+4)=dg
        if (mod(1:6) .eq. 'C_PLAN') then
            dy(ndt+5) = deps(3)
            dy(3) = 0.d0
        endif
!
! - SOLUTION INITIALE = VALEUR ESSAI POUR TOUTES LES COMPOSANTES
!
    else if (typess .eq. 3) then
        call vecini(ndt+4, essai, dy)
        if (mod(1:6) .eq. 'C_PLAN') then
            deps(3) = essai
            dy(3) = 0.d0
        endif
    endif
!
end subroutine

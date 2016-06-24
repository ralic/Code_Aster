subroutine disc_isotr(pp, nbeq, yy0, dy0, dyy, decoup, pf)
    implicit none
#include "asterf_types.h"
    integer :: nbeq
    real(kind=8) :: pp(*), yy0(nbeq), dy0(nbeq), dyy(nbeq)
    aster_logical :: decoup
    integer, optional :: pf(*)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! --------------------------------------------------------------------------------------------------
!
!        DISCRET AVEC COMPORTEMENT ISOTROPE
!
!  IN
!     pp       : paramètres
!     nbeq     : nombre d'équations
!     yy0      : valeurs initiales
!     dy0      : dérivées initiales
!     pf       : informations sur les fonctions pf(3*nbfct) : (nbvale, jprol, jvale)
!
!  OUT
!     dyy      : dérivées calculées
!     decoup   : pour forcer l'adaptation du pas de temps
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: vfct,dfct,seuil,raidex
!
!   système d'équations : Force, Up, U , dissipation, P
    integer :: iforce,   iup,   iu,   idissi,   ip
    parameter (iforce=1, iup=2, iu=3, idissi=4, ip=5)
!
    decoup = .false.
    dyy(iu) = dy0(iu)
    raidex = pp(1)/pp(2)
!   La fonction seuil et sa dérivée
    call val_fct_dfct(pf,pp(2)+yy0(ip),vfct,dfct)
    seuil = abs(yy0(iforce)) - vfct
    if ( seuil .le. 0.0 ) then
        dyy(ip) = 0.0
    else
        dyy(ip) = abs(dyy(iu))
    endif
    if ( yy0(iforce) .ge. 0.0 ) then
        dyy(iup) =  dyy(ip)*(1.0-dfct/raidex)
    else
        dyy(iup) = -dyy(ip)*(1.0-dfct/raidex)
    endif
    dyy(iforce) = raidex*(dyy(iu) - dyy(iup))
!
    dyy(idissi) = yy0(iforce)*dyy(iup)
!
! ==================================================================================================
contains
!
subroutine val_fct_dfct(info_fct,p,vfct,dfct)
    integer :: info_fct(3)
    real(kind=8) :: p, vfct, dfct
!
#include "jeveux.h"
#include "asterfort/utmess.h"
!
    integer :: ip, i0, jr, jp, nbvale
!
    nbvale = info_fct(1)
    jp     = info_fct(3)
    jr     = jp+nbvale
!
    i0 = 0
    do ip = 0 , nbvale-1
        if ( p .lt. zr(jp+ip) ) then
            i0 = ip - 1
            if ( i0.lt.0 ) then
                i0 = 0
            endif
            goto 20
        endif
    enddo
    call utmess('F', 'DISCRETS_61', sk=zk24(info_fct(2)+5), sr=zr(jp+nbvale-1))
20  continue
    dfct = (zr(jr+i0+1)-zr(jr+i0))/(zr(jp+i0+1)-zr(jp+i0))
    vfct = zr(jr+i0) + dfct*(p-zr(jp+i0))
end subroutine val_fct_dfct
!
end subroutine disc_isotr

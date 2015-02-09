subroutine coqgth(imate, compor, fami, ipg, ep, epsm, deps)
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/r8inir.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvala.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
#include "asterfort/assert.h"

! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer, intent(in) :: imate
    character(len=16), intent(in) :: compor
    character(len=*), intent(in) :: fami
    integer, intent(in) :: ipg
    real(kind=8), intent(in) :: ep
    real(kind=8), intent(inout) :: epsm(6)
    real(kind=8), intent(inout) :: deps(6)
! ----------------------------------------------------------------------
!
! but : Prise en compte de la dilatation thermique pour les coques "globales"
!
! in:
!       imate   : adresse du materiau
!       compor  : comportment
!       fami    : famille de points de Gauss
!       ipg     : numero du point de Gauss
!       ep      : epaisseur de la coque
!       epsm    : deformation "-"
!       deps    : increment de deformation
! out:
!       epsm,deps : deformation - dilatation thermique
! ----------------------------------------------------------------------
    integer :: icodre(1), iret, iret1, iret2, i
    real(kind=8) :: valres(1)
    real(kind=8) :: t1m, t2m, t3m, t1p, t2p, t3p
    real(kind=8) :: epsth,khith,depsth,dkhith
    real(kind=8) :: tref, dtmoy, dtgra, tmoyp, tmoym, tgrap, tgram, alphat
    character(len=16) :: nomres(1), phenom
! ----------------------------------------------------------------------


!   -- Y-a-t-il un chargement thermique ?
!   --------------------------------------
    call rcvarc(' ', 'TEMP_INF', '+', fami, ipg,&
                1, t2p, iret1)
    call rcvarc(' ', 'TEMP_SUP', '+', fami, ipg,&
                1, t3p, iret2)
    if (iret1.eq.0) then
        ASSERT(iret2.eq.0)
    else
        goto 999
    endif



!   -- temperature de reference :
!   ------------------------------
    call rcvarc('F', 'TEMP', 'REF', fami, 1,&
                1, tref, iret)


!   -- temperatures a t+ :
!   -----------------------
    call rcvarc(' ', 'TEMP', '+', fami, ipg,&
                1, t1p, iret)
!   -- si temp n'est pas fourni on met la moyenne de tsup et tinf
    if (iret .ne. 0) then
        t1p=(t2p+t3p)/2.d0
    endif

!   -- temperatures a t- :
!   ----------------------
    call rcvarc(' ', 'TEMP_INF', '-', fami, ipg,&
                1, t2m, iret)
    ASSERT(iret.eq.0)
    call rcvarc(' ', 'TEMP_SUP', '-', fami, ipg,&
                1, t3m, iret)
    ASSERT(iret.eq.0)

    call rcvarc(' ', 'TEMP', '-', fami, ipg,&
                1, t1m, iret)
    if (iret .ne. 0) then
        t1m=(t2m+t3m)/2.d0
    endif

!   -- calcul de tmoyp, tgradp, ... :
!   ---------------------------------
    tmoyp=(4.d0*t1p+t2p+t3p)/6.d0
    tgrap=(t3p-t2p)/ep
    tmoym=(4.d0*t1m+t2m+t3m)/6.d0
    tgram=(t3m-t2m)/ep
    dtmoy=tmoyp-tmoym
    dtgra=tgrap-tgram

!   -- recuperation de ALPHA :
!   --------------------------
    call rccoma(imate, 'ELAS', 1, phenom, icodre(1))
    ASSERT(icodre(1).eq.0)
    nomres(1) = 'ALPHA'
    call rcvala(imate, ' ', phenom, 1, 'TEMP',&
                [tmoyp], 1, nomres, valres, icodre,&
                1)
    ASSERT(icodre(1).eq.0)
    alphat = valres(1)

!
!   --  calcul de la deformation thermique :
!   ----------------------------------------
    epsth = alphat * (tmoym-tref)
    khith = alphat * tgram
    depsth = alphat * dtmoy
    dkhith = alphat * dtgra

!   --  modification de epsm et deps :
!   ---------------------------------
    do i = 1, 2
        epsm(i) = epsm(i) - epsth
        epsm(i+3) = epsm(i+3) - khith
    end do
    do i = 1, 2
        deps(i) = deps(i) - depsth
        deps(i+3) = deps(i+3) - dkhith
    end do


999 continue
end subroutine

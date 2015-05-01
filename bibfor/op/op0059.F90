subroutine op0059()
!
    implicit none
!
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/op5901.h"
#include "asterfort/op5902.h"
#include "asterfort/op5903.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-michel.proix at edf.fr
!
!
! --------------------------------------------------------------------------------------------------
!
! OPERATEUR    DEFI_COMPOR
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nboccp, nboccm, nbocci
    character(len=8) :: sdcomp
    character(len=24) :: k24bid
!
! --------------------------------------------------------------------------------------------------
!
    call infmaj()

    call getres(sdcomp, k24bid, k24bid)
    call getfac('MONOCRISTAL', nboccm)
    call getfac('POLYCRISTAL', nboccp)
    call getfac('MULTIFIBRE' , nbocci)
!
    if (nboccm .gt. 0) then
!
!        MONOCRISTAL
!
        call op5901(nboccm, sdcomp)
!
    else if (nboccp.gt.0) then
!
!        POLYCRISTAL
!
        call op5902(nboccp, sdcomp)
!
    else if (nbocci.gt.0) then
!
!        MULTIFIBRE
!
        call op5903(nbocci, sdcomp)
!
    endif
!
end subroutine

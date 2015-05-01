subroutine isacti(sddisc, actioz, ievdac)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
#include "asterfort/utdidt.h"
    character(len=19) :: sddisc
    character(len=*) :: actioz
    integer :: ievdac
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE EVENEMENT
!
! DIT SI UN EVENEMENT EST TRAITE
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  ACTION : ACTION A CHERCHER
! OUT LACTI  : .TRUE. SI TRAITE
!              .FALSE. SINON
!
! ----------------------------------------------------------------------
!
    integer :: ibid, ieven, neven
    real(kind=8) :: r8bid
    character(len=16) :: action, act, k16bid
!
! ----------------------------------------------------------------------
!
    ievdac = 0
    action = actioz
    call utdidt('L', sddisc, 'LIST', ibid, 'NECHEC',&
                r8bid, neven, k16bid)
!
    do 10 ieven = 1, neven
        call utdidt('L', sddisc, 'ECHE', ieven, 'ACTION',&
                    r8bid, ibid, act)
        if (act .eq. action) then
            ievdac = ieven
        endif
10  end do
!
end subroutine

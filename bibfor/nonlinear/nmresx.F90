subroutine nmresx(sddisc, sderro, iter_newt)
!
implicit none
!
#include "asterc/r8prem.h"
#include "asterf_types.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmlere.h"
#include "asterfort/utdidt.h"
!
! ======================================================================
! COPYRIGHT (C) 2016 Stefan H. Reiterer               WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=19), intent(in) :: sddisc
    character(len=24), intent(in) :: sderro
    integer, intent(in) :: iter_newt
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Events
!
! Check if RESI_GLOB_MAXI is not too large
!
! --------------------------------------------------------------------------------------------------
!
! In  sddisc           : datastructure for discretization
! In  sderro           : datastructure for error management (events)
! In  iter_newt        : index of current Newton iteration
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: r(1), vale_resi
    aster_logical :: l_resi_maxi
    integer :: nb_fail, i_fail, i_fail_acti
    character(len=16) :: event_type
!
! --------------------------------------------------------------------------------------------------
!
    l_resi_maxi = .false. 
!
! - Index of RESI_MAXI index
!
    call utdidt('L', sddisc, 'LIST',  'NECHEC', vali_ = nb_fail)
    i_fail_acti = 0
    do i_fail = 1, nb_fail
        call utdidt('L', sddisc, 'ECHE', 'NOM_EVEN', index_ = i_fail, valk_ = event_type)
        if (event_type .eq. 'RESI_MAXI') then
            i_fail_acti = i_fail
        endif
    end do
!
! - Get RESI_GLOB_MAXI
!
    call nmlere(sddisc, 'L', 'VMAXI', iter_newt, r(1))
!
! - Evaluate event
!
    if (i_fail_acti .gt. 0) then
        call utdidt('L', sddisc, 'ECHE', 'RESI_GLOB_MAXI', index_ = i_fail_acti, valr_ = vale_resi)
        if (r(1) .gt. vale_resi) then
            l_resi_maxi = .true.
        endif
    endif
!
! - Save event
!
    call nmcrel(sderro, 'RESI_MAXI', l_resi_maxi)
!
end subroutine

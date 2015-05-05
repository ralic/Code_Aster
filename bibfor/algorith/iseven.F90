subroutine iseven(sddisc, event_name_s_, lacti)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/utdidt.h"
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
    character(len=19), intent(in) :: sddisc
    character(len=*), intent(in) :: event_name_s_
    aster_logical :: lacti
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
! In  sddisc           : datastructure for time discretization
! IN  event_name_s     : EVENEMENT A CHERCHER
! OUT LACTI  : .TRUE. SI TRAITE
!              .FALSE. SINON
!
! ----------------------------------------------------------------------
!
    integer :: i_event, nb_event
    character(len=16) :: event_name, event_name_s
!
! ----------------------------------------------------------------------
!
    lacti = .false.
    event_name_s = event_name_s_
    call utdidt('L', sddisc, 'LIST', 'NECHEC',&
                vali_ = nb_event)
!
    do i_event = 1, nb_event
        call utdidt('L', sddisc, 'ECHE', 'NOM_EVEN', index_ = i_event,&
                    valk_ = event_name)
        if (event_name .eq. event_name_s) then
            lacti = .true.
        endif
    end do
!
end subroutine

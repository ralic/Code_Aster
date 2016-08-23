subroutine dfllne(keywf, nb_fail, l_fail_error)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/getvtx.h"
#include "asterfort/utmess.h"
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
!
    character(len=16), intent(in) :: keywf
    integer, intent(out) :: nb_fail
    aster_logical, intent(out) :: l_fail_error
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_LIST_INST
!
! Get number of failure keywords
!
! --------------------------------------------------------------------------------------------------
!
! In  keywf            : factor keyword to read failures
! Out nb_fail          : number of failures defined in command file
! Out l_fail_error     : .true. if ERREUR fail is defined
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_fail_read, i_event, i_event_acti
    character(len=16) :: event_type
    integer, parameter :: nb_event = 7
    character(len=16), parameter :: list_event(nb_event) = (/'ERREUR          ',&
                                                             'DELTA_GRANDEUR  ',&
                                                             'COLLISION       ',&
                                                             'RESI_MAXI       ',&
                                                             'INTERPENETRATION',&
                                                             'DIVE_RESI       ',&
                                                             'INSTABILITE     '/)
    integer, parameter :: nb_event_maxi(nb_event) = (/1 , 99, 1 ,&
                                                      1 , 99, 1 ,&
                                                      1 /)
    integer :: nb_event_count(nb_event)
!
! --------------------------------------------------------------------------------------------------
!
    l_fail_error      = .false._1
    nb_fail           = 0
    nb_event_count(:) = 0
!
! - Number of ECHEC keywords
!
    call getfac(keywf, nb_fail)
!
! - Count number of events
!
    do i_fail_read = 1, nb_fail
        call getvtx(keywf, 'EVENEMENT', iocc=i_fail_read, scal=event_type)
        i_event_acti = 0
        do i_event = 1, nb_event            
            if (event_type .eq. list_event(i_event)) then
                i_event_acti = i_event
            endif
        end do
        ASSERT(i_event_acti .gt. 0 )
        nb_event_count(i_event_acti) = nb_event_count(i_event_acti) + 1
    end do
!
! - Check number of events
!
    do i_event = 1, nb_event
        event_type = list_event(i_event)
        if (nb_event_count(i_event) .gt. nb_event_maxi(i_event)) then
            call utmess('F', 'DISCRETISATION_10', sk = event_type)
        endif
    end do
!
! - Special for ERROR failure
!
    ASSERT(list_event(1) .eq. 'ERREUR')
    l_fail_error = nb_event_count(1) .eq. 1
!
end subroutine

subroutine nmarce(ds_inout, result   , sddisc, time, nume_store,&
                  force   , ds_print_)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmeteo.h"
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
    type(NL_DS_InOut), intent(in) :: ds_inout
    character(len=8), intent(in) :: result
    character(len=19), intent(in) :: sddisc
    real(kind=8), intent(in) :: time
    integer, intent(in) :: nume_store
    aster_logical, intent(in) :: force
    type(NL_DS_Print), optional, intent(in) :: ds_print_
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output datastructure
!
! Save fields in results datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  result           : name of datastructure for results
! In  ds_inout         : datastructure for input/output management
! In  ds_print         : datastructure for printing parameters
! In  sddisc           : datastructure for discretization
! In  time             : current time
! In  force            : .true. to store field whatever storing options
! In  nume_store       : index to store in results
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_field, i_field
!
! --------------------------------------------------------------------------------------------------
!
    nb_field = ds_inout%nb_field
!
! - Loop on fields
!
    do i_field = 1, nb_field
        if (present(ds_print_)) then
            call nmeteo(result, sddisc , ds_inout , force, nume_store, &
                        time  , i_field, ds_print_)
        else
            call nmeteo(result, sddisc , ds_inout , force, nume_store, &
                        time  , i_field)
        endif
    end do
!
end subroutine

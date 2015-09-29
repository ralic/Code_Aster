subroutine nmeteo(result, sddisc , ds_inout , force, nume_store,&
                  time  , i_field, ds_print_)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/diincl.h"
#include "asterfort/exisd.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmarcc.h"
#include "asterfort/nmetnc.h"
#include "asterfort/utmess.h"
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
    character(len=19), intent(in) :: sddisc
    character(len=8), intent(in) :: result
    integer, intent(in) :: i_field
    integer, intent(in) :: nume_store
    real(kind=8), intent(in) :: time
    aster_logical, intent(in) :: force
    type(NL_DS_Print), optional, intent(in) :: ds_print_
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output datastructure
!
! Save field in results datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  result           : name of datastructure for results
! In  ds_inout         : datastructure for input/output management
! In  nume_store       : index to store in results
! In  i_field          : field index
! In  ds_print         : datastructure for printing parameters
! In  sddisc           : datastructure for discretization
! In  time             : current time
! In  force            : .true. to store field whatever storing options
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret
    character(len=24) :: algo_name, field_algo, field_type
    aster_logical :: l_print, l_store, l_acti
!
! --------------------------------------------------------------------------------------------------
!
!
! - Field to store ?
!
    l_store = ds_inout%field(i_field)%l_store
    if (l_store) then
!
! ----- Print for this step ?
!
        l_print = .true.
        if (present(ds_print_)) then
            l_print = ds_print_%l_print
        endif
!
! ----- Is field should been active ?
!
        l_acti     = ds_inout%l_field_acti(i_field)
!
! ----- Name of field (type) in results datastructure
!
        field_type = ds_inout%field(i_field)%type
!
! ----- Name of field in algorithm
!
        algo_name  = ds_inout%field(i_field)%algo_name
        call nmetnc(algo_name, field_algo)
!
! ----- Store field
!
        if (l_acti) then
            call exisd('CHAMP', field_algo, iret)
            if (diincl(sddisc, field_type, force).and.(iret.eq.1)) then
                if (l_print) then
                    call utmess('I', 'ARCHIVAGE_6', sk=field_type, si=nume_store, sr=time)
                endif
                call nmarcc(result, nume_store, field_type, field_algo)
            endif
        endif
    endif
!
end subroutine

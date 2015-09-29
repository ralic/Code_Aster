subroutine ntetcr(nume_dof  , l_temp_nonl, ds_inout  ,&
                  list_load_, compor_    , hydr_     , hydr_init_)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/nthydr.h"
#include "asterfort/nmetcc.h"
#include "asterfort/vtcreb.h"
#include "asterfort/copisd.h"
#include "asterfort/wkvect.h"
#include "asterfort/SetIOField.h"
#include "asterfort/GetIOField.h"
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
    character(len=24), intent(in) :: nume_dof
    aster_logical, intent(in) :: l_temp_nonl
    type(NL_DS_InOut), intent(inout) :: ds_inout
    character(len=19), optional, intent(in) :: list_load_
    character(len=*), optional, intent(in) :: compor_
    character(len=*), optional, intent(in) :: hydr_
    character(len=*), optional, intent(in) :: hydr_init_
!
! --------------------------------------------------------------------------------------------------
!
! THER_* - Init
!
! Create input/output datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  nume_dof         : name of nume_dof object (numbering equation)
! In  compor           : name of <CARTE> COMPOR
! In  l_temp_nonl      : .true. if THER_NON_LINE
! In  list_load        : name of datastructure for list of loads
! In  hydr             : name of field for hydratation
! In  hydr_init        : name of field for initialhydratation
! IO  ds_inout         : datastructure for input/output management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_field, i_field
    aster_logical :: l_hydr
    character(len=24) :: temp_init
    character(len=24) :: field_type, algo_name, init_name
    character(len=19) :: compor, list_load_resu
    character(len=24) :: hydr, hydr_init
!
! --------------------------------------------------------------------------------------------------
!
    compor       = ' '
    hydr         = ' '
    hydr_init    = ' '
    if (present(compor_))    then
        compor = compor_
    endif
    if (present(hydr_))      then
        hydr = hydr_
    endif
    if (present(hydr_init_)) then
        hydr_init = hydr_init_
    endif
    nb_field       = ds_inout%nb_field
    list_load_resu = ds_inout%list_load_resu
    temp_init      = '&&NTETCR.TEMP0'
!
! - Copy of list of loads for save in results datastructure
!
    if (present(list_load_)) then
        call copisd('LISTE_CHARGES', 'G', list_load_, list_load_resu)
    endif
!
! - Active functionnalities
!
    l_hydr = .false.
    if (l_temp_nonl) then
        call nthydr(l_hydr)
    endif
!
! - Select fields depending on active functionnalities
!
    call SetIOField(ds_inout, 'TEMP', l_acti_ = .true._1)
    if (l_temp_nonl) then
        call SetIOField(ds_inout, 'COMPORTHER', l_acti_ = .true._1)
    endif
    if (l_hydr) then
        call SetIOField(ds_inout, 'HYDR_ELNO' , l_acti_ = .true._1)
    endif
!
! - Add fields
!
    do i_field = 1, nb_field
        field_type = ds_inout%field(i_field)%type
        call nmetcc(field_type     , algo_name, init_name, &
                    compor = compor ,&
                    hydr   = hydr    , temp_init = temp_init, hydr_init = hydr_init)
        if (algo_name.ne.'XXXXXXXXXXXXXXXX') then
            ds_inout%field(i_field)%algo_name = algo_name
            ds_inout%field(i_field)%init_name = init_name
        endif
    end do
!
! - Create initial state fields
!
    call vtcreb(temp_init, 'V', 'R', nume_ddlz = nume_dof)
!
end subroutine

subroutine nmetcr(ds_inout, model     , compor   , list_func_acti, sddyna   ,&
                  sdpost  , ds_contact, cara_elem, list_load)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/liscpy.h"
#include "asterfort/nmetac.h"
#include "asterfort/nmetc0.h"
#include "asterfort/nmetcc.h"
#include "asterfort/rscrsd.h"
#include "asterfort/wkvect.h"
#include "asterfort/GetIOField.h"
#include "asterfort/SetIOField.h"
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
    type(NL_DS_InOut), intent(inout) :: ds_inout
    character(len=24), intent(in) :: model
    integer, intent(in) :: list_func_acti(*)
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: compor
    character(len=19), intent(in) :: sddyna
    character(len=19), intent(in) :: sdpost
    character(len=24), intent(in) :: cara_elem
    character(len=19), intent(in) :: list_load
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output management
!
! Initializations for input/output management
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_inout         : datastructure for input/output management
! In  model            : name of model
! In  cara_elem        : name of datastructure for elementary parameters (CARTE)
! In  compor           : name of <CARTE> COMPOR
! In  ds_contact       : datastructure for contact management
! In  list_func_acti   : list of active functionnalities
! In  sddyna           : name of dynamic parameters datastructure
! In  sdpost           : name of post-treatment for stability analysis parameters datastructure
! In  list_load        : name of datastructure for list of loads
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_field, nb_field_resu
    integer :: i_field, i_field_resu
    integer, pointer :: xfem_cont(:) => null()
    aster_logical :: l_find, l_xfem_cohe
    character(len=19) :: result, list_load_resu
    character(len=24) :: field_resu, field_type, algo_name, init_name
!
! --------------------------------------------------------------------------------------------------
!
    result         = '&&NMETCR'
    nb_field       = ds_inout%nb_field
    list_load_resu = ds_inout%list_load_resu
!
! - Special copy of list of loads for save in results datastructure
!
    call liscpy(list_load, list_load_resu, 'G')
!
! - Select fields depending on active functionnalities
!
    call nmetac(list_func_acti, sddyna, ds_contact, ds_inout)
!
! - Set localization for cohesive XFEM fields
!
    call GetIOField(ds_inout, 'COHE_ELEM', l_acti_ = l_xfem_cohe)
    if (l_xfem_cohe) then
        call jeveuo(model(1:8)//'.XFEM_CONT', 'L', vi=xfem_cont)
        if (xfem_cont(1).eq.2) then
            call SetIOField(ds_inout, 'COHE_ELEM', disc_type_ = 'ELNO')
        endif
        if (xfem_cont(1).eq.1.or.xfem_cont(1).eq.3) then
            call SetIOField(ds_inout, 'COHE_ELEM', disc_type_ = 'ELEM')
        endif
    endif
!
! - Add fields
!
    do i_field = 1, nb_field
        field_type = ds_inout%field(i_field)%type
        call nmetcc(field_type, algo_name, init_name, &
                    compor    , sddyna   , sdpost   , ds_contact)
        if (algo_name.ne.'XXXXXXXXXXXXXXXX') then
            ds_inout%field(i_field)%algo_name = algo_name
        endif
        if (init_name.ne.'XXXXXXXXXXXXXXXX') then
            ds_inout%field(i_field)%init_name = init_name
        endif
    end do
!
! - Create initial state fields
!
    call nmetc0(model, cara_elem, compor, ds_inout)
!
! - Check: fields have been defined in rscrsd.F90 ?
!
    call rscrsd('V', result, 'EVOL_NOLI', 1) 
    call jelira(result(1:8)//'           .DESC', 'NOMMAX', nb_field_resu)
    do i_field = 1, nb_field
        field_type = ds_inout%field(i_field)%type
        init_name  = ds_inout%field(i_field)%init_name
        if (ds_inout%field(i_field)%l_store) then
            l_find = .false._1
            do i_field_resu = 1, nb_field_resu
                call jenuno(jexnum(result(1:8)//'           .DESC', i_field_resu), field_resu)
                if (field_resu .eq. field_type) then
                    l_find = .true._1
                endif
            end do
! --------- No field in results => change rscrsd subroutine !
            ASSERT(l_find)
        endif
    end do
    call detrsd('RESULTAT', result)
!
end subroutine

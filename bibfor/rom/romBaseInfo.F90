subroutine romBaseInfo(ds_empi)
!
use Rom_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    type(ROM_DS_Empi), intent(in) :: ds_empi
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Informations about empiric modes base
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_empi          : datastructure for empiric modes
!
! --------------------------------------------------------------------------------------------------
!
    call utmess('I', 'ROM3_1', sk = ds_empi%base)
    call utmess('I', 'ROM3_2', sk = ds_empi%model)
    call utmess('I', 'ROM3_3', sk = ds_empi%mesh)
    call utmess('I', 'ROM3_4', sk = ds_empi%field_type)
    if (ds_empi%base_type .eq. 'LINEIC') then
        call utmess('I', 'ROM3_10')
        call utmess('I', 'ROM3_11', sk = ds_empi%axe_line)
        call utmess('I', 'ROM3_12', sk = ds_empi%surf_num)
    else
        call utmess('I', 'ROM3_20')
    endif
    call utmess('I', 'ROM3_5', si = ds_empi%nb_mode)
    call utmess('I', 'ROM3_6', si = ds_empi%nb_node)
    call utmess('I', 'ROM3_7', si = ds_empi%nb_equa)
    call utmess('I', 'ROM3_8', si = ds_empi%nb_cmp)
!
end subroutine

subroutine romBaseInit(ds_lineicnumb, ds_empi)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/infniv.h"
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
    type(ROM_DS_LineicNumb), intent(in) :: ds_lineicnumb
    type(ROM_DS_Empi), intent(out) :: ds_empi
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction - Initializations
!
! Initialisation of datastructure for empiric modes
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_lineicnumb    : datastructure for lineic base numbering
! Out ds_empi          : datastructure for empiric modes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_3')
    endif
!
! - Create parameters datastructure
!
    ds_empi%base         = ' '
    ds_empi%field_type   = ' '
    ds_empi%field_refe   = ' '
    ds_empi%mesh         = ' '
    ds_empi%model        = ' '
    ds_empi%base_type    = ' '
    ds_empi%axe_line     = ' '
    ds_empi%surf_num     = ' '
    ds_empi%nb_node      = 0
    ds_empi%nb_mode      = 0
    ds_empi%nb_equa      = 0
    ds_empi%nb_cmp       = 0
    ds_empi%ds_lineic    = ds_lineicnumb
!
end subroutine

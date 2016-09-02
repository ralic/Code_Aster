subroutine romBaseCopy(ds_empi_in, base, ds_empi_out)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "asterfort/romBaseInit.h"
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
    type(ROM_DS_Empi), intent(in)  :: ds_empi_in
    character(len=8), intent(in)   :: base
    type(ROM_DS_Empi), intent(out) :: ds_empi_out
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Copy empiric modes base
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_empi_in       : datastructure for empiric modes
! In  base             : name of output empiric base
! Out ds_empi_out      : datastructure for output empiric modes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=8) :: valk(2)
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        valk(1) = ds_empi_in%base
        valk(2) = base
        call utmess('I', 'ROM2_1', nk = 2, valk = valk)
    endif
!
! - Initialisation of datastructure
!
    call romBaseInit(ds_empi_in%ds_lineic, ds_empi_out) 
!
! - Copy informations
!
    ds_empi_out%base       = ds_empi_in%base
    ds_empi_out%field_type = ds_empi_in%field_type
    ds_empi_out%field_refe = ds_empi_in%field_refe
    ds_empi_out%mesh       = ds_empi_in%mesh
    ds_empi_out%model      = ds_empi_in%model
    ds_empi_out%base_type  = ds_empi_in%base_type
    ds_empi_out%axe_line   = ds_empi_in%axe_line
    ds_empi_out%surf_num   = ds_empi_in%surf_num
    ds_empi_out%nb_equa    = ds_empi_in%nb_equa
    ds_empi_out%nb_node    = ds_empi_in%nb_node
    ds_empi_out%nb_cmp     = ds_empi_in%nb_cmp
    ds_empi_out%nb_mode    = ds_empi_in%nb_mode
!
! - Change base
!
    ds_empi_out%base       = base
!
end subroutine

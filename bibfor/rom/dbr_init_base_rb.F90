subroutine dbr_init_base_rb(base, ds_para_rb, ds_empi)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "asterfort/dismoi.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8), intent(in) :: base
    type(ROM_DS_ParaDBR_RB), intent(in) :: ds_para_rb
    type(ROM_DS_Empi), intent(inout) :: ds_empi
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Initializations
!
! Prepare datastructure for empiric modes - For RB methods
!
! --------------------------------------------------------------------------------------------------
!
! In  base             : name of empiric base
! In  ds_para_rb       : datastructure for parameters (RB)
! IO  ds_empi          : datastructure for empiric modes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nb_equa = 0, nb_node = 0
    character(len=8)  :: model = ' ', mesh = ' ', matr_name = ' '
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_18')
    endif
!
! - Get "representative" matrix
!
    matr_name = ds_para_rb%ds_multipara%matr_name(1)  
!
! - Get information about model
!
    call dismoi('NOM_MODELE', matr_name, 'MATR_ASSE', repk = model)
!
! - Get informations about fields
!
    call dismoi('NB_EQUA'     , matr_name, 'MATR_ASSE', repi = nb_equa) 
    call dismoi('NOM_MAILLA'  , model    , 'MODELE'   , repk = mesh)
    call dismoi('NB_NO_MAILLA', mesh     , 'MAILLAGE' , repi = nb_node)
!
! - Save in empiric base
!
    ds_empi%base         = base
    ds_empi%field_type   = 'DEPL'
    ds_empi%mesh         = mesh
    ds_empi%model        = model
    ds_empi%nb_equa      = nb_equa
    ds_empi%nb_node      = nb_node
    ds_empi%nb_cmp       = nb_equa/nb_node
    ds_empi%nb_mode      = 0
!
end subroutine

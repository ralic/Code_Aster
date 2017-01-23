subroutine dbr_init_base_pod(ds_para, ds_empi)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
#include "asterfort/dismoi.h"
#include "asterfort/rs_getfirst.h"
#include "asterfort/rsexch.h"
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
    type(ROM_DS_ParaDBR), intent(in) :: ds_para
    type(ROM_DS_Empi), intent(inout) :: ds_empi
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_BASE_REDUITE - Initializations
!
! Prepare datastructure for empiric modes - For POD methods
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_para          : datastructure for parameters (POD)
! IO  ds_empi          : datastructure for empiric modes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: iret, nume_first
    integer :: nb_equa = 0, nb_node = 0
    character(len=8)  :: model = ' ', mesh = ' '
    character(len=8)  :: result_in = ' ', result_out = ' '
    character(len=24) :: field_refe = '&&ROM_COMP.FIELD', field_name = ' '
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_12')
    endif
!
! - Get informations from parameters
!
    result_out   = ds_para%result_out
    result_in    = ds_para%result_in
    field_name   = ds_para%field_name
!
! - Get information about model
!
    call dismoi('NOM_MODELE', result_in, 'RESULTAT', repk = model)
!
! - Get informations about fields
!
    call rs_getfirst(result_in, nume_first)
    call rsexch(' ', result_in, field_name, nume_first, field_refe, iret)
    if (iret .ne. 0) then
        call utmess('F', 'ROM5_11', sk = field_name)
    endif
    call dismoi('NB_EQUA'     , field_refe, 'CHAM_NO' , repi = nb_equa) 
    call dismoi('NOM_MAILLA'  , field_refe, 'CHAM_NO' , repk = mesh)
    call dismoi('NB_NO_MAILLA', mesh      , 'MAILLAGE', repi = nb_node)
!
! - Save in empiric base
!
    ds_empi%base         = result_out
    ds_empi%field_type   = field_name
    ds_empi%field_refe   = field_refe
    ds_empi%mesh         = mesh
    ds_empi%model        = model
    ds_empi%nb_node      = nb_node
    ds_empi%nb_mode      = 0
    ds_empi%nb_equa      = nb_equa
    ds_empi%nb_cmp       = nb_equa/nb_node
!
end subroutine

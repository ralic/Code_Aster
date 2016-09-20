subroutine nmdocc(model, chmate, l_etat_init, compor)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/comp_init.h"
#include "asterfort/comp_meca_info.h"
#include "asterfort/comp_meca_chck.h"
#include "asterfort/comp_meca_cvar.h"
#include "asterfort/comp_meca_elas.h"
#include "asterfort/comp_meca_full.h"
#include "asterfort/comp_meca_pvar.h"
#include "asterfort/comp_meca_read.h"
#include "asterfort/comp_meca_save.h"
#include "asterfort/dismoi.h"
#include "asterfort/imvari.h"
#include "asterfort/nocart.h"
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
! aslint: disable=W1003
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: model
    character(len=8), intent(in) :: chmate
    aster_logical, intent(in) :: l_etat_init
    character(len=19), intent(out) :: compor
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Get parameters from COMPORTEMENT keyword and prepare COMPOR <CARTE>
!
! --------------------------------------------------------------------------------------------------
!
! In  model       : name of model
! In  chmate      : name of material field
! In  l_etat_init : .true. if initial state is defined
! Out compor      : name of <CARTE> COMPOR
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_auto_elas, l_auto_deborst, l_comp_erre
    integer :: nb_cmp
    character(len=8) :: mesh
    character(len=19) :: comp_elas, full_elem_s
    character(len=19) :: compor_info
    type(NL_DS_ComporPrep) :: ds_compor_prep
!
! --------------------------------------------------------------------------------------------------
!
    compor      = '&&NMDOCC.COMPOR'
    comp_elas   = '&&NMDOCC.COMP_ELAS'
    full_elem_s = '&&NMDOCC.FULL_ELEM'
    compor_info = '&&NMDOCC.INFO'
    call dismoi('NOM_MAILLA', model, 'MODELE', repk=mesh)
!
! - Create datastructure to prepare comportement
!
    call comp_meca_info(ds_compor_prep)
    if (ds_compor_prep%nb_comp .eq. 0) then
        call utmess('I', 'COMPOR4_64')
    endif
    if (ds_compor_prep%nb_comp .ge. 99999) then
        call utmess('A', 'COMPOR4_65')
    endif
!
! - Create COMPOR <CARTE>
!
    call comp_init(mesh, compor, 'V', nb_cmp)
!
! - Set ELASTIQUE COMPOR
!
    call comp_meca_elas(compor, nb_cmp, l_etat_init)
!
! - Default ELASTIQUE COMPOR <CARTE> on all mesh
!
    call nocart(compor, 1, nb_cmp)
!
! - Read informations from command file
!
    call comp_meca_read(l_etat_init, ds_compor_prep, model)
!
! - Create <CARTE> of FULL_MECA option for checking
!
    call comp_meca_full(model, compor, full_elem_s)
!
! - Check informations in COMPOR <CARTE>
!
    call comp_meca_chck(model         , mesh          , full_elem_s, l_etat_init,&
                        ds_compor_prep,&
                        l_auto_elas   , l_auto_deborst, l_comp_erre)
    if (l_auto_deborst) then
        call utmess('I', 'COMPOR5_20')
    endif
    if (l_auto_elas) then
        call utmess('I', 'COMPOR5_21')
    endif
    if (l_comp_erre) then
        call utmess('I', 'COMPOR5_22')
    endif
!
! - Count internal variables
!
    call comp_meca_cvar(ds_compor_prep)
!
! - Save informations in COMPOR <CARTE>
!
    call comp_meca_save(model         , mesh, chmate, compor, nb_cmp,&
                        ds_compor_prep)
!
! - Prepare informations about internal variables
!
    call comp_meca_pvar(model_ = model, compor_cart_ = compor, compor_info = compor_info)
!
! - Print informations about internal variables
!
    call imvari(compor_info)
!
! - Cleaning
!
    deallocate(ds_compor_prep%v_comp)
    deallocate(ds_compor_prep%v_exte)
!
end subroutine

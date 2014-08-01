subroutine nmdocc(model, chmate, l_etat_init, compor)
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
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/nocart.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8), intent(in) :: model
    character(len=8), intent(in) :: chmate
    aster_logical, intent(in) :: l_etat_init
    character(len=19), intent(out) :: compor
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE> - MECHANICS
!
! SAISIE ET STOCKAGE DES PARAMETRES LOCAUX DE COMPORTEMENT
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
    integer :: nb_cmp, nbocc_compor
    character(len=8) :: mesh
    character(len=19) :: comp_elas, full_elem_s
    character(len=19) :: list_vari_name
    character(len=16), pointer :: p_info_comp_valk(:) => null()
    integer, pointer :: p_info_comp_vali(:) => null()
    integer, pointer :: p_info_comp_nvar(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    compor = '&&NMDOCC.COMPOR'
    comp_elas = '&&NMDOCC.COMP_ELAS'
    full_elem_s = '&&NMDOCC.FULL_ELEM'
    list_vari_name = '&&NMDOCC.LIST_VARI'
    call dismoi('NOM_MAILLA', model, 'MODELE', repk=mesh)
!
! - Create comportment informations objects
!
    call comp_meca_info(p_info_comp_valk, p_info_comp_vali, p_info_comp_nvar, nbocc_compor)
    if (nbocc_compor .eq. 0) then
        call utmess('I', 'COMPOR4_64')
    endif
    if (nbocc_compor .ge. 99999) then
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
    call comp_meca_read(l_etat_init, p_info_comp_valk, p_info_comp_vali)
!
! - Create <CARTE> of FULL_MECA option for checking
!
    call comp_meca_full(model, compor, full_elem_s)
!
! - Check informations in COMPOR <CARTE>
!
    call comp_meca_chck(model         , mesh       , full_elem_s, p_info_comp_valk, l_auto_elas,&
                        l_auto_deborst, l_comp_erre)
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
    call comp_meca_cvar(p_info_comp_valk, p_info_comp_vali, p_info_comp_nvar)
!
! - Save informations in COMPOR <CARTE>
!
    call comp_meca_save(mesh, chmate, compor, nb_cmp, p_info_comp_valk,&
                        p_info_comp_vali, p_info_comp_nvar)
!
! - Prepare informations about internal variables
!
    call comp_meca_pvar(list_vari_name, compor_cart = compor)
!
! - Print informations about internal variables
!
    call imvari(list_vari_name, compor_cart = compor)
!
    AS_DEALLOCATE(vk16 = p_info_comp_valk)
    AS_DEALLOCATE(vi   = p_info_comp_vali)
    AS_DEALLOCATE(vi   = p_info_comp_nvar)
!
end subroutine

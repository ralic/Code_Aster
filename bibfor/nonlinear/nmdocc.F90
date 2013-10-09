subroutine nmdocc(model, chmate, l_etat_init, compor)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/comp_init.h"
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
    logical, intent(in) :: l_etat_init
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
    integer :: nb_cmp, nbocc
    integer :: iret, ibid
    character(len=8) :: mesh
    character(len=19) :: list_vale, comp_elas, full_elem_s
    character(len=19) :: list_vari_name
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    compor         = '&&NMDOCC.COMPOR'
    list_vale      = '&&NMDOCC.LIST_VALE'
    comp_elas      = '&&NMDOCC.COMP_ELAS'
    full_elem_s    = '&&NMDOCC.FULL_ELEM'
    list_vari_name = '&&NMDOCC.LIST_VARI'
    call dismoi('NOM_MAILLA', model, 'MODELE', repk=mesh)
!
! - Create COMPOR <CARTE>
!
    call comp_init(mesh, compor, 'V', nb_cmp)
!
! - Set ELASTIQUE COMPOR
!
    call comp_meca_elas(compor, nb_cmp)
!
! - Read informations from command file
!
    call comp_meca_read(list_vale, l_etat_init, nbocc)
    if (nbocc.eq.0) goto 99
!
! - Create <CARTE> of FULL_MECA option for checking
!
    call comp_meca_full(model, compor, full_elem_s)
!
! - Check informations in COMPOR <CARTE>
!
    call comp_meca_chck(model, mesh, full_elem_s, list_vale)
!
! - Count internal variables
!
    call comp_meca_cvar(list_vale)
!
! - Save informations in COMPOR <CARTE>
!
    call comp_meca_save(mesh, chmate, compor, nb_cmp, list_vale)
!
! - Prepare informations about internal variables
!
    call comp_meca_pvar(list_vari_name, compor_cart = compor)
!
! - Print informations about internal variables
!
    call imvari(list_vari_name, compor_cart = compor)
!
    call jedetr(list_vale(1:19)//'.VALK')
    call jedetr(list_vale(1:19)//'.VALI')
    call jedetr(list_vale(1:19)//'.NVAR')
!
 99 continue
!
    call jedema()
end subroutine

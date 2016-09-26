subroutine nmdocm(model, mult_comp)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/alcart.h"
#include "asterfort/comp_meca_l.h"
#include "asterfort/comp_read_mesh.h"
#include "asterfort/dismoi.h"
#include "asterc/getfac.h"
#include "asterfort/getvtx.h"
#include "asterfort/getvid.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
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
    character(len=*), intent(in) :: model
    character(len=*), intent(in) :: mult_comp
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Get parameters from COMPORTEMENT keyword and prepare MULT_COMP <CARTE> (for crystals)
!
! --------------------------------------------------------------------------------------------------
!
! In  model       : name of model
! In  mult_comp   : name of <CARTE> MULT_COMP
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: list_elem_affe
    aster_logical :: l_affe_all
    integer :: nb_elem_affe
    integer, pointer :: v_elem_affe(:) => null()
    aster_logical :: l_cristal
    integer :: nb_cmp, nb_cmp_max, icmp, i_comp, nbocc_compor
    character(len=8) :: mesh
    character(len=16) :: rela_comp, comp_cris, keywordfact
    integer :: nume_gd
    character(len=8) :: name_gd
    character(len=16), pointer :: p_mcomp_valv(:) => null()
    character(len=8) , pointer :: p_cata_nomcmp(:) => null()
    character(len=8) , pointer :: p_mcomp_ncmp(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_cmp         = 0
    name_gd        = 'MULTCOMP'
    nbocc_compor   = 0
    keywordfact    = 'COMPORTEMENT'
    list_elem_affe = '&&COMPMECASAVE.LIST'
    call dismoi('NOM_MAILLA', model, 'MODELE', repk=mesh)
    call getfac(keywordfact, nbocc_compor)
!
! - Read catalog
!
    call jenonu(jexnom('&CATA.GD.NOMGD', name_gd), nume_gd)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', nume_gd), 'L', vk8 = p_cata_nomcmp)
    call jelira(jexnum('&CATA.GD.NOMCMP', nume_gd), 'LONMAX', nb_cmp_max)
!
! - Allocate <CARTE>
!
    call alcart('V', mult_comp, mesh, name_gd)
!
! - Acces to <CARTE>
!
    call jeveuo(mult_comp(1:19)//'.NCMP', 'E', vk8  = p_mcomp_ncmp)
    call jeveuo(mult_comp(1:19)//'.VALV', 'E', vk16 = p_mcomp_valv)
!
! - Init <CARTE>
!
    do icmp = 1, nb_cmp_max
        p_mcomp_ncmp(icmp) = p_cata_nomcmp(icmp)
        p_mcomp_valv(icmp) = ' '
    enddo
!
    nb_cmp = nb_cmp_max
!
! - Default ELASTIQUE COMPOR <CARTE> on all mesh
!
    call nocart(mult_comp, 1, nb_cmp)
!
! - Read informations from command file
!
    do i_comp = 1, nbocc_compor
!
        rela_comp = 'VIDE'
        comp_cris = ' '
!
! ----- Get RELATION from command file
!
        call getvtx(keywordfact, 'RELATION', iocc = i_comp, scal = rela_comp)
!
! ----- Detection of specific cases
!
        call comp_meca_l(rela_comp, 'CRISTAL', l_cristal)
!
! ----- Get multi-comportment *CRISTAL
!
        if (l_cristal) then
            call getvid(keywordfact, 'COMPOR', iocc = i_comp, scal = comp_cris)
        endif
!
! ----- Get elements
!
        call comp_read_mesh(mesh          , keywordfact, i_comp        ,&
                            list_elem_affe, l_affe_all , nb_elem_affe)
!
! ----- Set in <CARTE>
!
        p_mcomp_valv(1) = comp_cris
!
! ----- Affect in <CARTE>
!
        if (l_affe_all) then
            call nocart(mult_comp, 1, nb_cmp)
        else
            call jeveuo(list_elem_affe, 'L', vi = v_elem_affe)
            call nocart(mult_comp, 3, nb_cmp, mode = 'NUM', nma = nb_elem_affe,&
                        limanu = v_elem_affe)
            call jedetr(list_elem_affe)
        endif
    end do
!
! - Cleaning
!
    call jedetr(mult_comp(1:19)//'.NCMP')
    call jedetr(mult_comp(1:19)//'.VALV')
!
end subroutine

subroutine sele_elem_comp(modelz, compor, defo_comp, list_elem_comp)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/nbelem.h"
#include "asterfort/nbgrel.h"
#include "asterfort/typele.h"
#include "asterfort/as_allocate.h"
#include "asterfort/jenuno.h"
#include "asterfort/exisdg.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/dismoi.h"
#include "asterfort/etenca.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    character(len=*), intent(in) :: modelz
    character(len=24), intent(in) :: compor
    character(len=16), intent(in) :: defo_comp
    integer, pointer, intent(out) :: list_elem_comp(:)
!
! --------------------------------------------------------------------------------------------------
!
! Select elements by comportment
!
! --------------------------------------------------------------------------------------------------
!
! In  modelz          : name of model
! In  compor          : name of comportment CARTE
! In  defo_comp       : type of deformation to find
! In  list_elem_comp  : elements preselected on complete mesh
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: mesh
    character(len=16) :: grandeur_name, defo_comp_f
    character(len=19) :: ligrmo
    character(len=24) :: name_liel
    integer :: nume_elem
    integer :: idx_gd, idx_cmp, nb_gd_max, iret, nb_cmp_max, nb_elem_mesh
    integer :: nb_elem_grel, nb_grel, nb_ec
    integer :: i_elem_grel, i_grel
    integer, pointer :: comp_ptma(:) => null()
    integer, pointer :: comp_desc(:) => null()
    character(len=16), pointer :: comp_vale(:) => null()
    integer, pointer :: list_elem_grel(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    ligrmo = modelz(1:8)//'.MODELE'
    call dismoi('NOM_MAILLA', modelz, 'MODELE', repk=mesh)
    call dismoi('NB_MA_MAILLA', mesh, 'MAILLAGE', repi=nb_elem_mesh)
!
! - Allocate list of elements
!
    AS_ALLOCATE(vi=list_elem_comp, size = nb_elem_mesh)
!
! - Preparation of comportment datas
!
    grandeur_name = 'COMPOR'
    call dismoi('NB_EC', grandeur_name, 'GRANDEUR', repi=nb_ec)
    ASSERT(nb_ec.le.1)
    call etenca(compor, ligrmo, iret)
    ASSERT(iret.eq.0)
    call jeveuo(compor(1:19)//'.DESC', 'L', vi = comp_desc)
    nb_gd_max = comp_desc(2)
    call jelira(jexnom('&CATA.GD.NOMCMP', grandeur_name), 'LONMAX', nb_cmp_max)
    call jeveuo(compor(1:19)//'.VALE', 'L', vk16 = comp_vale)
    call jeveuo(compor(1:19)//'.PTMA', 'L', vi   = comp_ptma)
!
! - Select elements in list
!
    nb_grel   = nbgrel(ligrmo)
    name_liel = ligrmo//'.LIEL'
    do i_grel = 1, nb_grel
        nb_elem_grel = nbelem(ligrmo,i_grel)
        call jeveuo(jexnum(name_liel, i_grel), 'L', vi = list_elem_grel)
        do i_elem_grel = 1, nb_elem_grel
            nume_elem = list_elem_grel(i_elem_grel)
            idx_gd    = comp_ptma(nume_elem)
            if (idx_gd.ne. 0) then
                idx_cmp     = comp_desc(3+2*nb_gd_max+idx_gd)
                ASSERT(exisdg([idx_cmp], 1))
                if (exisdg([idx_cmp], 1)) then
                    defo_comp_f = comp_vale((idx_gd-1)*nb_cmp_max+3)
                    if (defo_comp .eq. defo_comp_f) then
                        list_elem_comp(nume_elem) = 1
                    endif
                endif
            endif
        end do
    end do
!
end subroutine
